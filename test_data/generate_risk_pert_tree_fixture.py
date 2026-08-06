#!/usr/bin/env python3
"""
Generate a flattened tree CSV fixture for beta-PERT Monte Carlo risk/cost/benefit aggregation.

Default practical output:
    risk_tree_fixture.csv

Optional detail outputs:
    emitted only when --include-detail-outputs is supplied.

Core model mirrored by this generator:
- Leaf nodes contain Optimistic, Likely, Pessimistic estimates.
- Risk leaves also contain Probability.
- Cost and Benefit leaves are always-occurring uncertain estimates.
- Risk leaves are occurrence-gated:
      contribution = Bernoulli(probability) * beta_pert_impact_sample
- Cost and Benefit leaves contribute sampled impact directly.
- Parent nodes aggregate all descendant leaves, not only immediate children.
- Parent outputs are literal statistical P10/P50/P90 of the empirical curve.
- Expected values and tolerance bands are estimated from repeated reference runs.

Dependencies:
- Python 3.9+
- numpy
- pandas

The script can bootstrap numpy and pandas at runtime if they are missing.
For formal audit generation, prefer a controlled virtual environment with pinned versions.
"""

from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import subprocess
import sys
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple


REQUIRED_PACKAGES = {
    "numpy": "numpy",
    "pandas": "pandas",
}


def ensure_dependencies() -> None:
    missing = [
        package_name
        for import_name, package_name in REQUIRED_PACKAGES.items()
        if importlib.util.find_spec(import_name) is None
    ]
    if not missing:
        return
    print(f"Installing missing Python packages: {', '.join(missing)}")
    subprocess.check_call([sys.executable, "-m", "pip", "install", *missing])


ensure_dependencies()

import numpy as np
import pandas as pd


MASTER_SEED = 20260805
DEFAULT_LAMBDA_VALUE = 4.0
SAMPLES_PER_LEAF = 10_000
QUICK_REFERENCE_RUNS = 100
AUDIT_REFERENCE_RUNS = 10_000
DEFAULT_MAX_TREE_DEPTH = 4
TOLERANCE_LOWER_PERCENTILE = 0.5
TOLERANCE_UPPER_PERCENTILE = 99.5
VALID_ESTIMATE_TYPES = {"Cost", "Benefit", "Risk"}


@dataclass
class TreeNode:
    node_id: str
    name: str
    node_type: str  # parent or leaf
    estimate_type: Optional[str] = None  # Cost, Benefit, Risk, or Mixed for parents
    optimistic: Optional[float] = None
    likely: Optional[float] = None
    pessimistic: Optional[float] = None
    probability: Optional[float] = None
    children: List["TreeNode"] = field(default_factory=list)


@dataclass(frozen=True)
class InvalidLeafCase:
    invalid_case_id: str
    case_name: str
    estimate_type: str
    optimistic: Any
    likely: Any
    pessimistic: Any
    probability: Any
    expected_error_code: str
    expected_error_reason: str


def stable_seed(*parts: Any) -> int:
    text = "|".join(str(part) for part in parts)
    digest = hashlib.sha256(text.encode("utf-8")).digest()
    return int.from_bytes(digest[:8], byteorder="big", signed=False) % (2**32 - 1)


def blank_if_none(value: Any) -> Any:
    return "" if value is None else value


def is_blank(value: Any) -> bool:
    return value is None or value == ""


def validate_leaf(node: TreeNode) -> Tuple[bool, str, str]:
    if node.node_type != "leaf":
        return False, "NOT_LEAF", "node is not a leaf"
    if node.estimate_type not in VALID_ESTIMATE_TYPES:
        return False, "INVALID_ESTIMATE_TYPE", "estimate_type must be Cost, Benefit, or Risk"

    values = {
        "optimistic": node.optimistic,
        "likely": node.likely,
        "pessimistic": node.pessimistic,
    }
    for label, value in values.items():
        if is_blank(value):
            return False, f"MISSING_{label.upper()}", f"{label} value is missing"
        try:
            float(value)
        except (TypeError, ValueError):
            return False, f"NON_NUMERIC_{label.upper()}", f"{label} value is not numeric"

    optimistic = float(node.optimistic)
    likely = float(node.likely)
    pessimistic = float(node.pessimistic)

    if pessimistic < optimistic:
        return False, "PESSIMISTIC_LESS_THAN_OPTIMISTIC", "pessimistic must be greater than optimistic"
    if pessimistic == optimistic:
        return False, "OPTIMISTIC_EQUALS_PESSIMISTIC", "pessimistic must be greater than optimistic"
    if likely < optimistic:
        return False, "LIKELY_LESS_THAN_OPTIMISTIC", "likely must be greater than optimistic"
    if likely > pessimistic:
        return False, "LIKELY_GREATER_THAN_PESSIMISTIC", "likely must be less than pessimistic"
    if likely == optimistic:
        return False, "LIKELY_EQUALS_OPTIMISTIC", "likely must be strictly greater than optimistic"
    if likely == pessimistic:
        return False, "LIKELY_EQUALS_PESSIMISTIC", "likely must be strictly less than pessimistic"

    if node.estimate_type == "Risk":
        if is_blank(node.probability):
            return False, "MISSING_PROBABILITY", "Risk probability is required"
        try:
            probability = float(node.probability)
        except (TypeError, ValueError):
            return False, "NON_NUMERIC_PROBABILITY", "Risk probability must be numeric"
        if probability < 0:
            return False, "PROBABILITY_LESS_THAN_ZERO", "Risk probability must be at least 0"
        if probability > 1:
            return False, "PROBABILITY_GREATER_THAN_ONE", "Risk probability must be at most 1"
    else:
        if not is_blank(node.probability):
            return False, "PROBABILITY_NOT_ALLOWED", "Probability must be blank for Cost and Benefit leaves"

    return True, "VALID", "valid leaf"


def pert_alpha_beta(optimistic: float, likely: float, pessimistic: float, lambda_value: float) -> Tuple[float, float]:
    span = pessimistic - optimistic
    alpha = 1.0 + lambda_value * (likely - optimistic) / span
    beta = 1.0 + lambda_value * (pessimistic - likely) / span
    return alpha, beta


def beta_pert_impact_samples(node: TreeNode, lambda_value: float, seed: int, samples_per_leaf: int) -> np.ndarray:
    valid, code, reason = validate_leaf(node)
    if not valid:
        raise ValueError(f"Invalid leaf {node.node_id}: {code}: {reason}")
    optimistic = float(node.optimistic)
    likely = float(node.likely)
    pessimistic = float(node.pessimistic)
    alpha, beta = pert_alpha_beta(optimistic, likely, pessimistic, lambda_value)
    rng = np.random.RandomState(seed)  # MT19937 Mersenne Twister.
    unit_samples = rng.beta(alpha, beta, size=samples_per_leaf)
    return optimistic + unit_samples * (pessimistic - optimistic)


def leaf_contribution_samples(
    leaf: TreeNode,
    parent_id: str,
    lambda_value: float,
    master_seed: int,
    run_index: int,
    samples_per_leaf: int,
) -> np.ndarray:
    impact_seed = stable_seed(master_seed, parent_id, leaf.node_id, run_index, "impact")
    impact = beta_pert_impact_samples(leaf, lambda_value, impact_seed, samples_per_leaf)

    if leaf.estimate_type != "Risk":
        return impact

    probability = float(leaf.probability)
    occurrence_seed = stable_seed(master_seed, parent_id, leaf.node_id, run_index, "occurrence")
    rng = np.random.RandomState(occurrence_seed)
    occurrence = rng.binomial(1, probability, size=samples_per_leaf)
    return occurrence * impact


def percentile(values: np.ndarray, q: float) -> float:
    try:
        return float(np.percentile(values, q, method="linear"))
    except TypeError:
        return float(np.percentile(values, q, interpolation="linear"))


def descendants(node: TreeNode) -> List[TreeNode]:
    result: List[TreeNode] = []
    for child in node.children:
        result.append(child)
        result.extend(descendants(child))
    return result


def descendant_leaves(node: TreeNode) -> List[TreeNode]:
    return [item for item in descendants(node) if item.node_type == "leaf"]


def all_nodes_preorder(root: TreeNode) -> List[TreeNode]:
    nodes = [root]
    for child in root.children:
        nodes.extend(all_nodes_preorder(child))
    return nodes


def parent_nodes(roots: Sequence[TreeNode]) -> List[TreeNode]:
    result: List[TreeNode] = []
    for root in roots:
        for node in all_nodes_preorder(root):
            if node.node_type == "parent":
                result.append(node)
    return result


def infer_parent_estimate_type(node: TreeNode) -> str:
    leaf_types = sorted({leaf.estimate_type for leaf in descendant_leaves(node)})
    return leaf_types[0] if len(leaf_types) == 1 else "Mixed"


def assign_parent_estimate_types(node: TreeNode) -> None:
    for child in node.children:
        assign_parent_estimate_types(child)
    if node.node_type == "parent":
        node.estimate_type = infer_parent_estimate_type(node)


def node_depth(root: TreeNode, target_id: str, depth: int = 1) -> Optional[int]:
    if root.node_id == target_id:
        return depth
    for child in root.children:
        found = node_depth(child, target_id, depth + 1)
        if found is not None:
            return found
    return None


def validate_tree_depths(roots: Sequence[TreeNode], max_tree_depth: int) -> None:
    for root in roots:
        for node in all_nodes_preorder(root):
            depth = node_depth(root, node.node_id)
            if depth is None:
                raise RuntimeError(f"Could not determine depth for {node.node_id}")
            if depth > max_tree_depth:
                raise ValueError(
                    f"Node {node.node_id} depth {depth} exceeds max_tree_depth {max_tree_depth}. "
                    "Increase --max-tree-depth or simplify the fixture tree."
                )


def simulate_parent_once(
    parent: TreeNode,
    lambda_value: float,
    samples_per_leaf: int,
    run_index: int,
    master_seed: int,
) -> Dict[str, float]:
    leaves = descendant_leaves(parent)
    parent_samples = np.zeros(samples_per_leaf, dtype=float)
    for leaf in leaves:
        parent_samples += leaf_contribution_samples(
            leaf=leaf,
            parent_id=parent.node_id,
            lambda_value=lambda_value,
            master_seed=master_seed,
            run_index=run_index,
            samples_per_leaf=samples_per_leaf,
        )
    return {
        "p10": percentile(parent_samples, 10),
        "p50": percentile(parent_samples, 50),
        "p90": percentile(parent_samples, 90),
    }


def build_parent_expectations(
    roots: Sequence[TreeNode],
    lambda_value: float,
    samples_per_leaf: int,
    reference_runs: int,
    master_seed: int,
) -> Dict[str, Dict[str, float]]:
    expectations: Dict[str, Dict[str, float]] = {}
    for parent in parent_nodes(roots):
        values = {"p10": [], "p50": [], "p90": []}
        for run_index in range(reference_runs):
            result = simulate_parent_once(parent, lambda_value, samples_per_leaf, run_index, master_seed)
            for key in values:
                values[key].append(result[key])

        parent_result: Dict[str, float] = {}
        for key, series in values.items():
            arr = np.asarray(series, dtype=float)
            parent_result[f"{key}_lower"] = percentile(arr, TOLERANCE_LOWER_PERCENTILE)
            parent_result[f"{key}_expected"] = percentile(arr, 50)
            parent_result[f"{key}_upper"] = percentile(arr, TOLERANCE_UPPER_PERCENTILE)
        expectations[parent.node_id] = parent_result
    return expectations


def level_values(depth: int, max_tree_depth: int, name: str) -> Dict[str, str]:
    return {f"level_{i}": (name if i == depth else "") for i in range(1, max_tree_depth + 1)}


def build_flat_rows_for_tree(
    root: TreeNode,
    expectations: Dict[str, Dict[str, float]],
    max_tree_depth: int,
) -> List[Dict[str, Any]]:
    rows: List[Dict[str, Any]] = []

    def visit(node: TreeNode, depth: int) -> None:
        base: Dict[str, Any] = {
            "node_type": node.node_type,
            "estimate_type": node.estimate_type,
            "optimistic": "",
            "likely": "",
            "pessimistic": "",
            "probability": "",
            "p10_lower": "",
            "p10_expected": "",
            "p10_upper": "",
            "p50_lower": "",
            "p50_expected": "",
            "p50_upper": "",
            "p90_lower": "",
            "p90_expected": "",
            "p90_upper": "",
            "expected_result": "accept",
            "expected_error_code": "",
            "expected_error_reason": "",
        }

        if node.node_type == "parent":
            base.update(expectations[node.node_id])
        elif node.node_type == "leaf":
            valid, code, reason = validate_leaf(node)
            if not valid:
                raise ValueError(f"Valid fixture leaf failed validation: {node.node_id}: {code}: {reason}")
            base.update(
                {
                    "optimistic": node.optimistic,
                    "likely": node.likely,
                    "pessimistic": node.pessimistic,
                    "probability": blank_if_none(node.probability),
                }
            )
        else:
            raise ValueError(f"Unsupported node_type: {node.node_type}")

        base.update(level_values(depth, max_tree_depth, node.name))
        rows.append(base)
        for child in node.children:
            visit(child, depth + 1)

    visit(root, 1)
    return rows


def invalid_leaf_cases() -> List[InvalidLeafCase]:
    return [
        InvalidLeafCase("IC001", "likely_equals_optimistic", "Risk", 100_000, 100_000, 300_000, 0.35, "LIKELY_EQUALS_OPTIMISTIC", "likely must be strictly greater than optimistic"),
        InvalidLeafCase("IC002", "likely_equals_pessimistic", "Risk", 100_000, 300_000, 300_000, 0.35, "LIKELY_EQUALS_PESSIMISTIC", "likely must be strictly less than pessimistic"),
        InvalidLeafCase("IC003", "optimistic_equals_pessimistic", "Cost", 250_000, 250_000, 250_000, None, "OPTIMISTIC_EQUALS_PESSIMISTIC", "pessimistic must be greater than optimistic"),
        InvalidLeafCase("IC004", "pessimistic_less_than_optimistic", "Cost", 500_000, 450_000, 300_000, None, "PESSIMISTIC_LESS_THAN_OPTIMISTIC", "pessimistic must be greater than optimistic"),
        InvalidLeafCase("IC005", "likely_less_than_optimistic", "Benefit", -100_000, -150_000, 300_000, None, "LIKELY_LESS_THAN_OPTIMISTIC", "likely must be greater than optimistic"),
        InvalidLeafCase("IC006", "likely_greater_than_pessimistic", "Benefit", -100_000, 450_000, 300_000, None, "LIKELY_GREATER_THAN_PESSIMISTIC", "likely must be less than pessimistic"),
        InvalidLeafCase("IC007", "missing_optimistic", "Risk", None, 150_000, 300_000, 0.4, "MISSING_OPTIMISTIC", "optimistic value is missing"),
        InvalidLeafCase("IC008", "missing_likely", "Risk", 100_000, None, 300_000, 0.4, "MISSING_LIKELY", "likely value is missing"),
        InvalidLeafCase("IC009", "missing_pessimistic", "Risk", 100_000, 150_000, None, 0.4, "MISSING_PESSIMISTIC", "pessimistic value is missing"),
        InvalidLeafCase("IC010", "non_numeric_optimistic", "Risk", "not-a-number", 150_000, 300_000, 0.4, "NON_NUMERIC_OPTIMISTIC", "optimistic value is not numeric"),
        InvalidLeafCase("IC011", "non_numeric_likely", "Risk", 100_000, "not-a-number", 300_000, 0.4, "NON_NUMERIC_LIKELY", "likely value is not numeric"),
        InvalidLeafCase("IC012", "non_numeric_pessimistic", "Risk", 100_000, 150_000, "not-a-number", 0.4, "NON_NUMERIC_PESSIMISTIC", "pessimistic value is not numeric"),
        InvalidLeafCase("IC013", "missing_risk_probability", "Risk", 100_000, 150_000, 300_000, None, "MISSING_PROBABILITY", "Risk probability is required"),
        InvalidLeafCase("IC014", "risk_probability_less_than_zero", "Risk", 100_000, 150_000, 300_000, -0.01, "PROBABILITY_LESS_THAN_ZERO", "Risk probability must be at least 0"),
        InvalidLeafCase("IC015", "risk_probability_greater_than_one", "Risk", 100_000, 150_000, 300_000, 1.01, "PROBABILITY_GREATER_THAN_ONE", "Risk probability must be at most 1"),
        InvalidLeafCase("IC016", "risk_probability_non_numeric", "Risk", 100_000, 150_000, 300_000, "likely", "NON_NUMERIC_PROBABILITY", "Risk probability must be numeric"),
        InvalidLeafCase("IC017", "cost_probability_populated", "Cost", 100_000, 150_000, 300_000, 0.5, "PROBABILITY_NOT_ALLOWED", "Probability must be blank for Cost and Benefit leaves"),
        InvalidLeafCase("IC018", "benefit_probability_populated", "Benefit", -300_000, -150_000, -50_000, 0.5, "PROBABILITY_NOT_ALLOWED", "Probability must be blank for Cost and Benefit leaves"),
    ]


def build_invalid_rows(max_tree_depth: int) -> List[Dict[str, Any]]:
    rows: List[Dict[str, Any]] = []
    for case in invalid_leaf_cases():
        candidate = TreeNode(
            node_id=case.invalid_case_id,
            name=case.case_name,
            node_type="leaf",
            estimate_type=case.estimate_type,
            optimistic=case.optimistic,
            likely=case.likely,
            pessimistic=case.pessimistic,
            probability=case.probability,
        )
        valid, code, reason = validate_leaf(candidate)
        row: Dict[str, Any] = {
            "node_type": "invalid_leaf",
            "estimate_type": case.estimate_type,
            "optimistic": blank_if_none(case.optimistic),
            "likely": blank_if_none(case.likely),
            "pessimistic": blank_if_none(case.pessimistic),
            "probability": blank_if_none(case.probability),
            "p10_lower": "",
            "p10_expected": "",
            "p10_upper": "",
            "p50_lower": "",
            "p50_expected": "",
            "p50_upper": "",
            "p90_lower": "",
            "p90_expected": "",
            "p90_upper": "",
            "expected_result": "reject",
            "expected_error_code": case.expected_error_code,
            "expected_error_reason": case.expected_error_reason,
        }
        if valid or code != case.expected_error_code:
            row["expected_error_reason"] = f"SPECIFICATION MISMATCH: actual {code}: {reason}; expected {case.expected_error_code}: {case.expected_error_reason}"
        levels = {f"level_{i}": "" for i in range(1, max_tree_depth + 1)}
        levels["level_1"] = f"Invalid input cases: {case.case_name}"
        row.update(levels)
        rows.append(row)
    return rows


def build_fixture_trees() -> List[TreeNode]:
    def parent(node_id: str, name: str, children: List[TreeNode]) -> TreeNode:
        return TreeNode(node_id=node_id, name=name, node_type="parent", children=children)

    def leaf(node_id: str, name: str, estimate_type: str, optimistic: float, likely: float, pessimistic: float, probability: Optional[float] = None) -> TreeNode:
        return TreeNode(
            node_id=node_id,
            name=name,
            node_type="leaf",
            estimate_type=estimate_type,
            optimistic=optimistic,
            likely=likely,
            pessimistic=pessimistic,
            probability=probability,
        )

    roots = [
        parent(
            "T01",
            "Cost-focused programme exposure",
            [
                parent(
                    "T01.1",
                    "Delivery cost exposure",
                    [
                        parent(
                            "T01.1.1",
                            "Implementation cost drivers",
                            [
                                leaf("T01.1.1.1", "Contractor extension", "Cost", 250_000, 600_000, 1_400_000),
                                leaf("T01.1.1.2", "Integration rework", "Cost", 100_000, 350_000, 1_100_000),
                                leaf("T01.1.1.3", "Testing overrun", "Cost", 80_000, 220_000, 650_000),
                            ],
                        ),
                        parent(
                            "T01.1.2",
                            "Operational readiness cost drivers",
                            [
                                leaf("T01.1.2.1", "Training rollout", "Cost", 60_000, 180_000, 420_000),
                                leaf("T01.1.2.2", "Support transition", "Cost", 90_000, 260_000, 750_000),
                            ],
                        ),
                    ],
                ),
                parent(
                    "T01.2",
                    "Large capital exposure",
                    [
                        leaf("T01.2.1", "Capital works delay", "Cost", 8_000_000, 14_000_000, 26_000_000),
                        leaf("T01.2.2", "Vendor remediation", "Cost", 3_500_000, 7_500_000, 18_000_000),
                        leaf("T01.2.3", "Programme extension", "Cost", 5_000_000, 11_000_000, 22_000_000),
                    ],
                ),
            ],
        ),
        parent(
            "T02",
            "Benefit-focused outcome exposure",
            [
                parent(
                    "T02.1",
                    "Operational benefit exposure",
                    [
                        leaf("T02.1.1", "Reduced support burden", "Benefit", -1_200_000, -650_000, -150_000),
                        leaf("T02.1.2", "Avoided licence cost", "Benefit", -900_000, -420_000, -80_000),
                        leaf("T02.1.3", "Reduced external spend", "Benefit", -1_600_000, -850_000, -200_000),
                    ],
                ),
                parent(
                    "T02.2",
                    "Productivity benefit exposure",
                    [
                        parent(
                            "T02.2.1",
                            "Staff productivity outcomes",
                            [
                                leaf("T02.2.1.1", "Workflow automation benefit", "Benefit", -2_500_000, -1_100_000, -250_000),
                                leaf("T02.2.1.2", "Reduced manual handling", "Benefit", -850_000, -350_000, -70_000),
                            ],
                        ),
                        leaf("T02.2.2", "Faster onboarding", "Benefit", -650_000, -280_000, -40_000),
                    ],
                ),
            ],
        ),
        parent(
            "T03",
            "Risk-focused delivery exposure",
            [
                parent(
                    "T03.1",
                    "Supplier risks",
                    [
                        leaf("T03.1.1", "Vendor delay risk", "Risk", 100_000, 350_000, 2_500_000, 0.35),
                        leaf("T03.1.2", "Key supplier issue", "Risk", 500_000, 1_200_000, 3_500_000, 1.0),
                        leaf("T03.1.3", "Alternative sourcing risk", "Risk", 50_000, 140_000, 900_000, 0.20),
                    ],
                ),
                parent(
                    "T03.2",
                    "Technical risks",
                    [
                        parent(
                            "T03.2.1",
                            "Platform stability risks",
                            [
                                leaf("T03.2.1.1", "Performance remediation risk", "Risk", 200_000, 600_000, 4_200_000, 0.28),
                                leaf("T03.2.1.2", "Security remediation risk", "Risk", 400_000, 900_000, 5_500_000, 0.18),
                            ],
                        ),
                        leaf("T03.2.2", "Data migration risk", "Risk", 150_000, 500_000, 3_800_000, 0.42),
                        leaf("T03.2.3", "Retired risk example", "Risk", 100_000, 220_000, 1_100_000, 0.0),
                    ],
                ),
            ],
        ),
        parent(
            "T04",
            "Mixed portfolio exposure",
            [
                parent(
                    "T04.1",
                    "Portfolio costs",
                    [
                        leaf("T04.1.1", "Programme delay cost", "Cost", 800_000, 2_100_000, 5_500_000),
                        leaf("T04.1.2", "Contingency drawdown", "Cost", 100_000, 500_000, 2_500_000),
                        leaf("T04.1.3", "Minor recurring cost", "Cost", 10_000, 30_000, 90_000),
                    ],
                ),
                parent(
                    "T04.2",
                    "Portfolio benefits",
                    [
                        leaf("T04.2.1", "Benefit offset", "Benefit", -3_000_000, -1_200_000, -200_000),
                        leaf("T04.2.2", "Avoided remediation", "Benefit", -1_500_000, -600_000, -100_000),
                    ],
                ),
                parent(
                    "T04.3",
                    "Portfolio risks",
                    [
                        leaf("T04.3.1", "Regulatory change risk", "Risk", 250_000, 750_000, 4_500_000, 0.25),
                        leaf("T04.3.2", "Adoption shortfall risk", "Risk", -500_000, 100_000, 900_000, 0.45),
                        parent(
                            "T04.3.3",
                            "Low probability high impact risks",
                            [
                                leaf("T04.3.3.1", "Major dependency failure", "Risk", 1_000_000, 2_000_000, 15_000_000, 0.08),
                                leaf("T04.3.3.2", "External policy shock", "Risk", -1_000_000, 500_000, 8_000_000, 0.12),
                            ],
                        ),
                    ],
                ),
            ],
        ),
    ]

    for root in roots:
        assign_parent_estimate_types(root)
    return roots


def build_flat_fixture_dataframe(
    roots: Sequence[TreeNode],
    expectations: Dict[str, Dict[str, float]],
    max_tree_depth: int,
) -> pd.DataFrame:
    rows: List[Dict[str, Any]] = []
    for root in roots:
        rows.extend(build_flat_rows_for_tree(root, expectations, max_tree_depth))
    rows.extend(build_invalid_rows(max_tree_depth))

    fixed_columns = [
        "node_type",
        "estimate_type",
        "optimistic",
        "likely",
        "pessimistic",
        "probability",
        "p10_lower",
        "p10_expected",
        "p10_upper",
        "p50_lower",
        "p50_expected",
        "p50_upper",
        "p90_lower",
        "p90_expected",
        "p90_upper",
        "expected_result",
        "expected_error_code",
        "expected_error_reason",
    ]
    level_columns = [f"level_{i}" for i in range(1, max_tree_depth + 1)]
    return pd.DataFrame(rows, columns=fixed_columns + level_columns)


def make_detail_tables(
    roots: Sequence[TreeNode],
    expectations: Dict[str, Dict[str, float]],
    lambda_value: float,
) -> Dict[str, pd.DataFrame]:
    parent_rows: List[Dict[str, Any]] = []
    leaf_rows: List[Dict[str, Any]] = []
    pert_rows: List[Dict[str, Any]] = []

    for root in roots:
        for node in all_nodes_preorder(root):
            if node.node_type == "parent":
                row = {
                    "node_id": node.node_id,
                    "name": node.name,
                    "estimate_type": node.estimate_type,
                    "descendant_leaf_count": len(descendant_leaves(node)),
                }
                row.update(expectations[node.node_id])
                parent_rows.append(row)
            elif node.node_type == "leaf":
                valid, code, reason = validate_leaf(node)
                leaf_rows.append(
                    {
                        "node_id": node.node_id,
                        "name": node.name,
                        "estimate_type": node.estimate_type,
                        "optimistic": node.optimistic,
                        "likely": node.likely,
                        "pessimistic": node.pessimistic,
                        "probability": blank_if_none(node.probability),
                        "is_valid": valid,
                        "validation_code": code,
                        "validation_reason": reason,
                    }
                )
                alpha, beta = pert_alpha_beta(float(node.optimistic), float(node.likely), float(node.pessimistic), lambda_value)
                pert_mean = (float(node.optimistic) + lambda_value * float(node.likely) + float(node.pessimistic)) / (lambda_value + 2.0)
                pert_rows.append(
                    {
                        "node_id": node.node_id,
                        "name": node.name,
                        "estimate_type": node.estimate_type,
                        "lambda_value": lambda_value,
                        "alpha": alpha,
                        "beta": beta,
                        "pert_mean": pert_mean,
                    }
                )

    return {
        "parent_expectations": pd.DataFrame(parent_rows),
        "leaf_estimates": pd.DataFrame(leaf_rows),
        "leaf_pert_parameters": pd.DataFrame(pert_rows),
        "invalid_leaf_cases": pd.DataFrame([asdict(case) for case in invalid_leaf_cases()]),
    }


def dataframe_records(df: pd.DataFrame) -> List[Dict[str, Any]]:
    clean = df.replace({np.nan: None})
    return clean.to_dict(orient="records")


def write_outputs(
    output_dir: Path,
    fixture_df: pd.DataFrame,
    roots: Sequence[TreeNode],
    expectations: Dict[str, Dict[str, float]],
    settings: Dict[str, Any],
    include_detail_outputs: bool,
    lambda_value: float,
) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)
    fixture_df.to_csv(output_dir / "risk_tree_fixture.csv", index=False)

    if not include_detail_outputs:
        return

    detail_dir = output_dir / "detail_outputs"
    detail_dir.mkdir(parents=True, exist_ok=True)
    detail_tables = make_detail_tables(roots, expectations, lambda_value)
    for name, df in detail_tables.items():
        df.to_csv(detail_dir / f"{name}.csv", index=False)

    with (detail_dir / "generation_settings.json").open("w", encoding="utf-8") as f:
        json.dump(settings, f, indent=2, ensure_ascii=False)

    payload = {
        "settings": settings,
        "flat_fixture_rows": dataframe_records(fixture_df),
    }
    with (detail_dir / "risk_tree_fixture.json").open("w", encoding="utf-8") as f:
        json.dump(payload, f, indent=2, ensure_ascii=False)


def run_generation(args: argparse.Namespace) -> None:
    if args.mode == "quick":
        reference_runs = QUICK_REFERENCE_RUNS
    elif args.mode == "audit":
        reference_runs = AUDIT_REFERENCE_RUNS
    else:
        raise ValueError(f"Unsupported mode: {args.mode}")

    if args.reference_runs is not None:
        reference_runs = args.reference_runs

    roots = build_fixture_trees()
    validate_tree_depths(roots, args.max_tree_depth)

    expectations = build_parent_expectations(
        roots=roots,
        lambda_value=args.lambda_value,
        samples_per_leaf=args.samples_per_leaf,
        reference_runs=reference_runs,
        master_seed=args.master_seed,
    )
    fixture_df = build_flat_fixture_dataframe(roots, expectations, args.max_tree_depth)

    leaves = [node for root in roots for node in all_nodes_preorder(root) if node.node_type == "leaf"]
    parents = parent_nodes(roots)
    settings = {
        "generator_version": "2.0.0",
        "mode": args.mode,
        "master_seed": args.master_seed,
        "rng_algorithm": "Mersenne Twister via numpy.random.RandomState / MT19937",
        "sample_parity_required": False,
        "statistical_equivalence_required": True,
        "samples_per_leaf": args.samples_per_leaf,
        "reference_runs": reference_runs,
        "lambda_value": args.lambda_value,
        "valid_leaf_rule": "optimistic < likely < pessimistic; Risk probability 0 <= p <= 1; Cost/Benefit probability blank",
        "risk_occurrence_model": "Risk contribution = Bernoulli(probability) * beta-PERT impact sample",
        "cost_benefit_occurrence_model": "Cost and Benefit leaves always occur",
        "issue_model": "Issue is represented as estimate_type Risk with probability 1.0",
        "pert_alpha_formula": "1 + lambda_value * (likely - optimistic) / (pessimistic - optimistic)",
        "pert_beta_formula": "1 + lambda_value * (pessimistic - likely) / (pessimistic - optimistic)",
        "scaling_formula": "scaled_sample = optimistic + beta_sample * (pessimistic - optimistic)",
        "parent_aggregation": "Each parent aggregates all descendant leaves with trial-wise summation",
        "percentile_convention": "literal statistical percentiles: P10=10th, P50=50th, P90=90th",
        "expected_value_rule": "median of repeated reference-run quantile estimates",
        "tolerance_lower_percentile": TOLERANCE_LOWER_PERCENTILE,
        "tolerance_upper_percentile": TOLERANCE_UPPER_PERCENTILE,
        "max_tree_depth": args.max_tree_depth,
        "root_tree_count": len(roots),
        "parent_node_count": len(parents),
        "valid_leaf_count": len(leaves),
        "invalid_leaf_case_count": len(invalid_leaf_cases()),
        "primary_output": "risk_tree_fixture.csv",
        "include_detail_outputs": args.include_detail_outputs,
    }

    write_outputs(
        output_dir=Path(args.output_dir),
        fixture_df=fixture_df,
        roots=roots,
        expectations=expectations,
        settings=settings,
        include_detail_outputs=args.include_detail_outputs,
        lambda_value=args.lambda_value,
    )

    print(f"Generated primary fixture: {(Path(args.output_dir) / 'risk_tree_fixture.csv').resolve()}")
    print(f"Mode: {args.mode}; reference_runs: {reference_runs}; samples_per_leaf: {args.samples_per_leaf}")
    print(f"Root trees: {len(roots)}; parent nodes: {len(parents)}; valid leaves: {len(leaves)}; invalid leaf cases: {len(invalid_leaf_cases())}")
    if args.include_detail_outputs:
        print(f"Detail outputs: {(Path(args.output_dir) / 'detail_outputs').resolve()}")


def parse_args(argv: Optional[Sequence[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate flattened beta-PERT Monte Carlo tree fixture CSV.")
    parser.add_argument("--mode", choices=("quick", "audit"), default="quick", help="quick=100 reference runs; audit=10,000 reference runs. Default: quick.")
    parser.add_argument("--output-dir", default="generated", help="Directory for generated outputs. Default: generated.")
    parser.add_argument("--lambda-value", type=float, default=DEFAULT_LAMBDA_VALUE, help="Modified beta-PERT lambda value. Default: 4.0.")
    parser.add_argument("--samples-per-leaf", type=int, default=SAMPLES_PER_LEAF, help="Monte Carlo samples per leaf per reference run. Default: 10000.")
    parser.add_argument("--master-seed", type=int, default=MASTER_SEED, help=f"Master seed. Default: {MASTER_SEED}.")
    parser.add_argument("--reference-runs", type=int, default=None, help="Override repeated reference runs, mainly for smoke testing.")
    parser.add_argument("--max-tree-depth", type=int, default=DEFAULT_MAX_TREE_DEPTH, help="Number of level_* columns to emit. Default: 4.")
    parser.add_argument("--include-detail-outputs", action="store_true", help="Also write optional debug/audit CSV and JSON files under detail_outputs/.")
    return parser.parse_args(argv)


def main(argv: Optional[Sequence[str]] = None) -> None:
    args = parse_args(argv)
    if args.lambda_value <= 0:
        raise ValueError("lambda_value must be positive")
    if args.samples_per_leaf <= 0:
        raise ValueError("samples_per_leaf must be positive")
    if args.reference_runs is not None and args.reference_runs <= 0:
        raise ValueError("reference_runs must be positive")
    if args.max_tree_depth <= 0:
        raise ValueError("max_tree_depth must be positive")
    run_generation(args)


if __name__ == "__main__":
    main()
