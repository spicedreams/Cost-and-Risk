#!/usr/bin/env python3
"""Generate deterministic hierarchical Monte Carlo fixture data.

The implementation follows the supplied 2026-08-10 specification:
* node_type: Root, Aggregate, Estimator, invalid_Estimator
* estimate_type: Cost, Benefit, Risk, Issue, Treatment, Residual
* estimator consensus: trial-wise mean of immediate Estimator children
* hierarchy roll-up: trial-wise sum of immediate Aggregate children
* mixed parent: estimator mean plus aggregate-child sum
* exposure: beta-PERT severity multiplied element-wise by Bernoulli occurrence
* Benefit exposure is negated
* Treatment is calculated from Cost and Residual children, but is excluded from
  roll-up into its parent Risk or Issue.
* Tree depth is unrestricted; level columns are determined from the generated trees.

The first CSV record contains the master seed. The second record is the header.
Input estimates are integers; valid probabilities are written to two decimals;
expected quantiles and tolerance limits are rounded to integers.

Python 3.9+ is required. NumPy is installed at runtime if unavailable.
"""
from __future__ import annotations

import argparse
import csv
import hashlib
from datetime import datetime
import importlib.util
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Sequence, Tuple


def ensure_numpy() -> None:
    if importlib.util.find_spec("numpy") is None:
        print("NumPy is missing; installing it with the active Python interpreter.")
        subprocess.check_call([sys.executable, "-m", "pip", "install", "numpy"])


ensure_numpy()
import numpy as np

MASTER_SEED = 20260805
DEFAULT_LAMBDA = 4.0
DEFAULT_SAMPLES = 10_000
QUICK_RUNS = 100
AUDIT_RUNS = 10_000
LOWER_TOLERANCE_PERCENTILE = 0.5
UPPER_TOLERANCE_PERCENTILE = 99.5

NODE_TYPES = {"Root", "Aggregate", "Estimator", "invalid_Estimator"}
ESTIMATE_TYPES = {"Cost", "Benefit", "Risk", "Issue", "Treatment", "Residual"}
PARENT_NODE_TYPES = {"Root", "Aggregate"}


@dataclass
class Node:
    node_id: str
    name: str
    node_type: str
    estimate_type: str
    opt: Optional[Any] = None
    likely: Optional[Any] = None
    pess: Optional[Any] = None
    prob: Optional[Any] = None
    children: List["Node"] = field(default_factory=list)
    expected_error_code: str = ""
    expected_error_reason: str = ""


def stable_seed(*parts: Any) -> int:
    text = "|".join(str(part) for part in parts)
    digest = hashlib.sha256(text.encode("utf-8")).digest()
    return int.from_bytes(digest[:8], "big") % (2**32 - 1)


def np_percentile(values: np.ndarray, q: float) -> float:
    try:
        return float(np.percentile(values, q, method="linear"))
    except TypeError:  # NumPy before 1.22
        return float(np.percentile(values, q, interpolation="linear"))


def make_estimator(node_id: str, name: str, estimate_type: str,
                   opt: int, likely: int, pess: int, prob: float) -> Node:
    return Node(node_id, name, "Estimator", estimate_type,
                opt, likely, pess, round(prob, 2))


def cost_values(rng: np.random.RandomState) -> Tuple[int, int, int]:
    opt = int(rng.randint(1_000, 48_001))
    pess = int(rng.randint(max(opt + 2_000, 20_000), 100_001))
    # Cost mode is deliberately close to the optimistic bound.
    likely = opt + max(1, int(round((pess - opt) * rng.uniform(0.16, 0.34))))
    return opt, min(likely, pess - 1), pess


def benefit_values(rng: np.random.RandomState) -> Tuple[int, int, int]:
    opt = int(rng.randint(1_000, 48_001))
    pess = int(rng.randint(max(opt + 2_000, 20_000), 100_001))
    # Benefit mode is deliberately close to the pessimistic bound.
    likely = opt + max(1, int(round((pess - opt) * rng.uniform(0.66, 0.84))))
    return opt, min(likely, pess - 1), pess


def inherited_values(estimate_type: str, rng: np.random.RandomState) -> Tuple[int, int, int]:
    return benefit_values(rng) if estimate_type == "Benefit" else cost_values(rng)


def estimator_group(prefix: str, estimate_type: str, count: int,
                    rng: np.random.RandomState, probability: Optional[float] = None) -> List[Node]:
    result: List[Node] = []
    for index in range(1, count + 1):
        opt, likely, pess = inherited_values(estimate_type, rng)
        if probability is not None:
            prob = probability
        elif estimate_type == "Issue":
            prob = 1.0
        elif estimate_type in {"Risk", "Residual"}:
            prob = float(rng.randint(10, 91)) / 100.0
        else:
            prob = 1.0
        result.append(make_estimator(
            f"{prefix}-E{index}", f"Estimator {index}", estimate_type,
            opt, likely, pess, prob
        ))
    return result


def build_fixture_trees(master_seed: int) -> List[Node]:
    rng = np.random.RandomState(stable_seed(master_seed, "fixture-shape"))

    cost_design = Node("C-A1", "Design costs", "Aggregate", "Cost",
                       children=estimator_group("C-A1", "Cost", 3, rng))
    cost_delivery = Node("C-A2", "Delivery costs", "Aggregate", "Cost", children=[
        Node("C-A2-A1", "Implementation", "Aggregate", "Cost",
             children=estimator_group("C-A2-A1", "Cost", 2, rng)),
        *estimator_group("C-A2", "Cost", 2, rng),
    ])
    cost_root = Node("C-ROOT", "Costs", "Root", "Cost",
                     children=[cost_design, cost_delivery, *estimator_group("C-ROOT", "Cost", 2, rng)])

    benefit_adoption = Node("B-A1", "Adoption benefits", "Aggregate", "Benefit",
                            children=estimator_group("B-A1", "Benefit", 3, rng))
    benefit_capability = Node("B-A2", "Capability benefits", "Aggregate", "Benefit", children=[
        Node("B-A2-A1", "Service improvement", "Aggregate", "Benefit",
             children=estimator_group("B-A2-A1", "Benefit", 2, rng)),
        *estimator_group("B-A2", "Benefit", 2, rng),
    ])
    benefit_root = Node("B-ROOT", "Benefits", "Root", "Benefit",
                        children=[benefit_adoption, benefit_capability, *estimator_group("B-ROOT", "Benefit", 2, rng)])

    risk_cost = Node("R-COST", "Direct risk cost", "Aggregate", "Cost",
                     children=estimator_group("R-COST", "Cost", 2, rng))
    residual = Node("R-TR-RES", "Residual exposure", "Aggregate", "Residual",
                    children=estimator_group("R-TR-RES", "Residual", 2, rng))
    treatment_cost = Node("R-TR-COST", "Treatment implementation cost", "Aggregate", "Cost",
                          children=estimator_group("R-TR-COST", "Cost", 2, rng))
    treatment = Node("R-TR", "Preferred treatment", "Aggregate", "Treatment",
                     children=[treatment_cost, residual])
    risk_root = Node("R-ROOT", "Risks", "Root", "Risk", children=[
        *estimator_group("R-ROOT", "Risk", 3, rng), risk_cost, treatment
    ])

    issue_cost = Node("I-COST", "Issue response cost", "Aggregate", "Cost",
                      children=estimator_group("I-COST", "Cost", 2, rng))
    issue_residual = Node("I-TR-RES", "Post-treatment issue exposure", "Aggregate", "Residual",
                          children=estimator_group("I-TR-RES", "Residual", 2, rng, probability=1.0))
    issue_treatment_cost = Node("I-TR-COST", "Issue treatment cost", "Aggregate", "Cost",
                                children=estimator_group("I-TR-COST", "Cost", 2, rng))
    issue_treatment = Node("I-TR", "Issue treatment", "Aggregate", "Treatment",
                           children=[issue_treatment_cost, issue_residual])
    issue_root = Node("I-ROOT", "Issues", "Root", "Issue", children=[
        *estimator_group("I-ROOT", "Issue", 3, rng, probability=1.0),
        issue_cost, issue_treatment
    ])
    return [cost_root, benefit_root, risk_root, issue_root]


def validate_estimator(node: Node) -> Tuple[bool, str, str]:
    if node.node_type not in {"Estimator", "invalid_Estimator"}:
        return False, "NOT_ESTIMATOR", "node_type is not Estimator"
    if node.estimate_type not in ESTIMATE_TYPES:
        return False, "INVALID_ESTIMATE_TYPE", "estimate_type is not recognised"
    for field_name in ("opt", "likely", "pess"):
        value = getattr(node, field_name)
        if value in (None, ""):
            return False, f"MISSING_{field_name.upper()}", f"{field_name} is missing"
        if isinstance(value, bool) or not isinstance(value, (int, float, np.integer, np.floating)):
            return False, f"NON_NUMERIC_{field_name.upper()}", f"{field_name} is not numeric"
        if float(value) != int(value):
            return False, f"NON_INTEGER_{field_name.upper()}", f"{field_name} must be an integer"
    opt, likely, pess = int(node.opt), int(node.likely), int(node.pess)
    if not 1_000 <= opt <= 100_000 or not 1_000 <= likely <= 100_000 or not 1_000 <= pess <= 100_000:
        return False, "ESTIMATE_OUT_OF_RANGE", "estimates must be between 1000 and 100000"
    if pess <= opt:
        return False, "PESS_NOT_GREATER_THAN_OPT", "pess must be greater than opt"
    if likely <= opt:
        return False, "LIKELY_NOT_GREATER_THAN_OPT", "likely must be greater than opt"
    if likely >= pess:
        return False, "LIKELY_NOT_LESS_THAN_PESS", "likely must be less than pess"
    if node.prob in (None, ""):
        return False, "MISSING_PROBABILITY", "probability is required"
    if isinstance(node.prob, bool) or not isinstance(node.prob, (int, float, np.integer, np.floating)):
        return False, "NON_NUMERIC_PROBABILITY", "probability must be numeric"
    if not 0.0 <= float(node.prob) <= 1.0:
        return False, "PROBABILITY_OUT_OF_RANGE", "probability must be from 0 to 1"
    if round(float(node.prob), 2) != float(node.prob):
        return False, "PROBABILITY_TOO_PRECISE", "probability may have at most two decimal places"
    if node.estimate_type == "Issue" and float(node.prob) != 1.0:
        return False, "ISSUE_PROBABILITY_NOT_ONE", "Issue probability must equal 1"
    return True, "", ""


def beta_pert_samples(node: Node, lambda_value: float, seed: int, samples: int) -> np.ndarray:
    valid, code, reason = validate_estimator(node)
    if not valid:
        raise ValueError(f"{node.node_id}: {code}: {reason}")
    opt, likely, pess = float(node.opt), float(node.likely), float(node.pess)
    span = pess - opt
    alpha = 1.0 + lambda_value * (likely - opt) / span
    beta = 1.0 + lambda_value * (pess - likely) / span
    rng = np.random.RandomState(seed)
    return opt + rng.beta(alpha, beta, size=samples) * span


def estimator_exposure(node: Node, master_seed: int, run_index: int,
                       samples: int, lambda_value: float) -> np.ndarray:
    severity = beta_pert_samples(
        node, lambda_value,
        stable_seed(master_seed, node.node_id, run_index, "severity"), samples)
    occurrence_rng = np.random.RandomState(
        stable_seed(master_seed, node.node_id, run_index, "occurrence"))
    occurrence = occurrence_rng.binomial(1, float(node.prob), size=samples)
    exposure = severity * occurrence
    return -exposure if node.estimate_type == "Benefit" else exposure


def aggregate_samples(node: Node, master_seed: int, run_index: int,
                      samples: int, lambda_value: float) -> np.ndarray:
    if node.node_type == "Estimator":
        return estimator_exposure(node, master_seed, run_index, samples, lambda_value)
    if node.node_type not in PARENT_NODE_TYPES:
        return np.zeros(samples, dtype=float)

    estimator_arrays = [
        aggregate_samples(child, master_seed, run_index, samples, lambda_value)
        for child in node.children if child.node_type == "Estimator"
    ]
    aggregate_arrays = []
    for child in node.children:
        if child.node_type not in PARENT_NODE_TYPES:
            continue
        # Treatment alternatives are evaluated, but do not alter untreated Risk/Issue exposure.
        if node.estimate_type in {"Risk", "Issue"} and child.estimate_type == "Treatment":
            continue
        aggregate_arrays.append(
            aggregate_samples(child, master_seed, run_index, samples, lambda_value))

    result = np.zeros(samples, dtype=float)
    if estimator_arrays:
        result += np.mean(np.vstack(estimator_arrays), axis=0)
    if aggregate_arrays:
        result += np.sum(np.vstack(aggregate_arrays), axis=0)
    return result


def preorder(nodes: Iterable[Node], depth: int = 1) -> Iterable[Tuple[Node, int]]:
    for node in nodes:
        yield node, depth
        yield from preorder(node.children, depth + 1)


def parent_expectations(roots: Sequence[Node], master_seed: int, reference_runs: int,
                        samples: int, lambda_value: float) -> Dict[str, Dict[str, int]]:
    output: Dict[str, Dict[str, int]] = {}
    parents = [node for node, _ in preorder(roots) if node.node_type in PARENT_NODE_TYPES]
    for parent_index, parent in enumerate(parents, start=1):
        print(f"Reference simulation {parent_index}/{len(parents)}: {parent.name}")
        run_quantiles = {10: [], 50: [], 90: []}
        for run_index in range(reference_runs):
            curve = aggregate_samples(parent, master_seed, run_index, samples, lambda_value)
            for q in run_quantiles:
                run_quantiles[q].append(np_percentile(curve, q))
        fields: Dict[str, int] = {}
        for q, values in run_quantiles.items():
            array = np.asarray(values, dtype=float)
            fields[f"p{q}_lower"] = int(round(np_percentile(array, LOWER_TOLERANCE_PERCENTILE)))
            fields[f"p{q}_expected"] = int(round(np_percentile(array, 50)))
            fields[f"p{q}_upper"] = int(round(np_percentile(array, UPPER_TOLERANCE_PERCENTILE)))
        output[parent.node_id] = fields
    return output


def invalid_cases() -> List[Node]:
    specs = [
        ("INV001", "missing_opt", "Risk", None, 15000, 50000, 0.35, "MISSING_OPT", "opt is missing"),
        ("INV002", "non_numeric_likely", "Cost", 10000, "high", 50000, 1.00, "NON_NUMERIC_LIKELY", "likely is not numeric"),
        ("INV003", "pess_below_opt", "Benefit", 60000, 50000, 40000, 1.00, "PESS_NOT_GREATER_THAN_OPT", "pess must be greater than opt"),
        ("INV004", "likely_equals_opt", "Cost", 10000, 10000, 50000, 1.00, "LIKELY_NOT_GREATER_THAN_OPT", "likely must be greater than opt"),
        ("INV005", "likely_above_pess", "Risk", 10000, 60000, 50000, 0.40, "LIKELY_NOT_LESS_THAN_PESS", "likely must be less than pess"),
        ("INV006", "estimate_below_range", "Cost", 999, 10000, 50000, 1.00, "ESTIMATE_OUT_OF_RANGE", "estimates must be between 1000 and 100000"),
        ("INV007", "estimate_above_range", "Benefit", 10000, 50000, 100001, 1.00, "ESTIMATE_OUT_OF_RANGE", "estimates must be between 1000 and 100000"),
        ("INV008", "missing_probability", "Risk", 10000, 20000, 50000, None, "MISSING_PROBABILITY", "probability is required"),
        ("INV009", "probability_below_zero", "Risk", 10000, 20000, 50000, -0.01, "PROBABILITY_OUT_OF_RANGE", "probability must be from 0 to 1"),
        ("INV010", "probability_above_one", "Risk", 10000, 20000, 50000, 1.01, "PROBABILITY_OUT_OF_RANGE", "probability must be from 0 to 1"),
        ("INV011", "non_numeric_probability", "Risk", 10000, 20000, 50000, "often", "NON_NUMERIC_PROBABILITY", "probability must be numeric"),
        ("INV012", "probability_too_precise", "Residual", 10000, 20000, 50000, 0.333, "PROBABILITY_TOO_PRECISE", "probability may have at most two decimal places"),
        ("INV013", "issue_probability_not_one", "Issue", 10000, 20000, 50000, 0.75, "ISSUE_PROBABILITY_NOT_ONE", "Issue probability must equal 1"),
        ("INV014", "non_integer_estimate", "Treatment", 10000, 20000.5, 50000, 1.00, "NON_INTEGER_LIKELY", "likely must be an integer"),
        ("INV015", "invalid_estimate_type", "Unknown", 10000, 20000, 50000, 0.50, "INVALID_ESTIMATE_TYPE", "estimate_type is not recognised"),
    ]
    return [Node(i, name, "invalid_Estimator", et, opt, likely, pess, prob,
                 expected_error_code=code, expected_error_reason=reason)
            for i, name, et, opt, likely, pess, prob, code, reason in specs]


def format_probability(value: Any) -> Any:
    if value in (None, ""):
        return ""
    if isinstance(value, (int, float, np.integer, np.floating)):
        return f"{float(value):.2f}"
    return value


def make_rows(roots: Sequence[Node], expectations: Dict[str, Dict[str, int]],
              max_depth: int) -> List[Dict[str, Any]]:
    quantile_columns = [f"p{q}_{suffix}" for q in (10, 50, 90)
                        for suffix in ("lower", "expected", "upper")]
    rows: List[Dict[str, Any]] = []
    for node, depth in preorder(roots):
        row: Dict[str, Any] = {
            "node_type": node.node_type, "estimate_type": node.estimate_type,
            "opt": node.opt if node.node_type == "Estimator" else "",
            "likely": node.likely if node.node_type == "Estimator" else "",
            "pess": node.pess if node.node_type == "Estimator" else "",
            "prob": format_probability(node.prob) if node.node_type == "Estimator" else "",
            **{column: expectations.get(node.node_id, {}).get(column, "") for column in quantile_columns},
            "expected_error_code": "", "expected_error_reason": "",
        }
        row.update({f"level_{level}": node.name if level == depth else ""
                    for level in range(1, max_depth + 1)})
        rows.append(row)

    for node in invalid_cases():
        valid, actual_code, actual_reason = validate_estimator(node)
        reason = node.expected_error_reason
        if valid or actual_code != node.expected_error_code:
            reason = (f"SPECIFICATION MISMATCH: validator returned {actual_code}: "
                      f"{actual_reason}; expected {node.expected_error_code}: {reason}")
        row = {
            "node_type": node.node_type, "estimate_type": node.estimate_type,
            "opt": "" if node.opt is None else node.opt,
            "likely": "" if node.likely is None else node.likely,
            "pess": "" if node.pess is None else node.pess,
            "prob": format_probability(node.prob),
            **{column: "" for column in quantile_columns},
            "expected_error_code": node.expected_error_code,
            "expected_error_reason": reason,
            **{f"level_{level}": (f"Invalid: {node.name}" if level == 1 else "")
               for level in range(1, max_depth + 1)},
        }
        rows.append(row)
    return rows


def write_csv(path: Path, rows: List[Dict[str, Any]], master_seed: int) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fieldnames = list(rows[0].keys())
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.writer(handle)
        writer.writerow(["master_seed", master_seed])
        dict_writer = csv.DictWriter(handle, fieldnames=fieldnames, extrasaction="raise")
        dict_writer.writeheader()
        dict_writer.writerows(rows)


def validate_tree_structure(roots: Sequence[Node]) -> None:
    ids: set[str] = set()
    for node, depth in preorder(roots):
        if node.node_id in ids:
            raise ValueError(f"Duplicate node_id: {node.node_id}")
        ids.add(node.node_id)
        if node.node_type not in NODE_TYPES:
            raise ValueError(f"Invalid node_type on {node.node_id}: {node.node_type}")
        if node.estimate_type not in ESTIMATE_TYPES:
            raise ValueError(f"Invalid estimate_type on {node.node_id}: {node.estimate_type}")
        if node.node_type == "Estimator":
            valid, code, reason = validate_estimator(node)
            if not valid:
                raise ValueError(f"Generated invalid estimator {node.node_id}: {code}: {reason}")
        if node.node_type in PARENT_NODE_TYPES and not node.children:
            raise ValueError(f"Parent node has no children: {node.node_id}")
        if node.estimate_type == "Treatment" and node.node_type in PARENT_NODE_TYPES:
            child_types = {child.estimate_type for child in node.children}
            if not {"Cost", "Residual"}.issubset(child_types):
                raise ValueError(f"Treatment {node.node_id} needs Cost and Residual children")


def parse_args(argv: Optional[Sequence[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate hierarchical beta-PERT Monte Carlo fixture data.")
    parser.add_argument("--mode", choices=("quick", "audit"), default="quick")
    parser.add_argument(
        "--output", default=None,
        help=("Output CSV path. Default: generated/<yyyy-mm-dd-hh-mm>.csv, "
              "using the local date and time when the script starts."))
    parser.add_argument("--master-seed", type=int, default=MASTER_SEED)
    parser.add_argument("--lambda-value", type=float, default=DEFAULT_LAMBDA)
    parser.add_argument("--samples", type=int, default=DEFAULT_SAMPLES)
    parser.add_argument("--reference-runs", type=int, default=None,
                        help="Override mode run count, primarily for smoke tests.")
    return parser.parse_args(argv)


def main(argv: Optional[Sequence[str]] = None) -> None:
    args = parse_args(argv)
    if args.lambda_value <= 0 or args.samples <= 0:
        raise ValueError("lambda-value and samples must be positive")
    reference_runs = args.reference_runs
    if reference_runs is None:
        reference_runs = QUICK_RUNS if args.mode == "quick" else AUDIT_RUNS
    if reference_runs <= 0:
        raise ValueError("reference-runs must be positive")

    roots = build_fixture_trees(args.master_seed)
    validate_tree_structure(roots)
    expectations = parent_expectations(
        roots, args.master_seed, reference_runs, args.samples, args.lambda_value)
    actual_max_depth = max(depth for _, depth in preorder(roots))
    rows = make_rows(roots, expectations, actual_max_depth)
    default_name = datetime.now().strftime("%Y-%m-%d-%H-%M") + ".csv"
    output_path = Path(args.output) if args.output else Path("generated") / default_name
    write_csv(output_path, rows, args.master_seed)
    print(f"Wrote {len(rows)} rows to {output_path}")


if __name__ == "__main__":
    main()
