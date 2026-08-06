#!/usr/bin/env python3
"""
Generate deterministic beta-PERT Monte Carlo test fixtures for additive parent-risk impact aggregation.

Specification mirrored by this generator:
- Each valid child has min, likely, and max impact estimates.
- Valid child rule: min < likely < max.
- Modified beta-PERT parameters:
    alpha = 1 + lambda_value * (likely - min) / (max - min)
    beta  = 1 + lambda_value * (max - likely) / (max - min)
- Draw 10,000 beta samples per child using a Mersenne-Twister based RNG.
- Scale samples into the child's min..max impact range.
- Sum child sample arrays trial-wise to create the parent empirical curve.
- Report literal statistical P10, P50, and P90 for each parent.
- Estimate empirical tolerance bands via repeated reference simulations.

Outputs:
- JSON and CSV files written to the selected output directory.

Dependencies:
- Python 3.9+
- numpy
- pandas
"""

from __future__ import annotations

import argparse
import hashlib
import json
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Sequence, Tuple

import importlib.util
import subprocess
import sys


# Convenience bootstrap only.
# For formal audit generation, prefer running inside a project virtual environment
# with pinned dependency versions.
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

    subprocess.check_call([
        sys.executable,
        "-m",
        "pip",
        "install",
        *missing,
    ])

ensure_dependencies()

import numpy as np
import pandas as pd

MASTER_SEED = 20260805
DEFAULT_LAMBDA_VALUE = 4.0
SAMPLES_PER_CHILD = 10_000
QUICK_REFERENCE_RUNS = 100
AUDIT_REFERENCE_RUNS = 10_000
TOLERANCE_LOWER_PERCENTILE = 0.5
TOLERANCE_UPPER_PERCENTILE = 99.5
TARGET_QUANTILES = (10, 50, 90)


@dataclass(frozen=True)
class ChildEstimate:
    parent_id: str
    child_id: str
    child_name: str
    min_value: float
    likely_value: float
    max_value: float
    scenario: str


@dataclass(frozen=True)
class InvalidChildCase:
    invalid_case_id: str
    case_name: str
    min_value: Any
    likely_value: Any
    max_value: Any
    expected_result: str
    expected_error_code: str
    expected_error_reason: str


@dataclass(frozen=True)
class ParentScenario:
    parent_id: str
    parent_name: str
    scenario: str
    children: Sequence[ChildEstimate]


def stable_seed(*parts: Any) -> int:
    """Return a deterministic 32-bit seed from identifying parts."""
    text = "|".join(str(part) for part in parts)
    digest = hashlib.sha256(text.encode("utf-8")).digest()
    return int.from_bytes(digest[:8], byteorder="big", signed=False) % (2**32 - 1)


def validate_child_values(min_value: Any, likely_value: Any, max_value: Any) -> Tuple[bool, str, str]:
    """Validate a child estimate against the strict fixture rule min < likely < max."""
    values = {"min": min_value, "likely": likely_value, "max": max_value}
    for label, value in values.items():
        if value is None:
            return False, f"MISSING_{label.upper()}", f"{label} value is missing"
        try:
            float(value)
        except (TypeError, ValueError):
            return False, f"NON_NUMERIC_{label.upper()}", f"{label} value is not numeric"

    mn = float(min_value)
    lk = float(likely_value)
    mx = float(max_value)

    if mx < mn:
        return False, "MAX_LESS_THAN_MIN", "max must be greater than min"
    if mx == mn:
        return False, "MIN_EQUALS_MAX", "max must be greater than min"
    if lk < mn:
        return False, "LIKELY_LESS_THAN_MIN", "likely must be greater than min"
    if lk > mx:
        return False, "LIKELY_GREATER_THAN_MAX", "likely must be less than max"
    if lk == mn:
        return False, "LIKELY_EQUALS_MIN", "likely must be strictly greater than min"
    if lk == mx:
        return False, "LIKELY_EQUALS_MAX", "likely must be strictly less than max"
    return True, "VALID", "valid child estimate"


def pert_alpha_beta(min_value: float, likely_value: float, max_value: float, lambda_value: float) -> Tuple[float, float]:
    """Calculate modified beta-PERT alpha and beta parameters."""
    is_valid, code, reason = validate_child_values(min_value, likely_value, max_value)
    if not is_valid:
        raise ValueError(f"Invalid child estimate: {code}: {reason}")
    span = max_value - min_value
    alpha = 1.0 + lambda_value * (likely_value - min_value) / span
    beta = 1.0 + lambda_value * (max_value - likely_value) / span
    return alpha, beta


def beta_pert_samples(
    child: ChildEstimate,
    lambda_value: float,
    seed: int,
    samples_per_child: int,
) -> np.ndarray:
    """Draw Mersenne-Twister beta-PERT samples and scale them to the child range."""
    alpha, beta = pert_alpha_beta(
        child.min_value,
        child.likely_value,
        child.max_value,
        lambda_value,
    )
    rng = np.random.RandomState(seed)  # NumPy legacy RandomState uses MT19937.
    unit_samples = rng.beta(alpha, beta, size=samples_per_child)
    return child.min_value + unit_samples * (child.max_value - child.min_value)


def percentile(values: np.ndarray, q: float) -> float:
    """A stable percentile wrapper using NumPy's linear interpolation method."""
    try:
        return float(np.percentile(values, q, method="linear"))
    except TypeError:  # compatibility with older NumPy versions
        return float(np.percentile(values, q, interpolation="linear"))


def simulate_parent_once(
    parent: ParentScenario,
    lambda_value: float,
    samples_per_child: int,
    run_index: int,
    master_seed: int,
) -> Dict[str, float]:
    """Run one reference simulation for a parent scenario and return P10/P50/P90."""
    parent_samples = np.zeros(samples_per_child, dtype=float)

    for child in parent.children:
        seed = stable_seed(master_seed, parent.parent_id, child.child_id, run_index, "impact")
        parent_samples += beta_pert_samples(child, lambda_value, seed, samples_per_child)

    return {
        "p10": percentile(parent_samples, 10),
        "p50": percentile(parent_samples, 50),
        "p90": percentile(parent_samples, 90),
    }


def build_parent_expectations(
    parents: Sequence[ParentScenario],
    lambda_value: float,
    samples_per_child: int,
    reference_runs: int,
    master_seed: int,
) -> pd.DataFrame:
    """Estimate expected quantiles and tolerance bands from repeated reference simulations."""
    rows: List[Dict[str, Any]] = []

    for parent in parents:
        run_quantiles = {"p10": [], "p50": [], "p90": []}
        for run_index in range(reference_runs):
            result = simulate_parent_once(
                parent=parent,
                lambda_value=lambda_value,
                samples_per_child=samples_per_child,
                run_index=run_index,
                master_seed=master_seed,
            )
            for key in run_quantiles:
                run_quantiles[key].append(result[key])

        for quantile_label, values in run_quantiles.items():
            arr = np.asarray(values, dtype=float)
            expected_value = percentile(arr, 50)
            lower_bound = percentile(arr, TOLERANCE_LOWER_PERCENTILE)
            upper_bound = percentile(arr, TOLERANCE_UPPER_PERCENTILE)
            rows.append(
                {
                    "parent_id": parent.parent_id,
                    "parent_name": parent.parent_name,
                    "scenario": parent.scenario,
                    "child_count": len(parent.children),
                    "quantile": quantile_label.upper(),
                    "expected_value": expected_value,
                    "lower_bound": lower_bound,
                    "upper_bound": upper_bound,
                    "absolute_tolerance_minus": expected_value - lower_bound,
                    "absolute_tolerance_plus": upper_bound - expected_value,
                    "reference_runs": reference_runs,
                    "samples_per_child": samples_per_child,
                    "lambda_value": lambda_value,
                    "tolerance_lower_percentile": TOLERANCE_LOWER_PERCENTILE,
                    "tolerance_upper_percentile": TOLERANCE_UPPER_PERCENTILE,
                    "expected_result": "accept",
                }
            )

    return pd.DataFrame(rows)


def build_pert_parameter_table(parents: Sequence[ParentScenario], lambda_value: float) -> pd.DataFrame:
    rows: List[Dict[str, Any]] = []
    for parent in parents:
        for child in parent.children:
            alpha, beta = pert_alpha_beta(child.min_value, child.likely_value, child.max_value, lambda_value)
            pert_mean = (child.min_value + lambda_value * child.likely_value + child.max_value) / (lambda_value + 2.0)
            rows.append(
                {
                    "parent_id": parent.parent_id,
                    "child_id": child.child_id,
                    "child_name": child.child_name,
                    "scenario": child.scenario,
                    "min_value": child.min_value,
                    "likely_value": child.likely_value,
                    "max_value": child.max_value,
                    "lambda_value": lambda_value,
                    "alpha": alpha,
                    "beta": beta,
                    "pert_mean": pert_mean,
                    "valid_rule": "min < likely < max",
                }
            )
    return pd.DataFrame(rows)


def build_child_estimate_table(parents: Sequence[ParentScenario]) -> pd.DataFrame:
    rows: List[Dict[str, Any]] = []
    for parent in parents:
        for child in parent.children:
            is_valid, code, reason = validate_child_values(child.min_value, child.likely_value, child.max_value)
            rows.append(
                {
                    "parent_id": parent.parent_id,
                    "parent_name": parent.parent_name,
                    "child_id": child.child_id,
                    "child_name": child.child_name,
                    "scenario": child.scenario,
                    "min_value": child.min_value,
                    "likely_value": child.likely_value,
                    "max_value": child.max_value,
                    "is_valid_input": is_valid,
                    "validation_code": code,
                    "validation_reason": reason,
                }
            )
    return pd.DataFrame(rows)


def build_invalid_child_table(invalid_cases: Sequence[InvalidChildCase]) -> pd.DataFrame:
    rows = []
    for case in invalid_cases:
        row = asdict(case)
        is_valid, code, reason = validate_child_values(case.min_value, case.likely_value, case.max_value)
        row.update(
            {
                "actual_validation_result": "accept" if is_valid else "reject",
                "actual_error_code": code,
                "actual_error_reason": reason,
                "validation_matches_expected": (not is_valid) and code == case.expected_error_code,
            }
        )
        rows.append(row)
    return pd.DataFrame(rows)


def valid_parent_scenarios() -> List[ParentScenario]:
    """Define 12 named valid parent scenarios with deliberately varied child counts."""

    def c(parent_id: str, idx: int, name: str, mn: float, lk: float, mx: float, scenario: str) -> ChildEstimate:
        return ChildEstimate(parent_id, f"{parent_id}_C{idx:02d}", name, mn, lk, mx, scenario)

    scenarios: List[ParentScenario] = []

    scenarios.append(
        ParentScenario(
            "P01",
            "Small positive costs",
            "small_positive_costs",
            [
                c("P01", 1, "Minor process rework", 20_000, 45_000, 90_000, "small_positive_costs"),
                c("P01", 2, "Temporary contractor cover", 15_000, 35_000, 80_000, "small_positive_costs"),
                c("P01", 3, "Additional testing", 10_000, 28_000, 65_000, "small_positive_costs"),
            ],
        )
    )

    scenarios.append(
        ParentScenario(
            "P02",
            "Large positive costs",
            "large_positive_costs_tens_of_millions",
            [
                c("P02", 1, "Capital works delay", 8_000_000, 14_000_000, 26_000_000, "large_positive_costs_tens_of_millions"),
                c("P02", 2, "Vendor remediation", 3_500_000, 7_500_000, 18_000_000, "large_positive_costs_tens_of_millions"),
                c("P02", 3, "Programme extension", 5_000_000, 11_000_000, 22_000_000, "large_positive_costs_tens_of_millions"),
            ],
        )
    )

    scenarios.append(
        ParentScenario(
            "P03",
            "Negative benefits only",
            "negative_benefits_only",
            [
                c("P03", 1, "Reduced support burden", -1_200_000, -650_000, -150_000, "negative_benefits_only"),
                c("P03", 2, "Avoided licence cost", -900_000, -420_000, -80_000, "negative_benefits_only"),
                c("P03", 3, "Efficiency benefit", -700_000, -300_000, -50_000, "negative_benefits_only"),
                c("P03", 4, "Reduced external spend", -1_600_000, -850_000, -200_000, "negative_benefits_only"),
            ],
        )
    )

    scenarios.append(
        ParentScenario(
            "P04",
            "Mixed benefits and costs crossing zero",
            "mixed_crossing_zero",
            [
                c("P04", 1, "Operational benefit", -1_500_000, -400_000, 150_000, "mixed_crossing_zero"),
                c("P04", 2, "Transition cost", -100_000, 450_000, 1_900_000, "mixed_crossing_zero"),
                c("P04", 3, "Productivity effect", -800_000, 50_000, 900_000, "mixed_crossing_zero"),
                c("P04", 4, "Compliance rework", 100_000, 650_000, 2_300_000, "mixed_crossing_zero"),
            ],
        )
    )

    scenarios.append(
        ParentScenario(
            "P05",
            "Strong right-tail parent outcome",
            "strong_right_tail",
            [
                c("P05", 1, "Low likely high exposure A", 100_000, 180_000, 5_000_000, "strong_right_tail"),
                c("P05", 2, "Low likely high exposure B", 50_000, 120_000, 3_800_000, "strong_right_tail"),
                c("P05", 3, "Low likely high exposure C", 80_000, 150_000, 4_500_000, "strong_right_tail"),
                c("P05", 4, "Low likely high exposure D", 120_000, 220_000, 6_000_000, "strong_right_tail"),
                c("P05", 5, "Low likely high exposure E", 40_000, 95_000, 2_900_000, "strong_right_tail"),
            ],
        )
    )

    scenarios.append(
        ParentScenario(
            "P06",
            "Strong left-tail parent outcome",
            "strong_left_tail",
            [
                c("P06", 1, "High likely downside benefit A", -5_000_000, -180_000, -100_000, "strong_left_tail"),
                c("P06", 2, "High likely downside benefit B", -3_500_000, -140_000, -45_000, "strong_left_tail"),
                c("P06", 3, "High likely downside benefit C", -4_200_000, -210_000, -90_000, "strong_left_tail"),
                c("P06", 4, "High likely downside benefit D", -2_800_000, -115_000, -35_000, "strong_left_tail"),
                c("P06", 5, "High likely downside benefit E", -6_000_000, -260_000, -100_000, "strong_left_tail"),
            ],
        )
    )

    scenarios.append(
        ParentScenario(
            "P07",
            "Narrow low-variance estimates",
            "narrow_low_variance",
            [
                c("P07", 1, "Narrow estimate A", 950_000, 1_000_000, 1_080_000, "narrow_low_variance"),
                c("P07", 2, "Narrow estimate B", 420_000, 450_000, 500_000, "narrow_low_variance"),
                c("P07", 3, "Narrow estimate C", 1_900_000, 2_000_000, 2_120_000, "narrow_low_variance"),
                c("P07", 4, "Narrow estimate D", -550_000, -500_000, -450_000, "narrow_low_variance"),
                c("P07", 5, "Narrow estimate E", 700_000, 735_000, 780_000, "narrow_low_variance"),
                c("P07", 6, "Narrow estimate F", 250_000, 280_000, 320_000, "narrow_low_variance"),
            ],
        )
    )

    scenarios.append(
        ParentScenario(
            "P08",
            "Wide high-variance estimates",
            "wide_high_variance",
            [
                c("P08", 1, "Wide estimate A", -2_000_000, 1_000_000, 9_000_000, "wide_high_variance"),
                c("P08", 2, "Wide estimate B", 500_000, 2_500_000, 14_000_000, "wide_high_variance"),
                c("P08", 3, "Wide estimate C", -4_500_000, -500_000, 3_500_000, "wide_high_variance"),
                c("P08", 4, "Wide estimate D", 100_000, 4_000_000, 18_000_000, "wide_high_variance"),
                c("P08", 5, "Wide estimate E", -1_000_000, 750_000, 7_500_000, "wide_high_variance"),
                c("P08", 6, "Wide estimate F", 2_000_000, 8_000_000, 24_000_000, "wide_high_variance"),
            ],
        )
    )

    scenarios.append(
        ParentScenario(
            "P09",
            "Internally conflicting child estimates",
            "internally_conflicting_children",
            [
                c("P09", 1, "Right-skewed child A", 100_000, 180_000, 4_000_000, "internally_conflicting_children"),
                c("P09", 2, "Left-skewed child B", -3_500_000, -220_000, -80_000, "internally_conflicting_children"),
                c("P09", 3, "Balanced child C", 400_000, 900_000, 1_600_000, "internally_conflicting_children"),
                c("P09", 4, "Large cost child D", 2_500_000, 5_500_000, 11_000_000, "internally_conflicting_children"),
                c("P09", 5, "Large benefit child E", -9_000_000, -4_500_000, -1_000_000, "internally_conflicting_children"),
                c("P09", 6, "Zero-crossing child F", -750_000, 100_000, 1_400_000, "internally_conflicting_children"),
                c("P09", 7, "Small cost child G", 30_000, 70_000, 200_000, "internally_conflicting_children"),
            ],
        )
    )

    scenarios.append(
        ParentScenario(
            "P10",
            "Many small children",
            "many_small_children",
            [
                c("P10", 1, "Small child A", 5_000, 18_000, 60_000, "many_small_children"),
                c("P10", 2, "Small child B", 8_000, 22_000, 75_000, "many_small_children"),
                c("P10", 3, "Small child C", -20_000, 5_000, 40_000, "many_small_children"),
                c("P10", 4, "Small child D", 12_000, 30_000, 95_000, "many_small_children"),
                c("P10", 5, "Small child E", 3_000, 15_000, 55_000, "many_small_children"),
                c("P10", 6, "Small child F", -15_000, 8_000, 45_000, "many_small_children"),
                c("P10", 7, "Small child G", 10_000, 25_000, 85_000, "many_small_children"),
                c("P10", 8, "Small child H", 6_000, 19_000, 70_000, "many_small_children"),
            ],
        )
    )

    scenarios.append(
        ParentScenario(
            "P11",
            "Few large children",
            "few_large_children",
            [
                c("P11", 1, "Strategic dependency A", 12_000_000, 22_000_000, 45_000_000, "few_large_children"),
                c("P11", 2, "Strategic dependency B", -8_000_000, 3_000_000, 30_000_000, "few_large_children"),
            ],
        )
    )

    scenarios.append(
        ParentScenario(
            "P12",
            "Mixed scale children",
            "mixed_scale_thousands_to_millions",
            [
                c("P12", 1, "Tiny admin effect", 1_000, 5_000, 20_000, "mixed_scale_thousands_to_millions"),
                c("P12", 2, "Small training cost", 25_000, 60_000, 150_000, "mixed_scale_thousands_to_millions"),
                c("P12", 3, "Medium integration cost", 250_000, 750_000, 2_000_000, "mixed_scale_thousands_to_millions"),
                c("P12", 4, "Large vendor exposure", 2_000_000, 6_000_000, 16_000_000, "mixed_scale_thousands_to_millions"),
                c("P12", 5, "Benefit offset", -3_000_000, -1_200_000, -200_000, "mixed_scale_thousands_to_millions"),
                c("P12", 6, "Zero-crossing adoption effect", -500_000, 100_000, 900_000, "mixed_scale_thousands_to_millions"),
                c("P12", 7, "Minor recurring cost", 10_000, 30_000, 90_000, "mixed_scale_thousands_to_millions"),
                c("P12", 8, "Programme delay", 800_000, 2_100_000, 5_500_000, "mixed_scale_thousands_to_millions"),
                c("P12", 9, "Avoided remediation", -1_500_000, -600_000, -100_000, "mixed_scale_thousands_to_millions"),
                c("P12", 10, "Contingency drawdown", 100_000, 500_000, 2_500_000, "mixed_scale_thousands_to_millions"),
            ],
        )
    )

    return scenarios


def invalid_child_cases() -> List[InvalidChildCase]:
    return [
        InvalidChildCase("IC001", "likely_equals_min", 100_000, 100_000, 300_000, "reject", "LIKELY_EQUALS_MIN", "likely must be strictly greater than min"),
        InvalidChildCase("IC002", "likely_equals_max", 100_000, 300_000, 300_000, "reject", "LIKELY_EQUALS_MAX", "likely must be strictly less than max"),
        InvalidChildCase("IC003", "min_equals_max", 250_000, 250_000, 250_000, "reject", "MIN_EQUALS_MAX", "max must be greater than min"),
        InvalidChildCase("IC004", "max_less_than_min", 500_000, 450_000, 300_000, "reject", "MAX_LESS_THAN_MIN", "max must be greater than min"),
        InvalidChildCase("IC005", "likely_less_than_min", 100_000, 50_000, 300_000, "reject", "LIKELY_LESS_THAN_MIN", "likely must be greater than min"),
        InvalidChildCase("IC006", "likely_greater_than_max", 100_000, 450_000, 300_000, "reject", "LIKELY_GREATER_THAN_MAX", "likely must be less than max"),
        InvalidChildCase("IC007", "missing_min", None, 150_000, 300_000, "reject", "MISSING_MIN", "min value is missing"),
        InvalidChildCase("IC008", "missing_likely", 100_000, None, 300_000, "reject", "MISSING_LIKELY", "likely value is missing"),
        InvalidChildCase("IC009", "missing_max", 100_000, 150_000, None, "reject", "MISSING_MAX", "max value is missing"),
        InvalidChildCase("IC010", "non_numeric_min", "not-a-number", 150_000, 300_000, "reject", "NON_NUMERIC_MIN", "min value is not numeric"),
        InvalidChildCase("IC011", "non_numeric_likely", 100_000, "not-a-number", 300_000, "reject", "NON_NUMERIC_LIKELY", "likely value is not numeric"),
        InvalidChildCase("IC012", "non_numeric_max", 100_000, 150_000, "not-a-number", "reject", "NON_NUMERIC_MAX", "max value is not numeric"),
    ]


def dataframe_records(df: pd.DataFrame) -> List[Dict[str, Any]]:
    """Convert a dataframe to JSON-safe records."""
    clean = df.replace({np.nan: None})
    return clean.to_dict(orient="records")


def write_outputs(
    output_dir: Path,
    settings: Dict[str, Any],
    parents: Sequence[ParentScenario],
    child_df: pd.DataFrame,
    pert_df: pd.DataFrame,
    expectations_df: pd.DataFrame,
    invalid_df: pd.DataFrame,
) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)

    child_df.to_csv(output_dir / "child_estimates.csv", index=False)
    pert_df.to_csv(output_dir / "child_pert_parameters.csv", index=False)
    expectations_df.to_csv(output_dir / "parent_quantile_expectations.csv", index=False)
    invalid_df.to_csv(output_dir / "invalid_child_cases.csv", index=False)
    pd.DataFrame([settings]).to_csv(output_dir / "generation_settings.csv", index=False)

    json_payload = {
        "settings": settings,
        "valid_parent_sets": [
            {
                "parent_id": parent.parent_id,
                "parent_name": parent.parent_name,
                "scenario": parent.scenario,
                "children": [asdict(child) for child in parent.children],
                "expected_quantiles": dataframe_records(expectations_df[expectations_df["parent_id"] == parent.parent_id]),
            }
            for parent in parents
        ],
        "invalid_child_cases": dataframe_records(invalid_df),
    }

    with (output_dir / "test_sets.json").open("w", encoding="utf-8") as f:
        json.dump(json_payload, f, indent=2, ensure_ascii=False)

    with (output_dir / "generation_settings.json").open("w", encoding="utf-8") as f:
        json.dump(settings, f, indent=2, ensure_ascii=False)


def run_generation(args: argparse.Namespace) -> None:
    if args.mode == "quick":
        reference_runs = QUICK_REFERENCE_RUNS
    elif args.mode == "audit":
        reference_runs = AUDIT_REFERENCE_RUNS
    else:
        raise ValueError(f"Unsupported mode: {args.mode}")

    if args.reference_runs is not None:
        reference_runs = args.reference_runs

    parents = valid_parent_scenarios()
    invalid_cases = invalid_child_cases()

    child_df = build_child_estimate_table(parents)
    pert_df = build_pert_parameter_table(parents, args.lambda_value)
    invalid_df = build_invalid_child_table(invalid_cases)
    expectations_df = build_parent_expectations(
        parents=parents,
        lambda_value=args.lambda_value,
        samples_per_child=args.samples_per_child,
        reference_runs=reference_runs,
        master_seed=args.master_seed,
    )

    valid_child_count = sum(len(parent.children) for parent in parents)
    settings = {
        "generator_version": "1.0.0",
        "mode": args.mode,
        "master_seed": args.master_seed,
        "rng_algorithm": "Mersenne Twister via numpy.random.RandomState / MT19937",
        "sample_parity_required": False,
        "statistical_equivalence_required": True,
        "samples_per_child": args.samples_per_child,
        "reference_runs": reference_runs,
        "lambda_value": args.lambda_value,
        "valid_child_rule": "min < likely < max",
        "pert_alpha_formula": "1 + lambda_value * (likely - min) / (max - min)",
        "pert_beta_formula": "1 + lambda_value * (max - likely) / (max - min)",
        "scaling_formula": "scaled_sample = min + beta_sample * (max - min)",
        "parent_aggregation": "trial-wise summation of child sample arrays",
        "percentile_convention": "literal statistical percentiles: P10=10th, P50=50th, P90=90th",
        "expected_value_rule": "median of repeated reference-run quantile estimates",
        "tolerance_lower_percentile": TOLERANCE_LOWER_PERCENTILE,
        "tolerance_upper_percentile": TOLERANCE_UPPER_PERCENTILE,
        "valid_parent_set_count": len(parents),
        "valid_child_count": valid_child_count,
        "invalid_child_case_count": len(invalid_cases),
    }

    write_outputs(
        output_dir=Path(args.output_dir),
        settings=settings,
        parents=parents,
        child_df=child_df,
        pert_df=pert_df,
        expectations_df=expectations_df,
        invalid_df=invalid_df,
    )

    print(f"Generated fixture outputs in: {Path(args.output_dir).resolve()}")
    print(f"Mode: {args.mode}; reference_runs: {reference_runs}; samples_per_child: {args.samples_per_child}")
    print(f"Valid parent sets: {len(parents)}; valid children: {valid_child_count}; invalid child cases: {len(invalid_cases)}")


def parse_args(argv: Optional[Sequence[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate beta-PERT Monte Carlo parent-risk impact test fixtures."
    )
    parser.add_argument(
        "--mode",
        choices=("quick", "audit"),
        default="quick",
        help="quick uses 100 repeated reference runs; audit uses 10,000 repeated reference runs. Default: quick.",
    )
    parser.add_argument(
        "--output-dir",
        default="generated",
        help="Directory for generated JSON and CSV outputs. Default: generated.",
    )
    parser.add_argument(
        "--lambda-value",
        type=float,
        default=DEFAULT_LAMBDA_VALUE,
        help="Modified beta-PERT lambda value. Default: 4.0.",
    )
    parser.add_argument(
        "--samples-per-child",
        type=int,
        default=SAMPLES_PER_CHILD,
        help="Monte Carlo samples per child per reference run. Default: 10000.",
    )
    parser.add_argument(
        "--master-seed",
        type=int,
        default=MASTER_SEED,
        help=f"Master seed used to derive deterministic child/run seeds. Default: {MASTER_SEED}.",
    )
    parser.add_argument(
        "--reference-runs",
        type=int,
        default=None,
        help="Override repeated reference runs, mainly for smoke testing. If omitted, mode controls the count.",
    )
    return parser.parse_args(argv)


def main(argv: Optional[Sequence[str]] = None) -> None:
    args = parse_args(argv)
    if args.lambda_value <= 0:
        raise ValueError("lambda_value must be positive")
    if args.samples_per_child <= 0:
        raise ValueError("samples_per_child must be positive")
    if args.reference_runs is not None and args.reference_runs <= 0:
        raise ValueError("reference_runs must be positive")
    run_generation(args)


if __name__ == "__main__":
    main()
