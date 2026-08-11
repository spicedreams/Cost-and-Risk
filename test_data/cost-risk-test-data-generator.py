#!/usr/bin/env python3
"""
Risk PERT Tree Fixture Generator
Implements recursive Monte Carlo aggregation with trial-wise consensus 
and additive roll-up logic. Rounds all quantiles to nearest integer.
"""

from __future__ import annotations
import argparse
import hashlib
import os
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import numpy as np
import pandas as pd

# Constants per specification [Source 3, 51, Turn 12]
DEFAULT_LAMBDA_VALUE = 4.0
SAMPLES_PER_CHILD = 10_000
QUICK_REFERENCE_RUNS = 100
TARGET_QUANTILES = (10, 50, 90)

@dataclass
class Node:
    name: str
    node_type: str  # Root, Aggregate, Estimator, invalid_Estimator
    estimate_type: str  # Cost, Benefit, Risk, Issue, Treatment, Residual
    opt: Optional[float] = None
    likely: Optional[float] = None
    pess: Optional[float] = None
    prob: Optional[float] = None
    children: List[Node] = field(default_factory=list)
    error_code: Optional[str] = None
    error_reason: Optional[str] = None
    
    # Storage for calculated empirical distribution and quantiles
    mc_array: Optional[np.ndarray] = None
    quantiles: Dict[str, float] = field(default_factory=dict)

def stable_seed(*parts: Any) -> int:
    """Return a deterministic 32-bit seed from identifying parts [Source 4]."""
    text = "|".join(str(part) for part in parts)
    digest = hashlib.sha256(text.encode("utf-8")).digest()
    return int.from_bytes(digest[:8], byteorder="big", signed=False) % (2**32 - 1)

def validate_estimator(node: Node) -> Tuple[bool, str, str]:
    """Validate leaf-level estimates [Source 5, 12, Turn 18]."""
    try:
        if any(v is None for v in [node.opt, node.likely, node.pess, node.prob]):
            return False, "MISSING_VALUES", "One or more input values are missing"
        o, l, p = float(node.opt), float(node.likely), float(node.pess)
        if not (o < l < p):
            if o >= p: return False, "MAX_LESS_THAN_MIN", "max must be greater than min"
            if l <= o: return False, "LIKELY_LESS_THAN_MIN", "likely must be greater than min"
            if l >= p: return False, "LIKELY_GREATER_THAN_MAX", "likely must be less than max"
        return True, "", ""
    except (ValueError, TypeError):
        return False, "NON_NUMERIC_INPUT", "Input values must be numeric"

def simulate_estimator(node: Node, master_seed: int) -> np.ndarray:
    """Generate 10k sample exposure array for a leaf node [Turn 12]."""
    is_valid, code, reason = validate_estimator(node)
    if not is_valid:
        node.node_type = "invalid_Estimator"
        node.error_code = code
        node.error_reason = reason
        return np.zeros(SAMPLES_PER_CHILD)

    seed = stable_seed(master_seed, node.name, "estimator")
    rng = np.random.RandomState(seed)
    
    # Severity (PERT)
    span = node.pess - node.opt
    alpha = 1.0 + DEFAULT_LAMBDA_VALUE * (node.likely - node.opt) / span
    beta = 1.0 + DEFAULT_LAMBDA_VALUE * (node.pess - node.likely) / span
    severity = node.opt + rng.beta(alpha, beta, size=SAMPLES_PER_CHILD) * span
    
    # Occurrence (Binomial)
    occurrence = rng.binomial(1, node.prob, size=SAMPLES_PER_CHILD)
    
    # Total Exposure
    exposure = severity * occurrence
    if node.estimate_type == "Benefit":
        exposure *= -1
        
    return exposure

def run_mc_recursive(node: Node, master_seed: int):
    """Recursively calculate distributions across the tree [Turn 16]."""
    
    if node.node_type in ["Estimator", "invalid_Estimator"]:
        node.mc_array = simulate_estimator(node, master_seed)
        return node.mc_array

    estimator_arrays = []
    aggregate_arrays = []
    
    for child in node.children:
        child_array = run_mc_recursive(child, master_seed)
        
        # Validation exclusions [Turn 18]
        if child.node_type == "invalid_Estimator":
            continue
        
        # Treatment logic: Total exposure calculated but not passed up [Turn 1, 16]
        if child.estimate_type == "Treatment" and node.estimate_type in ["Risk", "Issue"]:
            continue
            
        if child.node_type == "Estimator":
            estimator_arrays.append(child_array)
        else:
            aggregate_arrays.append(child_array)

    # Mixed Aggregation: Average Estimators + Sum Aggregates [Turn 16]
    final_array = np.zeros(SAMPLES_PER_CHILD)
    if estimator_arrays:
        final_array += np.mean(estimator_arrays, axis=0)
    if aggregate_arrays:
        final_array += np.sum(aggregate_arrays, axis=0)
        
    node.mc_array = final_array
    
    # Calculate quantiles rounded to integer [User Query]
    def q_round(val: float) -> int:
        return int(round(val))

    node.quantiles = {
        "p10_expected": q_round(np.percentile(final_array, 10)),
        "p50_expected": q_round(np.percentile(final_array, 50)),
        "p90_expected": q_round(np.percentile(final_array, 90)),
        # Placeholder tolerance for fixture clarity
        "p10_lower": q_round(np.percentile(final_array, 10) * 0.98),
        "p10_upper": q_round(np.percentile(final_array, 10) * 1.02),
        "p50_lower": q_round(np.percentile(final_array, 50) * 0.98),
        "p50_upper": q_round(np.percentile(final_array, 50) * 1.02),
        "p90_lower": q_round(np.percentile(final_array, 90) * 0.98),
        "p90_upper": q_round(np.percentile(final_array, 90) * 1.02),
    }
    
    return node.mc_array

def flatten_tree(node: Node, depth: int = 1) -> List[Dict]:
    """Convert tree to preorder traversal for CSV [Turn 18]."""
    row = {
        "node_type": node.node_type,
        "estimate_type": node.estimate_type,
        "opt": node.opt if node.node_type == "Estimator" else "",
        "likely": node.likely if node.node_type == "Estimator" else "",
        "pess": node.pess if node.node_type == "Estimator" else "",
        "prob": node.prob if node.node_type == "Estimator" else "",
    }
    for q in ["p10_lower", "p10_expected", "p10_upper", "p50_lower", "p50_expected", "p50_upper", "p90_lower", "p90_expected", "p90_upper"]:
        row[q] = node.quantiles.get(q, "")
        
    row["expected_error_code"] = node.error_code or ""
    row["expected_error_reason"] = node.error_reason or ""
    row[f"level_{depth}"] = node.name
    
    rows = [row]
    for child in node.children:
        rows.extend(flatten_tree(child, depth + 1))
    return rows

def build_representative_tree() -> Node:
    """Comprehensive test tree exercising all agreed features [Turn 1, 14, 16, 20]."""
    root = Node("Strategic Portfolio", "Root", "Mixed")

    # 1. Operational Delivery (Risk with Treatment)
    op_risk = Node("Delivery Failure", "Aggregate", "Risk")
    supplier_risk = Node("Key Supplier Insolvency", "Aggregate", "Risk")
    supplier_risk.children = [
        Node("Estimator A", "Estimator", "Risk", 100000, 500000, 2000000, 0.15),
        Node("Estimator B", "Estimator", "Risk", 80000, 450000, 1800000, 0.12)
    ]
    # Treatment for supplier risk (Does not aggregate to op_risk)
    backup_treatment = Node("Alternative Supplier Framework", "Aggregate", "Treatment")
    backup_treatment.children = [
        Node("Setup Fees", "Aggregate", "Cost", children=[
            Node("Vendor Audit", "Estimator", "Cost", 10000, 15000, 25000, 1.0),
            Node("Legal Review", "Estimator", "Cost", 5000, 8000, 15000, 1.0)
        ]),
        Node("Residual Slippage", "Estimator", "Residual", 50000, 100000, 300000, 0.05)
    ]
    op_risk.children = [supplier_risk, backup_treatment]

    # 2. Legacy System (Known Issue)
    legacy_issue = Node("Database Migration Issue", "Aggregate", "Issue")
    legacy_issue.children = [
        Node("Expert 1", "Estimator", "Issue", 250000, 350000, 500000, 1.0),
        Node("Expert 2", "Estimator", "Issue", 220000, 380000, 600000, 1.0)
    ]

    # 3. IT Transformation (Cost/Benefit structure)
    it_trans = Node("Digital Transformation", "Aggregate", "Mixed")
    hw_costs = Node("Infrastructure Hardware", "Aggregate", "Cost")
    hw_costs.children = [
        Node("Vendor Quote 1", "Estimator", "Cost", 800000, 1000000, 1500000, 1.0),
        Node("Vendor Quote 2", "Estimator", "Cost", 900000, 1100000, 1400000, 1.0)
    ]
    efficiency = Node("Efficiency Savings", "Aggregate", "Benefit")
    efficiency.children = [
        Node("HR Estimate", "Estimator", "Benefit", -500000, -300000, -100000, 1.0),
        Node("Ops Estimate", "Estimator", "Benefit", -600000, -400000, -200000, 1.0)
    ]
    it_trans.children = [hw_costs, efficiency]

    # 4. Invalid Cases
    invalid_node = Node("Broken Input", "Estimator", "Cost", 5000, 4000, 10000, 1.0)

    root.children = [op_risk, legacy_issue, it_trans, invalid_node]
    return root

def main():
    # Prompt for seed [Turn 19]
    try:
        seed_input = input("Enter Master Seed (integer): ")
        master_seed = int(seed_input)
    except ValueError:
        master_seed = 20260810
        print(f"Invalid input. Defaulting to {master_seed}")

    # Simulate
    tree = build_representative_tree()
    run_mc_recursive(tree, master_seed)
    
    # Export
    flat_data = flatten_tree(tree)
    df = pd.DataFrame(flat_data)
    
    # Order columns [Turn 18]
    level_cols = sorted([c for c in df.columns if c.startswith("level_")])
    cols = ["node_type", "estimate_type", "opt", "likely", "pess", "prob",
            "p10_lower", "p10_expected", "p10_upper", "p50_lower", "p50_expected", "p50_upper", "p90_lower", "p90_expected", "p90_upper",
            "expected_error_code", "expected_error_reason"] + level_cols
    df = df[cols]

    # Row 1 Master Seed [Turn 19]
    output_file = "fixture_v2.csv"
    with open(output_file, "w") as f:
        f.write(f"Master Seed,{master_seed}" + "," * (len(cols) - 2) + "\n")
        df.to_csv(f, index=False)
    
    print(f"Fixture generated: {output_file}")

if __name__ == "__main__":
    main()