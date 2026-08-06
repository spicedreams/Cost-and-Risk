# Risk PERT Tree Fixture Generator: Decision Summary

This document records the agreed design decisions for the risk/cost/benefit beta-PERT Monte Carlo fixture generator. It is intended to be sufficiently complete that another AI, or a cold-start session, could consume it and arrive at essentially the same implementation position.

## Objective

Generate deterministic test data for code that:

1. Takes leaf-level `Optimistic`, `Likely`, `Pessimistic`, and where applicable `Probability` estimates.
2. Converts the three-point estimate into a modified beta-PERT distribution.
3. Runs Monte Carlo sampling.
4. Aggregates sampled child impacts into parent empirical curves.
5. Emits parent `P10`, `P50`, and `P90` values.
6. Includes invalid input cases for validation testing.

## Output Format

Primary output is now a **single practical CSV**:

```text
risk_tree_fixture.csv
```

Detailed JSON and multiple detail CSV files are optional only:

```bash
--include-detail-outputs
```

## CSV Column Order

Fixed fields precede variable tree-depth fields:

```text
node_type
estimate_type
optimistic
likely
pessimistic
probability
p10_lower
p10_expected
p10_upper
p50_lower
p50_expected
p50_upper
p90_lower
p90_expected
p90_upper
expected_result
expected_error_code
expected_error_reason
level_1
level_2
level_3
level_4
```

`level_*` columns are configurable with:

```bash
--max-tree-depth
```

Default:

```text
4
```

Only the current node name is populated in the relevant `level_*` column. The full path is **not** repeated across the row.

## Tree Structure

The old 12 flat scenarios were replaced by **4 root trees**:

1. Cost-focused programme exposure
2. Benefit-focused outcome exposure
3. Risk-focused delivery exposure
4. Mixed portfolio exposure

Rows are exported in preorder traversal.

## Node Types

Valid `node_type` values:

```text
parent
leaf
invalid_leaf
```

Valid `estimate_type` values:

```text
Cost
Benefit
Risk
Mixed
```

`Mixed` is used only for parent nodes whose descendant leaves include more than one estimate type.

## Leaf Estimate Rules

For valid leaves:

```text
optimistic < likely < pessimistic
```

This applies even where values are negative.

## Probability Rules

For `Risk` leaves:

```text
probability is required
0 <= probability <= 1
```

A `Risk` with:

```text
probability = 1
```

represents an **Issue**. It is not given a separate `Issue` estimate type.

For `Cost` and `Benefit` leaves:

```text
probability must be blank
```

## Risk Aggregation Model

Risk leaves use the unconditional risk exposure curve approach:

```text
Risk contribution = Bernoulli(probability) × beta-PERT impact sample
```

Cost and Benefit leaves always occur:

```text
Cost contribution = beta-PERT impact sample
Benefit contribution = beta-PERT impact sample
```

## Parent Aggregation Rule

Each parent aggregates **all descendant leaves**, not just immediate child leaves.

For each parent and each simulation trial:

```text
parent_sample[i] = sum(descendant_leaf_contribution[i])
```

Parent outputs are literal statistical percentiles:

```text
P10 = 10th percentile
P50 = 50th percentile
P90 = 90th percentile
```

## Modified Beta-PERT Formula

Default:

```text
lambda_value = 4.0
```

Formula:

```python
alpha = 1 + lambda_value * (likely - optimistic) / (pessimistic - optimistic)
beta  = 1 + lambda_value * (pessimistic - likely) / (pessimistic - optimistic)
```

Scaling:

```python
scaled_sample = optimistic + beta_sample * (pessimistic - optimistic)
```

## Random Generation

The script uses NumPy `RandomState`, which is MT19937 / Mersenne Twister based.

The test objective is:

```text
statistical equivalence within tolerance
```

not exact sample-for-sample parity.

## Tolerance Generation

Quick mode:

```text
100 repeated reference runs
```

Audit mode:

```text
10,000 repeated reference runs
```

Expected value:

```text
median of repeated reference-run quantile estimates
```

Tolerance bounds:

```text
0.5th percentile and 99.5th percentile of repeated quantile estimates
```

## Invalid Cases

Invalid rows are included in the same CSV as:

```text
node_type = invalid_leaf
expected_result = reject
```

They do not participate in parent aggregation.

Covered invalid cases include:

```text
likely = optimistic
likely = pessimistic
optimistic = pessimistic
pessimistic < optimistic
likely < optimistic
likely > pessimistic
missing optimistic / likely / pessimistic
non-numeric optimistic / likely / pessimistic
missing Risk probability
Risk probability < 0
Risk probability > 1
non-numeric Risk probability
Cost probability populated
Benefit probability populated
```

## Runtime Dependencies

The script includes runtime bootstrap installation for:

```text
numpy
pandas
```

It will attempt to install them with:

```python
sys.executable -m pip install numpy pandas
```

if they are missing.

For formal audit runs, a controlled virtual environment with pinned dependency versions is still preferable.

## Implementation Notes for Future Sessions

When continuing this work, preserve these constraints:

1. Do not revert to multiple practical CSV files as the default output.
2. Do not reintroduce parent-level probability fields unless the aggregation model changes.
3. Keep `Issue` implicit as `estimate_type = Risk` and `probability = 1`.
4. Keep `Cost` and `Benefit` probability blank and always occurring.
5. Keep parent quantiles based on all descendant leaves.
6. Keep tree-level columns variable and positioned after all expected-value and validation columns.
7. Keep invalid cases in the same CSV using `node_type = invalid_leaf`.
8. Keep the default tree depth at 4 unless explicitly changed.
9. Keep expected values as medians of repeated reference-run quantile estimates.
10. Keep tolerance bounds as the 0.5th and 99.5th percentiles of repeated quantile estimates.
