# cat-code-interpret

This repository contains a number of (very ugly) R scripts that I've written to
explore the interpretation of the intercept term in multiple linear regression
models that contain different categorical predictors.

The *code* directory contains several R scripts that each contain a series of 
test cases. Each test case follows the same recipe:
1. Simulate some regression data
1. Estimate the (true) regression model
1. Compare the estimated intercept to its theoretical equivalent computed from
   the raw data
1. Compare the estimated intercept to its theoretical equivalent computed via
   the **emmeans** package
   
The test cases represent a nearly complete crossing of the following design
factors:
1. Type of Code: {Dummy Code, Unweighted Effects Code, Weighted Effects Code}
1. Correlation between Predictors: {Independent Predictors, Correlated
   Predictors}
1. Error Variance in the Outcome: {Deterministic Outcome, Noisy Outcome}
1. Balance of Group Sizes: {Balanced Groups, Unbalanced Groups}

Regardless of the setup, the estimated intercept always matches the respective
marginal mean computed via the **emmeans** package. The estimated intercepts
sometime match, and sometimes do not match, their respective marginal means
computed from the raw data. A summary of the concordance between the intercept
estimates and the raw data-based mean estimates is available in this [Google
Sheet](https://docs.google.com/spreadsheets/d/1mckS-lc754z989uuLWjGnCaFrwU3FB2fpNT4mLNtZUU/edit#gid=276481204).
