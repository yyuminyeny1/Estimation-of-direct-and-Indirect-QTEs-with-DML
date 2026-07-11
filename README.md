# Estimation-of-direct-and-Indirect-QTEs-with-DML

R scripts for implementing the double machine learning (DML) estimation of the natural direct, indirect, and total quantile treatment effects (QTEs) in the empirical application of the paper:

> Hsu et al. (2026). *Estimation of Direct and Indirect Quantile Treatment Effects with Double Machine Learning.* Journal of Business & Economic Statistics (JBES).

## Data

The folder includes the data used in the empirical application:

- **Raw data:** `JCquantile.Rdata`
- **Cleaned data:** `JC_data.csv`

## Files

The folder contains the following seven R scripts.

1. **`emp_main_med_qte.R`** — The main script. It includes the code for cleaning the data, estimating the QTEs and ATEs, implementing the multiplier bootstrap procedure, and plotting the estimated QTEs. Before running this script, use `setwd()` to specify the correct working directory so that the other scripts can be loaded properly.

2. **`function_med_qte.R`** — Contains the functions used throughout the estimation procedure.

3. **`med_qte_plasso_db_fit.R`** — Contains the code for conducting the DML estimation (using post-Lasso) of the CDFs of the potential outcomes, and computing the TQTE, NDQTE, NIQTE, NDQTE′, and NIQTE′, as well as the efficient influence functions (EIFs) for the CDFs of the potential outcomes. This script calls the following auxiliary scripts:
   - a. `plasso_model_selection_D_D1.R` — Uses Lasso to select covariates for estimating $P(D \mid X)$ and $P(D \mid M, X)$.
   - b. `plasso_D_D1.R` — Estimates $P(D \mid X)$ and $P(D \mid M, X)$ using post-Lasso.
   - c. `qte_plasso_input_regression_imputation.R` — Splits the data for implementing Algorithm 2 in Farbmacher et al. (2022).
   - d. `qte_plasso_regression_imputation.R` — Implements Algorithm 2 in Farbmacher et al. (2022).
