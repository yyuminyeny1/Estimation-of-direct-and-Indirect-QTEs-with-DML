# Estimation-of-direct-and-Indirect-QTEs-with-DML
The R scripts for implementing the DML estimations of the natural direct, indirect, and total QTEs in the empirical application of the paper "Estimation of Direct and Indirect Quantile Treatment Effects with Double Machine Learning" (Hsu et al., 2026, JBES)
The folder also includes the data used in the empirical application (raw data: JCquantile.Rdata; cleaned data: JC_data.csv). The folder also includes the data used in the empirical application (raw data: JCquantile.Rdata; cleaned data: JC_data.csv).
A brief description of the files is provided below. The folder contains the following seven R scripts:
1. emp_main_med_qte.R: The main script. It includes the code for cleaning the data, estimating the QTEs and ATEs, implementing the multiplier bootstrap procedure, and plotting the estimated QTEs. Before running this script, please use the setwd() function to specify the correct working directory so that the other scripts can be loaded properly.
2. function_med_qte.R: Contains the functions used throughout the estimation procedure.
3. med_qte_plasso_db_fit.R: Contains the code for conducting the DML estimation (using post-Lasso) of the cdf’s of the potential outcomes, and computing the TQTE, NDQTE, NIQTE, NDQTE′, and NIQTE′, as well as the efficient influence functions (EIFs) for the cdf’s of the potential outcomes. This script calls the following auxiliary scripts:
1. plasso_model_selection_D_D1.R: Uses Lasso to select covariates for estimating (P(D | X)) and (P(D | M,X)).
2. plasso_D_D1.R: Estimates (P(D | X)) and (P(D | M,X)) using post-Lasso.
3. qte_plasso_input_regression_imputation.R: Splits the data for implementing Algorithm 2 in Farbmacher et al. (2022).
4. qte_plasso_regression_imputation.R: Implements Algorithm 2 in Farbmacher et al. (2022).
