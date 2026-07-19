## DML estimation, five fold cross-fit
## with the triple robust estimator

k_fold<-5

## matrices for storing results
px11<-matrix(0, k_fold, length(a))
px10<-matrix(0, k_fold, length(a))
px00<-matrix(0, k_fold, length(a))
px01<-matrix(0, k_fold, length(a))

psi_11<-NULL
psi_10<-NULL
psi_00<-NULL
psi_01<-NULL

if_11<-NULL
if_10<-NULL
if_00<-NULL
if_01<-NULL

ind_pred<-k_fold_ind(n = length(Y), k_fold = k_fold)

## Data for model selection
## Need to be matrix
xD<-as.matrix(x)
colnames(xD)<-paste0("X", 1:ncol(xD))
xD1<-as.matrix(cbind(M, xD))                # inherits xD's names automatically, plus "M"
xY<-as.matrix(cbind(D, M, M*D, xD))         # same — will need explicit names for the D/M/MD columns too if unnamed

for(s in 1:nrow(ind_pred)){
  
  ind_st<-ind_pred$ind_st[s]
  ind_end<-ind_pred$ind_end[s]
  
  Ys<-Y[-c(ind_st:ind_end)]
  Yp<-Y[c(ind_st:ind_end)]
  
  Ms<-M[-c(ind_st:ind_end)]
  Mp<-M[c(ind_st:ind_end)]
  
  Ds<-D[-c(ind_st:ind_end)]
  Dp<-D[c(ind_st:ind_end)]
  
  xDs<-xD[-c(ind_st:ind_end),]
  xDp<-xD[c(ind_st:ind_end),]
  
  xD1s<-xD1[-c(ind_st:ind_end),]
  xD1p<-xD1[c(ind_st:ind_end),]
  
  xYs<-xY[-c(ind_st:ind_end),]
  xYp<-xY[c(ind_st:ind_end),]
  
  source("./plasso_model_selection_D_D1.R")
  
  ## Dp and Mp
  ind_d1<-as.numeric(Dp==1)
  ind_d0<-as.numeric(Dp==0)
  
  for(i in 1:length(a)){
    
    ## Fit P(Y<=a|d,M,X)
    indYs<-as.numeric(Ys<=a[i])
    mod_indYs<-rlassologit(x = xYs, y = indYs)
    
    sel_indYs<-!as.vector(mod_indYs$beta==0)
    sel_X<-sel_indYs[-c(1:3)]|sel_X0

    ## fit post lasso regressions for D and D1
    source("./plasso_D_D1.R")
    
    ## Fit P[Y<=a|d,M,X] on (D, X) for 
    ## estimating E[p(Y<=a|d,M,X)|d',X]
    source("./qte_plasso_input_regression_imputation.R")
    
    ## Regression imputation
    source("./qte_plasso_regression_imputation.R")
    
    ## 1{Yp<=a}
    indYp<-as.numeric(Yp<=a[i])
    
    ## Y1_M1, triply robust
    tr_11<-ind_d1/pD1*(indYp - g11a_x) + g11a_x
    px11_raw<-mean(tr_11)
    px11[s,i]<-max(min(1, px11_raw),0)
    psi11_raw<-tr_11
    
    ## Y1_M0, triply robust
    tr_10<-ind_d1/(pD0*pD1mi)*pD0mi*(indYp - py1mi)
    tr_10<-tr_10 + ind_d0/pD0*(py1mi - g10a_x) + g10a_x
    px10_raw<-mean(tr_10)
    px10[s,i]<-max(min(1, px10_raw),0)
    psi10_raw<-tr_10
    
    ## Y0_M0, triply robust
    tr_00<-ind_d0/pD0*(indYp - g00a_x) + g00a_x
    px00_raw<-mean(tr_00)
    px00[s,i]<-max(min(1, px00_raw),0)
    psi00_raw<-tr_00
    
    ## Y0_M1, triply robust
    tr_01<-ind_d0/(pD1*pD0mi)*pD1mi*(indYp - py0mi) 
    tr_01<-tr_01 + ind_d1/pD1*(py0mi - g01a_x) + g01a_x
    px01_raw<-mean(tr_01)
    px01[s,i]<-max(min(1, px01_raw),0)
    psi01_raw<-tr_01
    
    ## combine psi (IF) across a
    psi_11<-cbind(psi_11, psi11_raw)
    psi_10<-cbind(psi_10, psi10_raw)
    psi_01<-cbind(psi_01, psi01_raw)
    psi_00<-cbind(psi_00, psi00_raw)
    
    print(i)
    
  }
  
  ## combine IF across i
  if_11<-rbind(if_11, psi_11)
  if_10<-rbind(if_10, psi_10)
  if_00<-rbind(if_00, psi_00)
  if_01<-rbind(if_01, psi_01)
  
  psi_11<-NULL
  psi_10<-NULL
  psi_00<-NULL
  psi_01<-NULL
  
  
}

## Average the results, vartheta_hat
px11<-apply(px11, 2, mean, na.rm = T)
px10<-apply(px10, 2, mean, na.rm = T)
px00<-apply(px00, 2, mean, na.rm = T)
px01<-apply(px01, 2, mean, na.rm = T)

## Sort delta_hat
p11<-sort(px11)
p10<-sort(px10)
p00<-sort(px00)
p01<-sort(px01)

## find quantiles with the function approx
q10<-find_qx(px = p10, ax = a, taux = g)
q00<-find_qx(px = p00, ax = a, taux = g)
q11<-find_qx(px = p11, ax = a, taux = g)
q01<-find_qx(px = p01, ax = a, taux = g)

## QTEs
TQTE<-q11 - q00
NDQTE<-q10 - q00
NIQTE<-q11 - q10
NDQTE1<-q11 - q01
NIQTE1<-q01 - q00

## Influence functions
if_names1<-seq(200, 950, by = 1)
if_names2<-paste(0, if_names1, sep="")
if_names<-paste("p", if_names2, sep="")
colnames(if_11)<-if_names
colnames(if_10)<-if_names
colnames(if_00)<-if_names
colnames(if_01)<-if_names
