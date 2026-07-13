## Model P(Y<=a|D,M,X)
## Split the subsamples W_c using the first half
## Similar as algorithm 2 in Farbmacher et al (2022)
half_size<-ceiling(dim(xDs)[1]/2)

indYs1<-indYs[c(1:half_size)]
indYs2<-indYs[-c(1:half_size)]
Ds1<-Ds[c(1:half_size)]
Ds2<-Ds[-c(1:half_size)]
Ms1<-Ms[c(1:half_size)]
Ms2<-Ms[-c(1:half_size)]
xDs1<-xDs[c(1:half_size),]
xDs2<-xDs[-c(1:half_size),]
#ind_const1<-which_are_constant(xDs1, verbose = FALSE)
#sel_X[ind_const1]<-FALSE

## reverse the order, using the second half
if(s>=4){
  
  half_size<-floor(dim(xDs)[1]/2)
  
  indYs1<-indYs[-c(1:half_size)]
  indYs2<-indYs[c(1:half_size)]
  Ds1<-Ds[-c(1:half_size)]
  Ds2<-Ds[c(1:half_size)]
  Ms1<-Ms[-c(1:half_size)]
  Ms2<-Ms[c(1:half_size)]
  xDs1<-xDs[-c(1:half_size),]
  xDs2<-xDs[c(1:half_size),]
  #ind_const1<-which_are_constant(xDs1, verbose = FALSE)
  #sel_X[ind_const1]<-FALSE
  
}

new_data_DMX <- data.frame(D = Dp, M = Mp, MD = Mp * Dp, 
                           xDp[ ,sel_X, drop = FALSE])
new_data_DMX1<-data.frame(D = Ds2, M = Ms2, MD = Ms2 * Ds2, 
                          xDs2[ ,sel_X, drop = FALSE])

###################################################################
## Model P(Y<=a|D,M,X)
data_indYs1 <- data.frame(y = indYs1, D = Ds1, M = Ms1, MD = Ms1 * Ds1, xDs1[ ,sel_X, drop = FALSE])
mod_indYs1<-glm(y ~ ., data = data_indYs1, 
                family = binomial(link = "logit"))

## Predicting counterfactual with new_data_DMX1
## Estimates for P_hat(Y<=a|d = 1,M,X)
## These are for imputation
mod_data_Y1Mi <- new_data_DMX1
mod_data_Y1Mi[,'D'] <- 1
mod_data_Y1Mi[,'MD'] <- mod_data_Y1Mi[,'D'] * mod_data_Y1Mi[,'M']
py1mi_hat <- as.numeric(predict(mod_indYs1, newdata = mod_data_Y1Mi,
                                type = "response"))

## Estimates for P(Y<=a|d = 1,M,X) with new_data_DMX
## This is for IF calculation
new_data_Y1Mi<-new_data_DMX
new_data_Y1Mi[,'D']<-1
new_data_Y1Mi[,'MD']<-new_data_Y1Mi[,'D']*new_data_Y1Mi[,'M']
py1mi<-as.numeric(predict(mod_indYs1, newdata = new_data_Y1Mi,
                           type = "response"))

## Estimates for P_hat(Y<=a|d = 0,M,X) with new_data_DMX1
## These are for imputation
mod_data_Y0Mi<-new_data_DMX1
mod_data_Y0Mi[,'D']<-0
mod_data_Y0Mi[,'MD']<-mod_data_Y0Mi[,'D'] * mod_data_Y0Mi[,'M']
py0mi_hat <- as.numeric(predict(mod_indYs1, newdata = mod_data_Y0Mi,
                                type = "response"))

## Estimates for P(Y<=a|d = 0,M,X) with new_data_DMX
## this is for IF calculation
new_data_Y0Mi<-new_data_DMX
new_data_Y0Mi[,'D']<-0
new_data_Y0Mi[,'MD']<-new_data_Y0Mi[,'D']*new_data_Y0Mi[,'M']
py0mi<-as.numeric(predict(mod_indYs1, newdata = new_data_Y0Mi,
                           type = "response"))