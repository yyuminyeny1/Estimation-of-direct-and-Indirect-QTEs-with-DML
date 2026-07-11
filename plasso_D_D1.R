## Model D|X
#ind_const<-which_are_constant(xDs, verbose = FALSE)
#sel_X[ind_const]<-FALSE
data_D<-data.frame(D = Ds, xDs[,sel_X, drop = FALSE])      # data_D[-c(ind_st:ind_end),]
new_data_D<-data.frame(D = Dp, xDp[,sel_X, drop = FALSE])  # data_D[c(ind_st:ind_end),]

if(sum(sel_X)!=0){
  
  mod_D<-glm(D ~ ., data = data_D, 
             family = binomial(link = "logit"))
  pD<-predict(mod_D, newdata = new_data_D, 
              type ="response")
  
}else{
  
  ## Only the intercept term
  mod_D<-glm(Ds ~ 1, family = binomial(link = "logit"))
  pD<-rep(predict(mod_D, type = "response")[1], length(Dp))
  #pD<-predict(mod_D, newdata = NULL, 
  #            type = "response")[c(ind_st:ind_end)]
  
}

pD[pD<=0.02]<-0.02
pD[pD>=0.98]<-0.98
## observed, sample pD
pD1<-pD
pD0<-1 - pD
pDi<-Dp*pD1 + (1 - Dp)*pD0

## -----------------------------------------------------------------------
## Model D1|M,X
data_D1<-data.frame(D = Ds, M = Ms, xDs[, sel_X, drop = FALSE])       #data_M[-c(ind_st:ind_end),]
new_data_D1<-data.frame(D = Dp, M = Mp, xDp[, sel_X, drop = FALSE])   #data_M[c(ind_st:ind_end),]

mod_D1<-glm(D ~ ., data = data_D1, 
            family = binomial(link = "logit"))

pD1mi<-predict(mod_D1, newdata = new_data_D1, type = "response")
pD1mi[pD1mi<=0.02]<-0.02
pD1mi[pD1mi>=0.98]<-0.98
pD0mi<-1 - pD1mi