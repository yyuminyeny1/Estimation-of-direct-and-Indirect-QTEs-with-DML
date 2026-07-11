## Covariate selection with the lasso 
## Model selection for D
mod_D<-rlassologit(x = xDs, y = Ds)

sel_D<-!as.vector(mod_D$beta==0)

## Model selection for D1
mod_D1<-rlassologit(x = xD1s, y = Ds)
sel_D1<-!as.vector(mod_D1$beta==0)

## selection results for X on D and D1
sel_X0<-sel_D1[-1]|sel_D