## Regression imputation
## Model: E[P(Y<=a|d,M,X)|d',X]
## Split the subsamples W_c using the second half
## Similar as algorithm 2 in Farbmacher et al (2022)
xDs2_sel <- xDs2[, sel_X, drop = FALSE]
ind_const_X2 <- which_are_constant(xDs2_sel, verbose = FALSE)   # check X only
xDs2_sel <- as.data.frame(xDs2_sel)                             # undo any data.table conversion
if(length(ind_const_X2) > 0){
  xDs2_sel <- xDs2_sel[, -ind_const_X2, drop = FALSE]
}

data_py1mi<-data.frame(y = py1mi_hat, D = Ds2, xDs2_sel)
data_py0mi<-data.frame(y = py0mi_hat, D = Ds2, xDs2_sel)
data_YDX<-data.frame(y = indYs, D = Ds, xDs_sel)

new_data_DX<-data.frame(D = Dp, xDp[ ,sel_X, drop = FALSE])
new_data_py1mi<-new_data_DX
new_data_py0mi<-new_data_DX
new_data_py1mi[,"D"]<-1
new_data_py0mi[,"D"]<-0

## Estimating model for E[P(Y<=a|1,M,X)|d',X]
mod_py1mi <- lm(y ~ ., data = data_py1mi)
## Estimating model for E[P(Y<=a|0,M,X)|d',X]
mod_py0mi <- lm(y ~ ., data = data_py0mi)
## Estimating model for P(Y<=a|D,X) with data_YDX
mod_YDX<-glm(y ~., data = data_YDX, family = binomial(link = "logit"))

## Estimating E[P(Y<=a|1,M,X)|1,X] = P(Y<=a|1,X)
g11a_x<-as.numeric(predict(mod_YDX, newdata = new_data_py1mi, type ="response"))
## Estimating E[P(Y<=a|1,M,X)|0,X]
g10a_x <- as.numeric(predict(mod_py1mi, newdata = new_data_py0mi))
g10a_x<-pmin(pmax(g10a_x, 0), 1)
## Estimating E[P(Y<=a|0,M,X)|0,X] = P(Y<=a|0,X)
g00a_x <- as.numeric(predict(mod_YDX, newdata = new_data_py0mi, type = "response"))
## Estimating E[P(Y<=a|0,M,X)|1,X]
g01a_x <- as.numeric(predict(mod_py0mi, newdata = new_data_py1mi))
g01a_x<-pmin(pmax(g01a_x, 0), 1)