## Empirical application (HHY, 2026)
rm(list = ls(all = TRUE))
setwd("C:/emp_qte_070926/")
library(hdm)
library(causalweight)
source("function_med_qte.R")

################################################
## Clearn data
load("JCquantile.RData")
datax<-JCquantile
datax<-subset(datax, select = -c(h1, e1, e5, e6, e7, e8, e20))
datax<-datax[complete.cases(datax),]

## Remove duplicated columns
dupx<-duplicated(as.list(datax))
datax<-data.frame(datax)[!dupx]
dim(datax)

# write.table(datax, "D:/JC_data.csv", sep = ",")

Y<-datax$Yy4earn
M<-datax$My2pwork
D<-datax$treatment
x<-as.matrix(datax[,-c(1:6)])  ## delete treatment, mediator and outcome
dim(x)

n_var<-dim(x)[2]            ## number of exogenous variables
g<-seq(0.2, 0.95, by = 0.001) ## 20% - 95% by 0.1%
a<-as.numeric(quantile(Y, prob = g)) 

################################################
## Empirical application
## ATE DML estimation (Table 1, using causalweight)
medeff<-medDML(y = Y, d = D, m = M, x = x, k=5)
round(medeff$results,3)

## QTE DML estimation
source("med_qte_plasso_db_fit.R")

################################################
## Multiplier bootstrap
b<-1000            ## number of bootstrap samples
nx<-dim(if_11)[1]
Y<-datax$Yy4earn
g<-seq(0.2, 0.95, by = 0.001) ## 20% - 95% by 0.1%
a<-as.numeric(quantile(Y, prob = seq(0.2, 0.95, by = 0.001))) 

p11b<-matrix(0, b, length(g))
q11b<-matrix(0, b, length(g))
p10b<-matrix(0, b, length(g))
q10b<-matrix(0, b, length(g))
p01b<-matrix(0, b, length(g))
q01b<-matrix(0, b, length(g))
p00b<-matrix(0, b, length(g))
q00b<-matrix(0, b, length(g))

psi11<-if_11
psi10<-if_10
psi01<-if_01
psi00<-if_00

for(i in 1:b){
  
  xi<-rnorm(n=nx)
  p11b[i,]<-mboot_p(p = p11, psi = psi11, xi = xi)
  q11b[i,]<-find_qx(px = p11b[i,], ax = a, taux = g)
  
  #xi<-rnorm(n=nx)
  p10b[i,]<-mboot_p(p = p10, psi = psi10, xi = xi)
  q10b[i,]<-find_qx(px = p10b[i,], ax = a, taux = g)
  
  #xi<-rnorm(n=nx)
  p01b[i,]<-mboot_p(p = p01, psi = psi01, xi = xi)
  q01b[i,]<-find_qx(px = p01b[i,], ax = a, taux = g)
  
  #xi<-rnorm(n=nx)
  p00b[i,]<-mboot_p(p = p00, psi = psi00, xi = xi)
  q00b[i,]<-find_qx(px = p00b[i,], ax = a, taux = g)
  
  print(i)
  
}

## bootstrap estimates
TQTEb<-q11b-q00b
NDQTEb<-q10b-q00b
NIQTEb<-q11b-q10b
NDQTE1b<-q11b-q01b
NIQTE1b<-q01b-q00b

## pci: percentile method
TQTE_bci<-pci_per(t(TQTEb), TQTE, alpha = 0.05)
NDQTE_bci<-pci_per(t(NDQTEb), NDQTE, alpha = 0.05)
NIQTE_bci<-pci_per(t(NIQTEb), NIQTE, alpha = 0.05)
NDQTE1_bci<-pci_per(t(NDQTE1b), NDQTE1, alpha = 0.05)
NIQTE1_bci<-pci_per(t(NIQTE1b), NIQTE1, alpha = 0.05)

## sci
TQTE_sci<-sci_qs(t(TQTEb), TQTE, q = 0.25, alpha = 0.05)
NDQTE_sci<-sci_qs(t(NDQTEb), NDQTE, q = 0.25, alpha = 0.05)
NIQTE_sci<-sci_qs(t(NIQTEb), NIQTE, q = 0.25, alpha = 0.05)
NDQTE1_sci<-sci_qs(t(NDQTE1b), NDQTE1, q = 0.25, alpha = 0.05)
NIQTE1_sci<-sci_qs(t(NIQTE1b), NIQTE1, q = 0.25, alpha = 0.05)

################################################
## Plot the estimates and pci, ucb
k1<-701

## TQTE
y<-TQTE
bci<-TQTE_bci
sci<-TQTE_sci
data_result<-data.frame(quantile = g, slow = sci[,1],
                  low = bci[,1], y = y, high = bci[,2], shigh = sci[,2])
data_result<-data_result[1:k1, ]

matplot(data_result[,2:ncol(data_result)], type = "l")
abline(h = 0)

## NDATE
y<-NDQTE
bci<-NDQTE_bci
sci<-NDQTE_sci
data_result<-data.frame(quantile = g, slow = sci[,1],
                        low = bci[,1], y = y, high = bci[,2], shigh = sci[,2])
data_result<-data_result[1:k1, ]

matplot(data_result[,2:ncol(data_result)], type = "l")
abline(h = 0)

## NIQTE
y<-NIQTE
bci<-NIQTE_bci
sci<-NIQTE_sci
data_result<-data.frame(quantile = g, slow = sci[,1],
                        low = bci[,1], y = y, high = bci[,2], shigh = sci[,2])
data_result<-data_result[1:k1, ]

matplot(data_result[,2:ncol(data_result)], type = "l", ylim = c(-30,20))
abline(h = 0)

## NDQTE'
y<-NDQTE1
bci<-NDQTE1_bci
sci<-NDQTE1_sci
data_result<-data.frame(quantile = g, slow = sci[,1],
                        low = bci[,1], y = y, high = bci[,2], shigh = sci[,2])
data_result<-data_result[1:k1, ]

matplot(data_result[,2:ncol(data_result)], type = "l")
abline(h = 0)

## NIQTE'
y<-NIQTE1
bci<-NIQTE1_bci
sci<-NIQTE1_sci
data_result<-data.frame(quantile = g, slow = sci[,1],
                        low = bci[,1], y = y, high = bci[,2], shigh = sci[,2])
data_result<-data_result[1:k1, ]

matplot(data_result[,2:ncol(data_result)], type = "l", ylim = c(-30, 20))
abline(h = 0)
