##function quantile mediation effect
## pci: (t-percentile method (Van der Vaart, 1998) or the basic/reflection CI)
pci_per<-function(datab, est, alpha){    ## datab: bootstrap data
                                         ## est: estimate of the parameter
                                         ## alpha: significance level
  stopifnot(is.matrix(datab), nrow(datab) == length(est))
  stopifnot(alpha > 0, alpha < 1)
  
  std_b<-rep(1, times = length(est))     ## bootstrap standard deviation = 1
  
  ## calculate t-stat
  result<-scale(t(datab), center = est, scale = std_b)
  
  ## calculate pointwise confidence intervals
  z_up<-apply(result, 2, quantile, probs = 1 - alpha/2, na.rm = TRUE)
  z_low<-apply(result, 2, quantile, probs = alpha/2, na.rm = TRUE)
  
  pci<-cbind(est - std_b*z_up, est - std_b*z_low)
  pci<-apply(pci, 1, sort)
  return(t(pci))
  
}

## sci: using rescaled quantile spread (CFM, 2013)
sci_qs<-function(datab, est, q = 0.1, alpha){  ## datab: bootstrap data
                                         ## est: estimate of the parameter
                                         ## q: lower quantile for quantile spread, 
                                         ## common choices in the literature are q = 0.05 or 0.1.
                                         ## alpha: significance level
                                         
  stopifnot(is.matrix(datab), nrow(datab) == length(est))
  stopifnot(q>0, q<0.5)
  stopifnot(alpha > 0, alpha < 1)
  
  ##calculate bootstrap standard error using rescaled quantile spread
  q_up<-apply(datab, 1, quantile, probs = 1 - q, na.rm = TRUE)
  q_low<-apply(datab, 1, quantile, probs = q, na.rm = TRUE)
  
  std_b<-sqrt((q_up - q_low)^2/(qnorm(1 - q) - qnorm(q))^2)             
  
  stopifnot("All std_b values are NA or zero — check bootstrap draws" = any(std_b != 0, na.rm = TRUE))
  
  if(any(is.na(std_b))){
    warning(sum(is.na(std_b)), " grid point(s) have NA std_b; check bootstrap draws.")
  }
  std_b[std_b==0 | is.na(std_b)] <- min(std_b[std_b!=0 & !is.na(std_b)]) ## replace zero or NA with min
  
  ##calculate t-stat
  result<-scale(t(datab), center = est, scale = std_b)
  result<-abs(result)
  
  ##calculate max t-stat
  result_z_max<-apply(result, 1, max, na.rm = TRUE)
  zx<-as.numeric(quantile(result_z_max, probs = 1 - alpha))
  sci<-cbind(est - std_b*zx, est + std_b*zx)
  sci<-apply(sci,1,sort)
  return(t(sci))
  
}

## function for finding the quantile of continuous variables
find_qx<-function(px, ax, taux){
  
  stopifnot(length(px) == length(ax))
  stopifnot(!is.unsorted(ax))
  stopifnot(!is.unsorted(taux))
  stopifnot(all(px >= 0 & px <= 1), all(taux > 0 & taux < 1))
  
  px<-sort(px)                                      # sort the estimated probability
  datax<-data.frame(p = px, a = ax)
  result<-aggregate(a ~ p, FUN = min, data = datax) # find the smallest value of a
  a<-result$a
  p<-result$p
  
  result1<-approx(p, a, xout = taux, rule = 2)      # find quantile at a specific tau 
  #yleft = -Inf, yright = Inf)
  
  return(sort(result1$y))                           # sort the quantiles
  
}

## function for generating k_fold sample index

k_fold_ind<-function(n, k_fold){      ## n >> k_fold must hold
  
  stopifnot(n > k_fold)
  #ind<-sample(c(1:n), size = n, 
  #             replace = FALSE)     ## randomize the sample index
  #n_k<-floor(n/k_fold)*(k_fold-1)   ## sample size for estimation
  #n_pred<-n - n_k                   ## sample size for prediction
  
  n_pred<-floor(n/k_fold)
  stopifnot(n_pred >= 1)  
  
  ## for prediction
  ind_st<-seq(1, n, by = n_pred)    ## this is the start of index of samples
  ind_end<-seq(n_pred, n, by = n_pred)         ## this is the end of index of samples 

  if(length(ind_end)< length(ind_st)){
    ind_end<-c(ind_end, n)
  }
  
  ind_pred<-cbind(ind_st, ind_end)  ## combine the start and end indices
  
  ## collapse ANY number of excess rows, not just one
  while(nrow(ind_pred) > k_fold){
    ind_pred <- ind_pred[-nrow(ind_pred), , drop = FALSE]
    ind_pred[nrow(ind_pred), 2] <- n
  }
  
  stopifnot(nrow(ind_pred) == k_fold)   ## final safety check
  
  fold_sizes <- ind_pred[,2] - ind_pred[,1] + 1
  if(any(fold_sizes < 2)){
    warning("Some folds have fewer than 2 observations — check n vs k_fold ratio.")
  }
  
  return(data.frame(ind_pred))
  
}

## lower partial mean
lpm<-function(q, x){
  
  indx<-x<=q
  mean(x*indx)
  
}

## calculating multiplier bootstrap
mboot_p<-function(p, psi, xi){    ## p, psi and xi
  
  psib<-(t(psi)-p)%*%xi
  pb_raw<-p+psib/length(xi)
  pb<-pmax(pmin(1, pb_raw),0)
  pb<-sort(pb)
  
  return(pb)
  
}