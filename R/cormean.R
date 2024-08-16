#' @name correlation-tools
#' @title Miscellaneous correlation tools
#' @description Helper functions to compute important statistics from correlation coefficients.
#' @param r,r1,r2 Correlation values.
#' @param z Z-scores.
#' @param t t-scores.
#' @param n,n1,n2 Sample sizes.
#' @param alpha The significance level to use.
#' @param x A \code{compcorr} object to print.
#' @param ... Ignored.
#' @seealso \link{cormean}
#' @return For \code{r2z()}, \code{z2r}, \code{r2t}, \code{t2r}, and \code{r2p}, 
#' a numeric vector with the requested transformation applied. 
#' For \code{rconfint()}, a numeric vector with two values representing 
#' the lower and upper confidence intervals of the correlation coefficient.
#' For \code{compcorr()}, a \code{compcorr} object containing
#' a z and p value for the requested comparison, 
#' which can be printed with \code{print.compcorr()}.
#' @examples
#' z <- r2z(.5)
#' r <- z2r(z)
#' t<-r2t(r,30)
#' r<-t2r(t,30)
#' r2p(r,30)
#' print(rconfint(r,30))
#' print(compcorr(.5,.7,20,20))
NULL

#' @export
#' @describeIn correlation-tools Converts correlation coefficients to z-scores.
r2z<-function(r){
  z<-.5 * (log(1+r) - log(1-r))
  return(z)
}
#' @export
#' @describeIn correlation-tools Converts z-scores to correlation coefficients.
z2r<-function(z){
  r<-(exp(2*z)-1)/(exp(2*z)+1)
  rma<-which(is.nan(r))
  r[rma]<-ifelse(z[rma]>0,1,-1)
  return(r)
}

#' @export
#' @describeIn correlation-tools Converts correlation coefficients to t-scores.
r2t<-function(r,n){ (r*sqrt(n-2))/sqrt(1-r^2) }

#' @export
#' @describeIn correlation-tools Converts t-scores to correlation coefficients.
t2r<-function(t,n){ sqrt(t/sqrt(t^2+n-2)) }

#' @export
#' @describeIn correlation-tools Computes the two-sided p-value for a given correlation.
r2p<-function(r,n){ 2*pt(abs(r2t(r,n)),n-2,lower.tail=FALSE) }

#' @export
#' @describeIn correlation-tools Computes confidence intervals for one or multiple correlation coefficients.
rconfint<-function(r,n,alpha=.05){
  z <- r2z(r)
  zint <- qnorm(1 - alpha/2) * sqrt(1/(n - 3))
  if(length(r)==1){
    confints <- c(z2r(z - zint), z2r(z + zint))
  }else if(length(r)>1){
    confints <- cbind(z2r(z - zint), z2r(z + zint))
  }else{
    confints <- NULL
  }
  return(confints)
}

#' @export
#' @describeIn correlation-tools Computes the significance of the difference between two correlation coefficients.
compcorr<-function(r1,r2,n1,n2){
  zval<-abs(r2z(r1)-r2z(r2)) / sqrt((1/(n1-3)) + (1/(n2-3)))
  pval<-min(1,pnorm(abs(zval),lower.tail=FALSE)*2)
  return(structure(list(zscore=zval,pvalue=pval),class="compcorr"))
}

#' @export
#' @describeIn correlation-tools Computes the significance of the difference between two correlation coefficients.
print.compcorr<-function(x,...){
  cat("Two-tailed Z-test for the difference between two correlation coefficients.",
      "\nZ =",x$zscore,"\np =",x$pvalue,"\n")
}

#' Compute a minimally biased average of correlation values
#'
#' This function computes a minimally biased average of correlation values.
#' This is needed because simple averaging of correlations is negatively biased,
#' and the often used z-transformation method of averaging correlations is positively biased.
#' The algorithm was developed by Olkin & Pratt (1958).
#'
#' @param r A vector containing correlation values/
#' @param n A single value or vector containing sample sizes/
#' @param weights Character. How should the correlations be weighted?
#' \code{none} leads to no weighting, \code{n} weights by sample size,
#' \code{df} weights by sample size minus one.
#' @param type Character. Determines which averaging algorithm to use, 
#' with "OP5" usually being the most accurate.
#' @param na.rm Logical. Should missing values be removed?
#'
#' @return An average correlation.
#' @name cormean
#' @export
#'
#' @references
#' Olkin, I., & Pratt, J. (1958). Unbiased estimation of certain correlation coefficients.
#' The Annals of Mathematical Statistics, 29. https://doi.org/10.1214/aoms/1177706717
#'
#' Shieh, G. (2010). Estimation of the simple correlation coefficient. Behavior Research Methods,
#' 42(4), 906-917. https://doi.org/10.3758/BRM.42.4.906
#'
#' @examples
#' cormean(c(0,.3,.5),c(30,30,60))
#' 
cormean<-function(r,n,weights=c("none","n","df"),type=c("OP5","OP2","OPK"),na.rm=FALSE){
  type<-match.arg(type)
  weights<-match.arg(weights)
  
  if(length(r)==1){
    return(r)
  }else if(length(n)==1){
    n<-rep(n,length(r))
  }
  
  if(na.rm){
    missing<-which(is.na(r) | is.na(n))
    if(length(missing)>0){
      r<-r[-missing]
      n<-n[-missing]
    }
  }
  weight<-list(rep(1,times=length(n)),n,n-1)[[1+(weights=="n")+2*(weights=="df")]]
  if(length(r)!=length(n)){
    stop("Length of r and n not equal!")
  }
  
  if(any(n<5)){
    stop("This function cannot accurately average correlations when any have n<5")
  }
  
  if(type=="OP5"){
    sizevec<-unique(n)
    lgammachain1<-lgamma(.5+1:5)*2
    lgammachain2<-lgamma(.5)*2
    factorialchain<-factorial(1:5)
    gammalist<-exp(sapply(sizevec,function(nr){
      (lgammachain1 + lgamma(nr/2-1)) - (lgammachain2 + lgamma(nr/2-1+1:5))}))
    corlist<-sapply(seq_along(r),
                    function(i){ r[i]*(1+ sum(gammalist[,match(n[i],sizevec)] *
                                                (1-r[i]^2)^(1:5)/factorialchain))})
    rmean<-weighted.mean(x= corlist,w= weight)
  }else if(type=="OPK"){
    rmean<-weighted.mean(x= r*(1+(1-r^2)/(2*(n-(9*sqrt(2)-7)/2))),
                         w= weight)
  }else if(type=="OP2"){
    rmean<-weighted.mean(x= r*(1+ (1-r^2)/(2*(n-2)) +
                                 (9*(1-r^2)^2)/(8*n*(n-2))),
                         w= weight)
  }
  return(rmean)
}



