


#' Spearman-Brown correction
#' Perform a Spearman-Brown correction on the provided correlation score.
#' @param r To-be-corrected correlation coefficient.
#' @param ntests An integer indicating how many times larger the full test is, 
#' for which the corrected correlation coefficient is being computed.
#' @param fix.negative How will negative input values be dealt with? 
#' * \code{"mirror"} submits the absolute correlations to the formula 
#' and restores the original sign afterwards.
#' * \code{"nullify"} sets negative correlations to zero.
#' * \code{"none"} leaves them as-is (not recommended).
#' 
#' @details
#' When \code{ntests=2}, the formula will compute what the correlation coefficient would be 
#' if the test were twice as long.
#' 
#' @return Spearman-Brown corrected correlation coefficients.
#' @export
#' @author Sercan Kahveci
#' @md
#' @seealso [requiredTestSize()] to compute how many items a test with a known reliability needs, to achieve a goal reliability.
#' @examples
#' spearmanBrown(.5)
#' 
spearmanBrown<-function(r,ntests=2,fix.negative=c("mirror","nullify","none")){
  fix.negative<-match.arg(fix.negative)
  
  if(fix.negative=="mirror"){
    sb<-ntests*r / (1+(ntests-1)*abs(r))
  }else{
    # includes "none"
    sb<-ntests*r / (1+(ntests-1)*r)
    
    if(fix.negative=="nullify"){
      sb[sb<0]<-0
    }
  }
  return(sb)
}


#' Required test size for target reliability
#' 
#' Compute how many items a test needs, for its reliability to reach a goal value.
#'
#' @param obs_r The observed reliability
#' @param goal_r The goal reliability
#' @param n Current test size, i.e., the number of items or trials.
#' @param fix.negative How should negative observed reliabilities be handled? See [spearmanBrown()].
#'
#' @returns The required test size.
#' @details This function is obtained by solving the [spearmanBrown()] formula for the \code{ntests} argument.
#' @md
#' @author Sercan Kahveci
#' @export
#'
#' @examples
#' requiredTestSize(obs_r=0.5, goal_r=0.8,n=256)
#' 
#' requiredTestSize(obs_r=0, goal_r=0.8,n=256)
#' 
requiredTestSize<-function(obs_r,goal_r=.8,n=1,fix.negative=c("mirror","nullify","none")){
  fix.negative<-match.arg(fix.negative)
  if(fix.negative=="mirror"){
    k<-(1/obs_r-sign(obs_r))/(1/goal_r-sign(obs_r))*n
  }else{
    if(fix.negative=="nullify"){
      obs_r[obs_r<0]<-0
    }
    k<-(1/obs_r-1)/(1/goal_r-1)*n
  }
  k[obs_r<=0 & goal_r>0]<-Inf
  k[obs_r>=0 & goal_r<0]<-NA
  return(k)
}







