


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
#' @md
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









