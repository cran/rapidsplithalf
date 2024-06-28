


#' Spearman-Brown correction
#' Perform a Spearman-Brown correction on the provided correlation score.
#' @param r To-be-corrected correlation coefficient.
#' @param ntests An integer indicating how many times larger the full test is, 
#' for which the corrected correlation coefficient is being computed.
#' 
#' @details
#' When \code{ntests=2}, the formula will compute what the correlation coefficient would be 
#' if the test were twice as long.
#' 
#' @return Spearman-Brown corrected correlation coefficients.
#' @export
#'
#' @examples
#' spearmanBrown(.5)
#' 
spearmanBrown<-function(r,ntests=2){
  sb<-ntests*r / (1+(ntests-1)*r)
  return(sb)
}