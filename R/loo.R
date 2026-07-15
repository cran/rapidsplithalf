
#' Compute leave-one-out reliabilities
#' 
#' Per subject, compute what the reliability would be if they were left out. 
#'
#' @param x A \code{rapidsplit} object.
#'
#' @returns A \code{data.frame} with 3 columns: (1) **subj** holds the subject ID (character),
#' (2) **loo.r** holds the reliability if this subject is left out, and 
#' (3) **contribution** the full-sample reliability minus the leave-one-out reliability - 
#' aids in interpretation since this value is positive when reliability is improved by the subject 
#' being in the sample.
#' @export
#' @md
#' @author Sercan Kahveci
#' @examples
#' # Get a rapidsplit object to compute leave-one-out reliabilities from
#' data(foodAAT)
#' myrel <- rapidsplit.chunks(data=foodAAT,
#'                            subjvar="subjectid",
#'                            aggvar="RT",
#'                            splits=400,
#'                            split.chunksize=200,
#'                            sample.chunksize=50)
#' 
#' loo.rapidsplit(myrel)
#' 
#' 
loo.rapidsplit <- function(x){
  loosplit <- data.frame(subj=rownames(x$scores$half1),loo.r=NA,contribution=NA,
                         stringsAsCharacters=FALSE)
  loosplit$loo.r <- 
    spearmanBrown(looCorStatsByColumns(x$scores$half1,x$scores$half2)$meanloocors)
  loosplit$contribution <- x$r - loosplit$loo.r
  return(loosplit)
}

