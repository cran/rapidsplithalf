
# Helper functions used by rapidsplit() and associated methods.

mf<-function(x,digits=2){
  s<-format(round(x,digits=digits),
            digits=ifelse(digits!=0,digits,getOption("digits")),
            drop0trailing=T,scientific=F,nsmall=digits)
  s<-sub("(?<![0-9])0+(?=\\.)", "",s, perl = TRUE)
  return(s)
}

runlengths <- function (x) {
  n <- length(x)
  if (n == 0L) return(integer())
  y <- x[-1L] != x[-n]
  i <- c(which(y), n)
  return(diff(c(0L, i)))
}

cols2ids<-function(df){
  for(col in seq_along(df)){
    df[,col]<-as.numeric(as.factor(df[[col]]))
  }
  df
}

clamp.range<-function(x){
  x[x==min(x)]<- -1
  x[x==max(x)]<- 1
  x
}

# takes vector input
makeCustomAggregator<-function(fun){
  function(x,mask){
    out<-numeric(ncol(mask))
    for(i in seq_len(ncol(mask))){
      out[i]<-fun(x[mask[,i,drop=T]])
    }
    return(out)
  }
}

# takes matrix input
makeCustomAggregator2<-function(fun){
  function(x,mask){
    out<-numeric(ncol(mask))
    for(i in seq_len(ncol(mask))){
      out[i]<-fun(x[mask[,i,drop=T],i])
    }
    return(out)
  }
}

datachecker<-function(data,subjvar,diffvars,stratvars,subscorevar,aggvar,
                      errorhandling,standardize){
  if(!is.data.frame(data)){
    stop("Input dataset not a data.frame.")
  }
  if(length(subjvar)!=1L || length(aggvar)!=1L || !is.character(subjvar) || !is.character(aggvar)){
    stop("Arguments subjvar and aggvar must be character vectors of length 1.")
  }
  if(length(subscorevar)>1L){
    stop("Can only specify 1 subscorevar.")
  }else if(length(subscorevar)==1L){
    if(!is.character(subscorevar)){
      stop("Argument subscorevar must be a character vector.")
    }
  }
  if(length(diffvars)>0L){
    if(!is.character(diffvars)){
      stop("Argument diffvars must be a character vector.")
    }
  }
  if(length(stratvars)>0L){
    if(!is.character(stratvars)){
      stop("Argument stratvars must be a character vector.")
    }
  }
  if(length(aggvar)>1L){
    stop("Can only have one aggvar.")
  }else if(!is.numeric(data[[aggvar]])){
    stop("The variable specified by aggvar must be numeric.")
  }
  
  if(!all(c(subjvar,diffvars,stratvars,subscorevar,aggvar,
            errorhandling$blockvar,errorhandling$errorvar) %fin% names(data))){
    stop("Not all specified columns present in data.")
  }
  if(length(intersect(diffvars,stratvars))>0L){
    stop("Cannot have a variable specified as both diffvars and stratvars: ",
         intersect(diffvars,stratvars))
  }
  if(length(intersect(subscorevar,stratvars))>0L){
    stop("Cannot have a variable specified as both subscorevar and stratvars: ",
         intersect(subscorevar,stratvars))
  }
  if(length(intersect(diffvars,subscorevar))>0L){
    stop("Cannot have a variable specified as both diffvars and subscorevar: ",
         intersect(diffvars,subscorevar))
  }
  if(anyDuplicated(c(subjvar,diffvars,stratvars,aggvar,subscorevar,errorhandling$errorvar))){
    stop("Cannot have the same variable specified more than once among ",
         "subjvar, diffvars, stratvars, aggvar, subscorevar, or errorhandling$errorvar.")
  }
  
  if(anyNA(data[[aggvar]])){
    stop(sum(is.na(data[[aggvar]]))," missing values present in variable to be aggregated.",
         " Please remove them first.")
  }
  if(!is.numeric(data[[aggvar]])){
    stop("Aggregation variable is not numeric.")
  }
  nvalues<-sapply(data[,diffvars,drop=FALSE],uniqLen)
  if(any(nvalues!=2L)){
    stop("Less or more than 2 unique values present in diffvars ",diffvars[nvalues!=2L],
         ". The variables specified by this argument should feature two unique values, ",
         "indicating which condition (the lower value) ",
         "will be subtracted from the other (the higher value).")
  }
  
  if(length(subscorevar)>0L){
    condpairs<-funique(data[,c(subjvar,subscorevar,diffvars),drop=FALSE])
    subscoresubvalues<-countOccur(condpairs[,c(subjvar,subscorevar),drop=FALSE])
    smallsubscores<-subscoresubvalues$Count!=2^length(diffvars)
    if(any(smallsubscores)){
      stop("Some participants do not have data for all specified pairs of diffvars within subscores: ",
           paste0("participant ",
                  subscoresubvalues[[subjvar]][smallsubscores],
                  " within subscore ",
                  subscoresubvalues[[subjvar]][smallsubscores],
                  collapse=", "))
    }
    
    condcounts<-countOccur(data[,c(subjvar,subscorevar,diffvars),drop=FALSE])
    smallconds<-condcounts$Count<2L
    if(any(smallconds)){
      stop("Insufficient data (<2 obs) in 1 or more conditions belonging to these participants: ",
           paste0(funique(paste0(condcounts[[subjvar]][smallconds],
                                 " within subscore ",
                                 condcounts[[subscorevar]][smallconds])),
                  collapse=", "))
    }
    
    if(standardize){
      datapersubject<-countOccur(data[,c(subjvar,subscorevar),drop=FALSE])
      if(!all(datapersubject$Count>=4L)){
        smallsubscores <- datapersubject$Count<4L
        stop("Cannot compute some participants' standard deviation due to insufficient data: ",
             paste(datapersubject[smallsubscores,subjvar],
                   "within subscore",
                   datapersubject[smallsubscores,subscorevar],collapse=", "))
      }
      
      indices<-do.call(paste,data[,c(subjvar,subscorevar),drop=FALSE])
      nuniques<-tapply(X=data[[aggvar]],INDEX=indices,FUN=uniqLen)
      toofew<-names(nuniques[nuniques==1L])
      if(any(nuniques==1L)){
        stop("Cannot compute the SD of every participant/subscore, ",
             "due to less than 2 unique values in these participants/subscores: ",
             paste0(funique(paste0("participant ",data[[subjvar]][indices %fin% toofew],
                                   " within subscore ",data[[subscorevar]][indices %fin% toofew])),
                    collapse=", "))
      }
    }
  }else{
    condpairs<-funique(data[,c(subjvar,diffvars),drop=FALSE])
    condcounts<-countOccur(condpairs[[subjvar]])
    smallcondcounts<-condcounts$Count!=2L^length(diffvars)
    if(sum(smallcondcounts)>0){
      stop("Some participants do not have data for all specified pairs of diffvars: ",
           paste0("participant ",condcounts$Variable[smallcondcounts]))
    }
    
    condcounts<-countOccur(data[,c(subjvar,diffvars),drop=FALSE])
    smallconds<-condcounts$Count<2L
    if(any(smallconds)){
      stop("Insufficient data (<2 obs) in 1 or more conditions belonging to these participants: ",
           paste(funique(condcounts[[subjvar]][smallconds]),collapse=", "))
    }
    
    datapersubject<-countOccur(data[[subjvar]])
    if(standardize && !all(datapersubject$Count>=4L)){
      stop("Cannot compute some participants' standard deviation due to insufficient data: ",
           paste(datapersubject$Variable[datapersubject$Count<4L],collapse=", "))
    }
    if(standardize){
      nuniques<-tapply(X=data[[aggvar]],INDEX=data[[subjvar]],FUN=uniqLen)
      toofew<-names(nuniques[nuniques==1L])
      if(any(nuniques==1L)){
        stop("Cannot compute the SD of every participant/subscore, ",
             "due to less than 2 unique values in these participants: ",
             paste0(toofew,collapse=", "))
      }
    }
  }
}

checkerrorhandling<-function(x){
  if(length(x$type)>1L){
    x$type<-x$type[1L]
  }
  if(length(x$type)==0L){
    x$type<-"none"
  }
  missings<-setdiff(c("errorvar","blockvar"),names(x))
  missings<-vector(mode="list",length=length(missings)) |> setNames(missings)
  x<-c(x,missings)
  if(length(x$fixedpenalty)==0L){
    x$fixedpenalty<-600
  }
  return(x)
}
