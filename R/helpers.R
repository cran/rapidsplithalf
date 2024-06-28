
# Helper functions used by rapidsplit() and associated methods.

mf<-function(x,digits=2){
  s<-format(round(x,digits=digits),
            digits=digits,drop0trailing=T,scientific=F,nsmall=digits)
  s<-sub("(?<![0-9])0+(?=\\.)", "",s, perl = TRUE)
  return(s)
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
  if(length(subjvar)!=1 | length(aggvar)!=1 | !is.character(subjvar) | !is.character(aggvar)){
    stop("Arguments subjvar and aggvar must be character vectors of length 1.")
  }
  if(length(subscorevar)>1){
    stop("Can only specify 1 subscorevar.")
  }else if(length(subscorevar==1)){
    if(!is.character(subscorevar)){
      stop("Argument subscorevar must be a character vector of length 1.")
    }
  }
  if(length(diffvars)>0){
    if(!is.character(diffvars)){
      stop("Argument diffvars must be a character vector.")
    }
  }
  if(length(stratvars)>0){
    if(!is.character(stratvars)){
      stop("Argument stratvars must be a character vector.")
    }
  }
  if(length(aggvar)>1){
    stop("Can only have one aggvar.")
  }else if(!is.numeric(data[[aggvar]])){
    stop("The variable specified by aggvar must be numeric.")
  }
  
  if(!all(c(subjvar,diffvars,stratvars,subscorevar,aggvar,
            errorhandling$blockvar,errorhandling$errorvar) %in% names(data))){
    stop("Not all specified columns present in data.")
  }
  if(length(intersect(diffvars,stratvars))>0){
    stop("Cannot have a variable specified as both diffvars and stratvars: ",
         intersect(diffvars,stratvars))
  }
  if(length(intersect(subscorevar,stratvars))>0){
    stop("Cannot have a variable specified as both subscorevar and stratvars: ",
         intersect(subscorevar,stratvars))
  }
  if(length(intersect(diffvars,subscorevar))>0){
    stop("Cannot have a variable specified as both diffvars and subscorevar: ",
         intersect(diffvars,subscorevar))
  }
  if(anyNA(data[[aggvar]])){
    stop(sum(is.na(data[[aggvar]]))," missing values present in variable to be aggregated.",
         " Please remove them first.")
  }
  if(!is.numeric(data[[aggvar]])){
    stop("Aggregation variable is not numeric.")
  }
  nvalues<-sapply(data[,diffvars,drop=FALSE],function(x){ length(unique(x))})
  if(any(nvalues!=2)){
    stop("Less or more than 2 unique values present in diffvars ",diffvars[nvalues!=2],
         ". The variables specified by this argument should feature two unique values, ",
         "indicating which condition (the lower value) ",
         "will be subtracted from the other (the higher value).")
  }
  
  if(length(subscorevar)>0){
    indices<-as.data.frame(table(unique(data[,c(subjvar,subscorevar,diffvars)])))
    subscoresubvalues<-table(do.call(paste,indices[,c(subjvar,subscorevar),drop=FALSE]))
    if(!all(subscoresubvalues == 2^length(diffvars))){
      stop("Some participants do not have data for all specified conditions within subscores: ",
           paste0("participant ",indices[[subjvar]][subscoresubvalues!=2^length(diffvars)],
                  " within subscore ",indices[,subscorevar][subscoresubvalues!=2^length(diffvars)],
                  collapse=", "))
    }
    condpaste<-do.call(paste,data[,c(subjvar,subscorevar,diffvars),drop=FALSE])
    condcounts<-table(condpaste)
    smallconds<-names(condcounts[condcounts<2])
    if(!all(condcounts>=2)){
      stop("Insufficient data (<2 obs) in 1 or more conditions belonging to these participants: ",
           paste0(unique(paste0(data[[subjvar]][condpaste %in% smallconds],
                                " within subscore ",
                                data[[subscorevar]][condpaste %in% smallconds])),
                  collapse=", "))
    }
    datapersubject<-table(do.call(paste,data[,c(subjvar,subscorevar),drop=FALSE]))
    if(standardize & !all(datapersubject>=4)){
      stop("Cannot compute some participants' standard deviation due to insufficient data: ",
           paste(names(datapersubject)[datapersubject<4],collapse=", "))
    }
    if(standardize){
      indices<-do.call(paste,data[,c(subjvar,subscorevar),drop=FALSE])
      nuniques<-tapply(X=data[[aggvar]],INDEX=indices,FUN=function(x){length(unique(x))})
      toofew<-names(nuniques[nuniques==1])
      if(any(nuniques==1)){
        stop("Cannot compute the SD of every participant/subscore, ",
             "due to less than 2 unique values in these participants/subscores: ",
             paste0(unique(paste0("participant ",data[[subjvar]][indices %in% toofew],
                                  " within subscore ",data[[subscorevar]][indices %in% toofew])),
                    collapse=", "))
      }
    }
  }else{
    indices<-as.data.frame(table(unique(data[,c(subjvar,diffvars)])))
    subscoresubvalues<-table(indices[[subjvar]])
    if(!all(subscoresubvalues == 2^length(diffvars))){
      stop("Some participants do not have data for all specified conditions: ",
           paste(unique(indices[[subjvar]][subscoresubvalues!=2^length(diffvars)]),collapse=", "))
    }
    condpaste<-do.call(paste,data[,c(subjvar,diffvars),drop=FALSE])
    condcounts<-table(condpaste)
    smallconds<-names(condcounts[condcounts<2])
    if(!all(condcounts>=2)){
      stop("Insufficient data (<2 obs) in 1 or more conditions belonging to these participants: ",
           paste(unique(data[[subjvar]][condpaste %in% smallconds]),collapse=", "))
    }
    datapersubject<-table(data[[subjvar]])
    if(standardize & !all(datapersubject>=4)){
      stop("Cannot compute some participants' standard deviation due to insufficient data: ",
           paste(names(datapersubject)[datapersubject<4],collapse=", "))
    }
    if(standardize){
      nuniques<-tapply(X=data[[aggvar]],INDEX=data[[subjvar]],FUN=function(x){length(unique(x))})
      toofew<-names(nuniques[nuniques==1])
      if(any(nuniques==1)){
        stop("Cannot compute the SD of every participant/subscore, ",
             "due to less than 2 unique values in these participants: ",
             paste0(toofew,collapse=", "))
      }
    }
  }
}

checkerrorhandling<-function(x){
  if(length(x$type)>1){
    x$type<-x$type[1]
  }
  if(length(x$type)==0){
    x$type<-"none"
  }
  missings<-setdiff(c("errorvar","blockvar"),names(x))
  missings<-vector(mode="list",length=length(missings)) |> setNames(missings)
  x<-c(x,missings)
  if(length(x$fixedpenalty)==0){
    x$fixedpenalty<-600
  }
  return(x)
}
