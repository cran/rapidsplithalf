


#' rapidsplit
#' @description A very fast algorithm for computing stratified permutated split-half reliability.
#'
#' @param data Dataset, a \code{data.frame}.
#' @param subjvar Subject ID variable name, a \code{character}.
#' @param diffvars Names of variables that determine which conditions 
#' need to be subtracted from each other, \code{character}.
#' @param stratvars Additional variables that the splits should 
#' be stratified by; a \code{character}.
#' @param subscorevar A \code{character} variable identifying subgroups within 
#' a participant's data from which separate scores should be computed. 
#' To compute a participant's final score, these subscores will be averaged together. 
#' A typical use case is the D-score of the implicit association task.
#' @param aggvar Name of variable whose values to aggregate, a \code{character}.
#' Examples include reaction times and error rates.
#' @param splits Number of split-halves to average, an \code{integer}. 
#' It is recommended to use around 5000.
#' @param aggfunc The function by which to aggregate the variable 
#' defined in \code{aggvar}; can be \code{"means"}, \code{"medians"}, 
#' or a custom function (not a function name). 
#' This custom function must take a numeric vector and output a single value.
#' Only if \code{aggfunc} is set to \code{"custom"}.
#' @param errorhandling A list with 4 named items, to be used to replace error trials
#'  with the block mean of correct responses plus a fixed penalty, as in the IAT D-score.
#'  The 4 items are \code{type} which can be set to \code{"none"} for no error replacement, 
#'  or \code{"fixedpenalty"} to replace error trials as described;
#'  \code{errorvar} requires name of the \code{logical} variable indicating an incorrect response
#'  (as \code{TRUE}); \code{fixedpenalty} indicates how much of a penalty should be added 
#'  to said block mean; and \code{blockvar} indicates the name of the block variable.
#' @param standardize Whether to divide by scores by the subject's SD; a \code{logical}. 
#' Regardless of whether error penalization is utilized, this standardization 
#' will be based on the unpenalized SD of correct and incorrect trials, as in the IAT D-score.
#' @param include.scores Include all individual split-half scores?
#' @param verbose Display progress bars? Defaults to \code{TRUE}.
#' @param check Check input for possible problems?
#'
#' @return A \code{list} containing 
#' 
#' * \code{r}: the averaged reliability.
#' 
#' * \code{allcors}: a vector with the reliability of each iteration.
#' 
#' * \code{nobs}: the number of participants.
#' 
#' * \code{scores}: the individual participants scores in each split-half, 
#' contained in a list with two matrices (Only included if requested with \code{include.scores}).
#' 
#' @md
#' 
#' @note
#' * This function can use a lot of memory in one go. 
#' If you're computing the reliability of a large dataset or you have little RAM, 
#' it may pay off to use the sequential version of this function instead: 
#' [rapidsplit.chunks()]
#' 
#' * It is currently unclear it is better to pre-process your data before or after splitting it. 
#' If you are computing the IAT D-score, 
#' you can therefore use \code{errorhandling} and \code{standardize} to perform these two actions
#' after splitting, or you can process your data before splitting and forgo these two options.
#' 
#' @details
#' The order of operations (with optional steps between brackets) is: 
#' * Splitting
#' * (Replacing error trials within block within split)
#' * Computing aggregates per condition (per subscore) per person
#' * Subtracting conditions from each other
#' * (Dividing the resulting (sub)score by the SD of the data used to compute that (sub)score)
#' * (Averaging subscores together into a single score per person)
#' * Correlating scores from one half with scores from the other half
#' * Applying the Spearman-Brown correction using [spearmanBrown()]
#' * Computing the average split-half reliability using [cormean()]
#' 
#' @export
#'
#' @examples 
#' 
#' data(foodAAT)
#' # Reliability of the double difference score:
#' # [RT(push food)-RT(pull food)] - [RT(push object)-RT(pull object)]
#' 
#' frel<-rapidsplit(data=foodAAT,
#'                  subjvar="subjectid",
#'                  diffvars=c("is_pull","is_target"),
#'                  stratvars="stimid",
#'                  aggvar="RT",
#'                  splits=100)
#'                  
#' print(frel)
#' 
#' plot(frel,type="all")
#' 
#'            
#' # Compute a single random split-half reliability of the error rate
#' rapidsplit(data=foodAAT,
#'            subjvar="subjectid",
#'            aggvar="error",
#'            splits=1,
#'            aggfunc="means")
#' 
#' # Compute the reliability of an IAT D-score
#' data(raceIAT)
#' rapidsplit(data=raceIAT,
#'            subjvar="session_id",
#'            diffvars="congruent",
#'            subscorevar="blocktype",
#'            aggvar="latency",
#'            errorhandling=list(type="fixedpenalty",errorvar="error",
#'                               fixedpenalty=600,blockvar="block_number"),
#'            splits=100,
#'            standardize=TRUE)
#' 
rapidsplit<-function(data,subjvar,diffvars=NULL,stratvars=NULL,subscorevar=NULL,
                      aggvar,splits,
                      aggfunc=c("means","medians"),
                      errorhandling=list(type=c("none","fixedpenalty"),
                                         errorvar=NULL,fixedpenalty=600,blockvar=NULL),
                      standardize=FALSE,include.scores=TRUE,verbose=TRUE,check=TRUE){
  data<-as.data.frame(data)
  
  # process errorhandling object
  errorhandling<-checkerrorhandling(errorhandling)
  
  # check input
  if(check){
    datachecker(data=data,subjvar=subjvar,diffvars=diffvars,stratvars=stratvars,
                subscorevar=subscorevar,aggvar=aggvar,
                errorhandling=errorhandling,standardize=standardize)
  }
  
  # Arrange properly
  runorder<-do.call(order,data[c(subjvar,subscorevar,errorhandling$blockvar,diffvars,stratvars)])
  arr.ds<- data[runorder,c(subjvar,subscorevar,errorhandling$blockvar,diffvars,stratvars,
                           aggvar,errorhandling$errorvar)]
  origpps<-unique(arr.ds[[subjvar]])
  arr.ds[c(subjvar,subscorevar,diffvars,stratvars)]<-
    cols2ids(arr.ds[c(subjvar,subscorevar,diffvars,stratvars)])
  
  # Create indices
  arr.ds[[".diffidx"]]<-do.call(paste,arr.ds[,c(subjvar,subscorevar,diffvars),drop=FALSE])
  arr.ds[[".subscore"]]<-do.call(paste,arr.ds[,c(subjvar,subscorevar),drop=FALSE])
  subscores<-unique(arr.ds[[".subscore"]])
  pps<-unique(arr.ds[[subjvar]])
  
  diffidx<-unique(arr.ds[,c(subjvar,subscorevar,diffvars,".subscore",".diffidx"),drop=FALSE])
  subscorelist<-split(arr.ds,arr.ds[[".subscore"]])
  difflist<-split(arr.ds,arr.ds[[".diffidx"]])
  
  # Compute splithalf masks by subscore
  keys<-setNames(vector(mode="list",length=length(subscores)),subscores)
  if(verbose){
    cat("Generating splits...\n")
    pb = txtProgressBar(min = 0, max = length(subscores), initial = 0)
  }
  for(ss in subscores){
    if(verbose){ setTxtProgressBar(pb,which(ss==subscores)) }
    iterds<-arr.ds[arr.ds[[".subscore"]]==ss,c(diffvars,stratvars),drop=FALSE]
    iterrle<-rle(do.call(paste,args=iterds))
    grsizes<-iterrle$lengths
    if(length(grsizes)==0){grsizes<-nrow(iterds)}
    keys[[ss]]<-stratifiedItersplits(splits=splits, groupsizes=grsizes)
  }
  antikeys<-lapply(keys,function(x) !x)
  if(verbose){close(pb)}
  
  # Generate aggvar matrix and apply error rule
  if(errorhandling$type!="none"){
    aggdim<-"matrix"
    arr.ds[[".blockidx"]]<-
      do.call(paste,arr.ds[,c(subjvar,subscorevar,errorhandling$blockvar),drop=FALSE])
    blockidx<-unique(arr.ds[[".blockidx"]])
    aggmats<-split(x=arr.ds[[aggvar]],f=arr.ds[[".blockidx"]])[blockidx] |> 
      lapply(function(x){ matrix(x,nrow=length(x),ncol=splits)})
    
    # Apply error rule
    if(verbose){
      cat("Replacing error trials\n")
      pb = txtProgressBar(min = 0, max = length(blockidx), initial = 0)
    }
    for(i in seq_along(blockidx)){
      if(verbose){ setTxtProgressBar(pb,i) }
      
      # get mask
      ss<-arr.ds[[".subscore"]][arr.ds[[".blockidx"]]==blockidx[i]][1]
      maskkey<-arr.ds[arr.ds[[".subscore"]]==ss,".blockidx"]==blockidx[i]
      errorvec<-as.logical(arr.ds[[errorhandling$errorvar]][arr.ds[[".blockidx"]]==blockidx[i]])
      
      # replace errors in key
      aggmats[[blockidx[i]]] <- 
        ReplaceErrorsFixed(x=aggmats[[blockidx[i]]],
                           mask=keys[[ss]][maskkey,],
                           error=errorvec,
                           penalty=errorhandling[["fixedpenalty"]])
      
      # replace errors in antikey
      aggmats[[blockidx[i]]] <- 
        ReplaceErrorsFixed(x=aggmats[[blockidx[i]]],
                           mask=antikeys[[ss]][maskkey,],
                           error=errorvec,
                           penalty=errorhandling[["fixedpenalty"]])
    }
    # Merge to align with arr.ds
    aggmat<-do.call(rbind,aggmats)
    rm(aggmats)
    if(verbose){close(pb)}
    
  }else{
    aggdim<-"vector"
  }
  
  # Select aggregator type
  if(all(is.function(aggfunc))){
    custom.aggfunc<-aggfunc
    aggfunc<-"custom"
  }else{
    aggfunc<-match.arg(aggfunc)
  }
  if(aggdim=="vector"){
    if(aggfunc=="means"){
      aggfunc<-meansByMask
    }else if(aggfunc=="medians"){
      aggfunc<-mediansByMask
    }else if(aggfunc=="custom"){
      aggfunc<-makeCustomAggregator(custom.aggfunc)
    }
  }else if(aggdim=="matrix"){
    if(aggfunc=="means"){
      aggfunc<-colMeansMasked
    }else if(aggfunc=="medians"){
      aggfunc<-colMediansMasked
    }else if(aggfunc=="custom"){
      aggfunc<-makeCustomAggregator2(custom.aggfunc)
    }
  }
  
  # Get aggregates per condition
  keymeans<-antikeymeans<-matrix(ncol=splits,nrow=length(difflist))
  if(verbose){
    cat("Aggregating conditions...\n")
    pb = txtProgressBar(min = 0, max = length(difflist), initial = 0) 
  }
  rownames(keymeans)<-rownames(antikeymeans)<-names(difflist)
  for(i in seq_along(difflist)){
    if(verbose){ setTxtProgressBar(pb,i) }
    ssid<-difflist[[i]][[".subscore"]][1]
    diffid<-names(difflist)[i]
    
    if(aggdim=="vector"){
      keymeans[i,]<-
        aggfunc(x=difflist[[i]][[aggvar]],
                mask=keys[[ssid]][subscorelist[[ssid]][[".diffidx"]]==diffid,,drop=FALSE])
      antikeymeans[i,]<-
        aggfunc(x=difflist[[i]][[aggvar]],
                mask=antikeys[[ssid]][subscorelist[[ssid]][[".diffidx"]]==diffid,,drop=FALSE])
      
    }else{
      diffkey<-arr.ds[[".diffidx"]]==diffid
      keymeans[i,]<-
        aggfunc(x=aggmat[diffkey,],
                mask=keys[[ssid]][subscorelist[[ssid]][[".diffidx"]]==diffid,,drop=FALSE])
      antikeymeans[i,]<-
        aggfunc(x=aggmat[diffkey,],
                mask=antikeys[[ssid]][subscorelist[[ssid]][[".diffidx"]]==diffid,,drop=FALSE])
    }
  }
  if(aggdim=="matrix"){ rm(aggmat) }
  if(verbose){close(pb)}
  
  # Get scores
  if(length(diffvars)>0){
    diffidx[[".valence"]]<-Reduce(`*`,x=clamp.range(diffidx[diffvars]))
    keyscores<-antikeyscores<-matrix(ncol=splits,nrow=length(subscores))
    rownames(keyscores)<-rownames(antikeyscores)<-subscores
    for(i in seq_along(subscores)){
      keyscores[i,]<-
        colSums(keymeans[diffidx[[".subscore"]]==subscores[i],,drop=FALSE] * 
                  diffidx[[".valence"]][diffidx[[".subscore"]]==subscores[i]])
      
      antikeyscores[i,]<-
        colSums(antikeymeans[diffidx[[".subscore"]]==subscores[i],,drop=FALSE] * 
                  diffidx[[".valence"]][diffidx[[".subscore"]]==subscores[i]])
      
    }
  }else{
    keyscores<-keymeans
    antikeyscores<-antikeymeans
  }
  
  # Standardize if necessary
  if(standardize){
    keysds<-antikeysds<-matrix(ncol=splits,nrow=length(subscorelist))
    rownames(keysds)<-rownames(antikeysds)<-subscores
    if(verbose){
      cat("Computing standard deviations to standardize scores by...\n")
      pb = txtProgressBar(min = 0, max = length(subscorelist), initial = 0) 
    }
    for(ss in subscores){
      if(verbose){ setTxtProgressBar(pb,which(ss==subscores)) }
      keysds[ss,] <- sdsByMask(x=subscorelist[[ss]][[aggvar]], mask=keys[[ss]])
      antikeysds[ss,] <- sdsByMask(x=subscorelist[[ss]][[aggvar]], mask=antikeys[[ss]])
    }
    keyscores<-keyscores/keysds
    antikeyscores<-antikeyscores/antikeysds
    if(verbose){ close(pb) }
  }
  
  # Aggregate into single scores, if there were subscores
  if(length(subscorevar)>0){
    newkeyscores<-newantikeyscores<-matrix(ncol=splits,nrow=length(pps))
    rownames(newkeyscores)<-rownames(newantikeyscores)<-pps
    for(i in seq_along(pps)){
      ss<-unique(arr.ds[[".subscore"]][arr.ds[[subjvar]]==pps[i]])
      newkeyscores[i,]<-colMeans(keyscores[ss,])
      newantikeyscores[i,]<-colMeans(antikeyscores[ss,])
    }
    keyscores<-newkeyscores
    antikeyscores<-newantikeyscores
  }
  
  # Rename matrix rows to fit names in dataset
  rownames(keyscores)<-origpps[match(rownames(keyscores),pps)]
  rownames(antikeyscores)<-origpps[match(rownames(antikeyscores),pps)]
  
  # Get correlations
  cors<-corByColumns(keyscores,antikeyscores) |> spearmanBrown()
  sampsize<-length(pps)
  
  # Form output
  out<-list(r=cormean(cors,sampsize),
            allcors=cors,
            nobs=sampsize)
  
  # Add individual split halves if requested
  if(include.scores){
    out$scores<-list(half1=keyscores,
                     half2=antikeyscores)
  }
  
  # Add class
  out<-structure(out,class="rapidsplit")
  
  # return
  return(out)
}

#' @param x \code{rapidsplit} object to print or plot.
#' @param ... Ignored.
#' @export
#' @rdname rapidsplit
print.rapidsplit<-function(x,...){
  coefstr<-paste0("Full-length reliability (Spearman-Brown coefficient):\n",
                  "rSB (",mf(x$nobs-2),") = ",mf(x$r),
                  ", 95%CI [", mf(quantile(x$allcors,.025)), 
                  ", ", mf(quantile(x$allcors,.975)),"]",
                  ", p = ",mf(r2p(x$r,x$nobs),digits=3),
                  ", based on ",length(x$allcors)," permutations","\n")
  cat(coefstr,sep="")
}

#' @param type Character argument indicating what should be plotted. 
#' By default, this plots the random split whose correlation is closest to the average.
#' However, this can also plot the random split with 
#' the \code{"minimum"} or \code{"maximum"} split-half correlation, or any \code{"random"} split. 
#' \code{"all"} splits can also be plotted together in one figure.
#' @param show.labels Should participant IDs be shown above their points in the scatterplot?
#' Defaults to \code{TRUE} and is ignored when \code{type} is \code{"all"}.
#'
#' @export
#' @rdname rapidsplit
plot.rapidsplit<-function(x,type=c("average","minimum","maximum","random","all"),
                          show.labels=TRUE,...){
  if(!("scores" %in% names(x))){
    message("Individual split-half scores were not included in rapidsplit object, so cannot be plotted")
    return()
  }
  
  type<-match.arg(type)
  if(type=="average"){
    title<-"Split-half Scatterplot for\nIteration with Average Reliability"
    idx<-which.min(abs(x$allcors-x$r))
  }else if(type=="minimum"){
    title<-"Split-half Scatterplot for\nIteration with the Lowest Reliability"
    idx<-which.min(x$allcors)
  }else if(type=="maximum"){
    title<-"Split-half Scatterplot for\nIteration with the Highest Reliability"
    idx<-which.max(x$allcors)
  }else if(type=="random"){
    title<-"Split-half Scatterplot for Random Iteration"
    idx<-sample(seq_along(x$allcors),1)
  }else if(type=="all"){
    title<-"Split-half Scatterplot for All Iterations"
    idx<-seq_along(x$allcors)
  }
  title <- paste0(title,"\n(r = ", mf(ifelse(length(idx)==1,x$allcors[idx],x$r)),")")
  
  h1vals<-x$scores$half1[,idx] |> as.vector()
  h2vals<-x$scores$half2[,idx] |> as.vector()
  plot(h1vals,h2vals,pch=20,main=title,
       xlab="Half 1 computed score",ylab="Half 2 computed score",
       col=rgb(0,0,0,ifelse(length(idx)==1,1,1/sqrt(length(idx)))))
  if(length(idx)==1 & show.labels){
    text(h1vals,h2vals,rownames(x$scores$half1),cex= 0.7, pos=3, offset=0.3)
  }
}



#' @rdname rapidsplit
#'
#' @param chunks Number of chunks to divide the splits in, for more memory-efficient computation,
#' and to divide over multiple cores if requested.
#' @param cluster Chunks will be run on separate cores if a cluster is provided, 
#' or an \code{integer} specifying the number of cores. Otherwise, if the value is \code{NULL},
#' the chunks are run sequentially.
#'
#' @export
#'
#' @examples
#' 
#' # Unstratified reliability of the median RT
#' rapidsplit.chunks(data=foodAAT,
#'                   subjvar="subjectid",
#'                   aggvar="RT",
#'                   splits=100,
#'                   aggfunc="medians",
#'                   chunks=8)
#' 
#' # Compute the reliability of Tukey's trimean of the RT
#' # on 2 CPU cores
#' trimean<-function(x){ 
#'   sum(quantile(x,c(.25,.5,.75))*c(1,2,1))/4
#' }
#' rapidsplit.chunks(data=foodAAT,
#'                   subjvar="subjectid",
#'                   aggvar="RT",
#'                   splits=200,
#'                   aggfunc=trimean,
#'                   cluster=2)
#' 
rapidsplit.chunks<-
  function(data,subjvar,diffvars=NULL,stratvars=NULL,subscorevar=NULL,
           aggvar,splits,
           aggfunc=c("means","medians","custom"),
           errorhandling=list(type=c("none","fixedpenalty"),
                              errorvar=NULL,fixedpenalty=600,blockvar=NULL),
           standardize=FALSE,include.scores=TRUE,verbose=TRUE,check=TRUE,
           chunks=4,cluster=NULL){
    
  # process errorhandling object
  errorhandling<-checkerrorhandling(errorhandling)
  
  # check input
  if(check){
    datachecker(data=data,subjvar=subjvar,diffvars=diffvars,stratvars=stratvars,
                subscorevar=subscorevar,aggvar=aggvar,
                errorhandling=errorhandling,standardize=standardize)
  }
  
  # Get split counts
  chunks<-abs(ceiling(chunks))
  splitsizes<-rep(floor(splits/chunks),chunks)
  splitsizes[1]<-splitsizes[1] + splits %% chunks
  splitsizes<-splitsizes[splitsizes>0]
  chunks<-length(splitsizes)
  
  if(is.null(cluster)){
    # Run split chunks one by one
    
    outcomes<-list()
    if(verbose){ 
      cat("Running rapidsplit() in chunks...\n")
      pb = txtProgressBar(min = 0, max = length(splitsizes), initial = 0)  
    }
    for(i in seq_along(splitsizes)){
      if(verbose){ setTxtProgressBar(pb,i) }
      outcomes[[i]]<-
        rapidsplit(data=data,subjvar=subjvar,diffvars=diffvars,stratvars=stratvars,
                   subscorevar=subscorevar,aggvar=aggvar,
                   splits=splitsizes[i],
                   aggfunc=aggfunc,
                   errorhandling=errorhandling,
                   standardize=standardize,include.scores=include.scores,
                   verbose=FALSE,check=FALSE)
    }
    if(verbose){close(pb)}
  }else{
    # If cluster is requested but not given, make one
    if(is.numeric(cluster)){
      ncores<-min(parallel::detectCores(),cluster)
      cluster<-parallel::makeCluster(spec=ncores)
      on.exit(stopCluster(cluster))
    }
    
    # Run split chunks in parallel
    ss<-NULL
    registerDoParallel(cl=cluster,cores=ncores)
    outcomes<-foreach(ss=splitsizes,
                      .packages=c("rapidsplithalf"),
                      .multicombine=TRUE,
                      .inorder=FALSE) %dopar% {
                                    
      rapidsplit(data=data,subjvar=subjvar,diffvars=diffvars,stratvars=stratvars,
                 subscorevar=subscorevar,aggvar=aggvar,
                 splits=ss,
                 aggfunc=aggfunc,
                 errorhandling=errorhandling,
                 standardize=standardize,include.scores=include.scores,
                 verbose=FALSE,check=FALSE)
    }
  }
  
  # Merge content
  output<-list()
  output[["allcors"]]<-unlist(lapply(outcomes,\(x)x[["allcors"]]))
  output[["nobs"]]<-outcomes[[1]][["nobs"]]
  output[["r"]]<-cormean(output[["allcors"]],output[["nobs"]])
  if(include.scores){
    output[["scores"]][["half1"]]<-do.call(cbind,lapply(outcomes,\(x)x[["scores"]][["half1"]]))
    for(i in seq_along(splitsizes)){ outcomes[[i]][["scores"]][["half1"]]<-NULL }
    output[["scores"]][["half2"]]<-do.call(cbind,lapply(outcomes,\(x)x[["scores"]][["half2"]]))
    for(i in seq_along(splitsizes)){ outcomes[[i]][["scores"]][["half2"]]<-NULL }
  }
  output<-output[c("r","allcors","nobs","scores")]
  
  # Add class
  output<-structure(output,class="rapidsplit")
  
  # Return
  return(output)
}



