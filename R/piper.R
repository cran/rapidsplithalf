
#' A balanced split-half generator
#' 
#' Generates split-half indices that can be stratified by multiple subgroup variables
#' while guaranteeing near-equal numbers of trials in both halves.
#'
#' @param data A dataset to generate split-halves from.
#' @param subsetvars Variables identifying subgroups that must be individually split 
#' into equally sized halves, e.g. participant number and experimental condition.
#' @param stratvars Variables identifying subgroups that are nested within the subsetvars, 
#' and must be split as fairly as possible, while preserving the equal size of 
#' the two halves of each subset identified by the subsetvars, e.g. stimulus ID.
#' @param splits How many splits to generate.
#' @param verbose Display progress bar?
#'
#' @return A logical \code{matrix} in which each row represents a row of the input dataset,
#' and each column represents a single split. 
#' @export
#'
#' @examples
#' data(foodAAT)
#' mysplits<-generateSplits(data=foodAAT,
#'                          subsetvars=c("subjectid","is_pull","is_target"),
#'                          stratvars="stimid",
#'                          splits=1)
#' half1<-foodAAT[ mysplits[,1],]
#' half2<-foodAAT[!mysplits[,1],]
#' 
generateSplits<-function(data,subsetvars,stratvars=NULL,splits,verbose=TRUE){
  
  # Arrange properly
  runorder<-do.call(order,data[,c(subsetvars,stratvars),drop=FALSE])
  arr.ds<- cols2ids(data[runorder,c(subsetvars,stratvars),drop=FALSE])
  
  subsetvec<-do.call(paste,arr.ds[,subsetvars,drop=FALSE])
  subsets<-unique(subsetvec)
  
  # Stratify by split stratum
  origkeys<-setNames(vector(mode="list",length=length(subsets)),subsets)
  if(verbose){ pb = txtProgressBar(min = 0, max = length(subsets), initial = 0) }
  for(ss in subsets){
    if(verbose){ setTxtProgressBar(pb,which(ss==subsets)) }
    iterds<-arr.ds[subsetvec==ss,c(stratvars),drop=FALSE]
    iterrle<-rle(do.call(paste,args=iterds))
    grsizes<-iterrle$lengths
    if(length(grsizes)==0){grsizes<-nrow(iterds)}
    origkeys[[ss]]<-stratifiedItersplits(splits=splits, groupsizes=grsizes)
  }
  origkeys<-do.call(rbind,origkeys)
  if(verbose){ close(pb) }
  
  # return
  backorder<-order(runorder)
  return(origkeys[backorder,,drop=FALSE])
}

applyAggregator<-function(data,subsetvars,aggvar,aggfunc,mask,verbose=TRUE){
  subsetvec<-do.call(paste,cols2ids(data[,subsetvars,drop=FALSE]))
  runorder<-order(subsetvec)
  subsetvec<-subsetvec[runorder]
  data<-data[runorder,]
  mask<-mask[runorder,]
  
  subsets<-unique(subsetvec)
  aggs<-matrix(ncol=ncol(mask),nrow=length(subsets))
  if(verbose){ pb = txtProgressBar(min = 0, max = length(subsets), initial = 0) }
  for(i in seq_along(subsets)){
    if(verbose){ setTxtProgressBar(pb,i) }
    aggs[i,]<-
      aggfunc(x=data[[aggvar]][subsetvec==subsets[i]],
              mask=mask[subsetvec==subsets[i],,drop=FALSE])
  }
  if(verbose){ close(pb) }
  list(indices=unique(data[,subsetvars]),aggs=aggs)
}

applyAggregator2<-function(data,subsetvars,aggvar,aggfunc,masks,verbose=TRUE){
  outputs<-list()
  for(i in seq_along(masks)){
    outputs[[i]]<-applyAggregator(data,subsetvars,aggvar,aggfunc,masks[[i]],verbose)
  }
  outputs
}

reduceRows<-function(indices,aggs,groupvars,collapsevar,
                     action=c("subtract","add","divide","multiply"),
                     highlow=TRUE){
  action<-match.arg(action)
  
  subsetvec<-do.call(paste,indices[,c(groupvars,collapsevar),drop=FALSE])
  if(is.unsorted(subsetvec)){
    runorder<-order(subsetvec)
    indices<-indices[runorder,]
    aggs<-aggs[runorder,]
  }
  
  groupvec<-do.call(paste,indices[,c(groupvars),drop=FALSE])
  
  # add
  if(action=="add" | action=="multiply"){
    subsets<-unique(groupvec)
    newagg<-matrix(ncol=ncol(aggs),nrow=length(subsets))
    agger<-ifelse(action=="add",colSums,colProds)
    for(i in seq_along(subsets)){
      newagg[i,]<-agger(aggs[groupvec==subsets[i],])
    }
    newindices<-unique(indices[,c(groupvars),drop=FALSE])
  }else{
    # Checks
    condcounts<-table(groupvec)
    if(!all(sapply(unique(groupvec),
                   function(x){length(unique(indices[groupvec==x,collapsevar]))}) == 2)){
      stop("More or less than 2 levels of collapse variable detected.")
    }
    
    if(any(condcounts!=2)){
      stop("In ",sum(condcounts!=2)," case(s) there are less or more than 2 instances",
           " of a permutation of grouping variables.",
           " Cannot unambiguously match the collapse variable.")
    }
    if(highlow){
      mat1<-aggs[indices[[collapsevar]]==max(indices[[collapsevar]]),]
      mat2<-aggs[indices[[collapsevar]]==min(indices[[collapsevar]]),]
    }else{
      mat1<-aggs[indices[[collapsevar]]==min(indices[[collapsevar]]),]
      mat2<-aggs[indices[[collapsevar]]==max(indices[[collapsevar]]),]
    }
    if(action=="subtract"){ newagg<-mat1-mat2 }else
    if(action=="divide"){ newagg<-mat1/mat2 }
    newindices<-indices[indices[[collapsevar]]==max(indices[[collapsevar]]),
                        colnames(indices)!=collapsevar]
  }
  list(indices=newindices,aggs=newagg)
}
