
#include <Rcpp.h>
using namespace Rcpp;

//' stratifiedItersplits
//' 
//' Generate stratified splits for a single participant
//' 
//' This equally splits what can be equally split within groups.
//' Then it randomly splits all the leftovers to ensure near-equal split sizes.
//' This function is moreso used internally, 
//' but you can use it if you know what you are doing.
//'
//' @param splits Number of iterations.
//' @param groupsizes An integer vector of how many RTs per group need to be stratified.
//' @returns A matrix with zeroes and ones. Each column is a random split.
//' @examples
//' 
//' # We will create splits stratified by stimulus for a single participant
//' data(foodAAT)
//' currdata<-foodAAT[foodAAT$subjectid==3,]
//' currdata$stratfactor<-interaction(currdata$is_pull,currdata$is_target,currdata$stimid)
//' currdata<-currdata[order(currdata$stratfactor),]
//' groupsizes<-rle(as.character(currdata$stratfactor))$lengths
//' 
//' mysplits<-stratifiedItersplits(splits=1000,groupsizes=groupsizes)
//' 
//' # Now the data can be split with the values from any column.
//' half1<-currdata[mysplits[,1]==1,]
//' half2<-currdata[mysplits[,1]==0,]
//' 
//' # Or the split objects can be used as masks for the aggregation functions in this package
//' meansByMask(x=currdata$RT,mask=mysplits==1)
//'  
//' @export
// [[Rcpp::export]]
LogicalMatrix stratifiedItersplits(int splits, IntegerVector groupsizes){
  
  // Get info on arguments and create output matrix
  int sampsize = sum(groupsizes);
  int groupcount = groupsizes.length();
  LogicalMatrix itermat (sampsize,splits);
  
  // count uneven subgroups and prepare an empty vector 
  // to hold half assignments for their leftovers
  int n_uneven = 0;
  for(int i = 0; i < groupcount; i++){
    n_uneven += groupsizes[i] % 2;
  }
  IntegerVector uneven_sampvec(n_uneven);
  
  // Create simple vectors for use in assignment to halves
  IntegerVector zeroone = {0,1};
  IntegerVector onezero = {1,0};
  IntegerVector sampvec_zeroone = rep_len(zeroone,n_uneven);
  IntegerVector sampvec_onezero = rep_len(onezero,n_uneven);
  
  // loop over iterations (columns in output matrix)
  for(int i = 0; i < splits; i++) {
    
    // Generate an assignments vector for all the leftovers 
    // after all evenly splittable values have been split
    bool sampvec_order = sample(onezero,1,false);
    if(sampvec_order){
      uneven_sampvec = sample(sampvec_onezero,n_uneven,false);
    }else{
      uneven_sampvec = sample(sampvec_zeroone,n_uneven,false);
    }
    
    // Assign all to halves
    int min_index = 0;
    int uneven_index = 0;
    for(int j = 0; j < groupcount; j++){
      int currgroupsize = groupsizes[j];
      IntegerVector sampvec = rep_len(zeroone, currgroupsize);
      if(currgroupsize % 2 == 1){
        sampvec[0] = uneven_sampvec[uneven_index];
        uneven_index++;
      }
      sampvec = sample(sampvec, currgroupsize, false);
      
      for(int k = 0; k < currgroupsize; k++){
        itermat(min_index+k,i)=sampvec[k];
      }
      min_index += currgroupsize;
    }
  }
  
  return itermat;
}


//' Bootstrap Weights
//' 
//' Create a matrix of bootstrap samples expressed as frequency weights
//'
//' @param size Number of values to bootstrap
//' @param times Number of bootstraps
//' @returns A matrix with bootstrap samples expressed as frequency weights. 
//' Each column represents a single bootstrap iteration and each row represents a case.
//' 
//' @examples
//' # Rapidly compute a bootstrapped median to obtain its standard error
//' myweights<-bootstrapWeights(size=50, times=100)
//' meds<-mediansByWeight(x=rnorm(50),weights=myweights)
//' # SE
//' sd(meds)
//' 
//' @export
// [[Rcpp::export]]
IntegerMatrix bootstrapWeights(int size, int times){
  IntegerMatrix out (size,times);
  
  for(int i=0; i<times; ++i){
    IntegerVector currsamples = sample(size,size,true,R_NilValue,false);
    
    for(int j=0; j<size; ++j){
      out(currsamples(j),i)++;
    }
  }
  
  return out;
}


