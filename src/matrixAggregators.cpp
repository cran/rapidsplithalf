

#include <Rcpp.h>
using namespace Rcpp;



//' Fast matrix column aggregators
//' @name colAggregators
//' 
//' @param x A numeric matrix to compute column aggregates of.
//' 
//' @examples
//' x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
//' colMedians(x)
//' 
//' @seealso \link[base]{colMeans}, \link{mediansByMask}, \link{maskAggregators}
//' @return A numeric vector representing values aggregated by column.
//' @export
// [[Rcpp::export]]
NumericVector colMedians(NumericMatrix x){
 int tl = x.ncol();
 NumericVector out (tl);
 
 for(int i=0; i<tl; i++){
   NumericVector currcol = x.column(i);
   out[i]=median(currcol);
 }
 return out;
}

//' @rdname colAggregators
//' @export
//' @examples
//' colProds(x)
//' 
// [[Rcpp::export]]
NumericVector colProds(NumericMatrix x){
   int tl = x.ncol();
   int xr = x.nrow();
   NumericVector out (tl);
   out = rep(1,tl);
   
   for(int i=0; i<xr; i++){
      out = out * x.row(i);
   }
   return out;
}

//' @rdname colAggregators
//' @export
//' @examples
//' colSds(x)
//' 
// [[Rcpp::export]]
NumericVector colSds(NumericMatrix x){
   int tl = x.ncol();
   NumericVector out (tl);
   
   for(int i=0; i<tl; i++){
      NumericVector currcol = x.column(i);
      out[i]=sd(currcol);
   }
   return out;
}
 
//' @rdname colAggregators
//' @param mask A logical matrix determining which data points to include in 
//' the column-wise aggregations.
//' 
//' @examples
//' mask<-cbind(rep(c(TRUE,FALSE),4),
//'             rep(c(TRUE,FALSE),each=4))
//' colMediansMasked(x,mask)
//' 
//' @export
// [[Rcpp::export]]
NumericVector colMediansMasked(NumericMatrix x, LogicalMatrix mask){
   int tl = x.ncol();
   NumericVector out (tl);
   
   for(int i=0; i<tl; i++){
      LogicalVector currmask = mask.column(i);
      NumericVector currcol = x.column(i);
      NumericVector maskedcol = currcol[currmask];
      out[i] = median(maskedcol);
   }
   return out;
}

//' @rdname colAggregators
//' @export
//' @examples
//' colMeansMasked(x,mask)
//' 
// [[Rcpp::export]]
NumericVector colMeansMasked(NumericMatrix x, LogicalMatrix mask){
   int tl = x.ncol();
   NumericVector out (tl);
   
   for(int i=0; i<tl; i++){
      LogicalVector currmask = mask.column(i);
      NumericVector currcol = x.column(i);
      NumericVector maskedcol = currcol[currmask];
      out[i] = mean(maskedcol);
   }
   return out;
}

//' @rdname colAggregators
//' @export
//' @examples
//' colSdsMasked(x,mask)
//' 
// [[Rcpp::export]]
NumericVector colSdsMasked(NumericMatrix x, LogicalMatrix mask){
   int tl = x.ncol();
   NumericVector out (tl);
   
   for(int i=0; i<tl; i++){
      LogicalVector currmask = mask.column(i);
      NumericVector currcol = x.column(i);
      NumericVector maskedcol = currcol[currmask];
      out[i]=sd(maskedcol);
   }
   return out;
}




//' @title Multi-mask/weight based aggregators
//' @name maskAggregators
//' @description Methods to aggregate the same vector with different masks or frequency weights.
//' Useful for fast bootstrapping or split-half scoring.
//' A single aggregate value of \code{x} is computed for each column of the mask or weight matrix.
//' 
//' @param x A vector to aggregate over with different masks or weights.
//' @param mask Logical matrix where each column represents a separate vector of masks 
//' to aggregate \code{x} with. Only values marked \code{TRUE} are included in the aggregation.
//' 
//' @examples
//' 
//' # Demonstration of mediansByMask()
//' x<-1:6
//' mask<-rbind(c(TRUE,FALSE,FALSE),
//'             c(TRUE,FALSE,FALSE),
//'             c(FALSE,TRUE,FALSE),
//'             c(FALSE,TRUE,FALSE),
//'             c(FALSE,FALSE,TRUE),
//'             c(FALSE,FALSE,TRUE))
//' mediansByMask(x,mask)
//' 
//' # Compute split-halves for a single 
//' # participant, stratified by stimulus
//' data(foodAAT)
//' currdata<-foodAAT[foodAAT$subjectid==3,]
//' currdata$stratfactor<-
//'   interaction(currdata$is_pull,
//'               currdata$is_target,
//'               currdata$stimid)
//' currdata<-currdata[order(currdata$stratfactor),]
//' groupsizes<-
//'   rle(as.character(currdata$stratfactor))$lengths
//' mysplits<-
//'   stratifiedItersplits(splits=1000,
//'                        groupsizes=groupsizes)
//' 
//' # Median for half 1
//' mediansByMask(currdata$RT,mysplits==1)
//'  
//' @seealso \link{colMedians}, \link{colAggregators}, \link{generateSplits}
//' @return a vector with each value representing an aggregate of the same single input vector 
//' but with different masks or frequency weights applied.
//' @export
// [[Rcpp::export]]
NumericVector mediansByMask(NumericVector x, LogicalMatrix mask){
 int tl = mask.ncol();
 NumericVector out (tl);
 
 for(int i=0; i<tl; i++){
   LogicalVector currmask = mask.column(i);
   NumericVector maskedcol = x[currmask];
   out[i] = median(maskedcol);
 }
 return out;
}

//' @rdname maskAggregators
//' @examples
//' #How to use meansByMask()
//' meansByMask(x,mask)
//' sd(meansByMask(currdata$RT,mysplits==1))
//' 
//' @export
// [[Rcpp::export]]
NumericVector meansByMask(NumericVector x, LogicalMatrix mask){
   int tl = mask.ncol();
   NumericVector out (tl);
   
   for(int i=0; i<tl; i++){
      LogicalVector currmask = mask.column(i);
      NumericVector maskedcol = x[currmask];
      out[i] = mean(maskedcol);
   }
   return out;
}


//' @rdname maskAggregators
//' @export
//' @examples
//' # How to use sdsByMask() to compute
//' # mask-based D-scores
//' meansByMask(currdata$RT,mysplits==1) / 
//'   sdsByMask(currdata$RT,mysplits==1)
//' 
// [[Rcpp::export]]
NumericVector sdsByMask(NumericVector x, LogicalMatrix mask){
   int tl = mask.ncol();
   NumericVector out (tl);
   
   for(int i=0; i<tl; i++){
      LogicalVector currmask = mask.column(i);
      NumericVector maskedcol = x[currmask];
      maskedcol=maskedcol-mean(maskedcol);
      out[i]=sqrt((sum(pow(maskedcol,2)))/(maskedcol.length()-1));
   }
   return out;
}

//' @rdname maskAggregators
//' @param weights Integer matrix where each column represents frequency weights 
//' to weight the aggregation by.
//' @examples
//' # Compute the bootstrapped 
//' # standard error of a median
//' weights<-
//'   bootstrapWeights(size=nrow(currdata),
//'                    times=1000)
//' bootmeds<-mediansByWeight(currdata$RT,weights)
//' sd(bootmeds) # bootstrapped standard error
//' 
//' @export
// [[Rcpp::export]]
NumericVector mediansByWeight(NumericVector x,IntegerMatrix weights){
 int iters=weights.ncol();
 int xlen=x.size();
 NumericVector out (iters);
 
 // sort
 IntegerVector idx = seq_along(x) - 1;
 std::sort(idx.begin(), idx.end(), [&](int i, int j){return x[i] < x[j];});
 x=x[idx];
 
 for(int i=0; i<iters; i++){
   IntegerVector currweights=weights.column(i);
   currweights=currweights[idx];
   
   // remove empty
   LogicalVector isEmpty = currweights==0;
   NumericVector currvalues=x[!isEmpty];
   currweights=currweights[!isEmpty];
   
   double halfsum=sum(as<NumericVector>(currweights))/2;
   
   // find median
   IntegerVector meds (0);
   double counter=0;
   for(int j=0; j<xlen; j++){
     bool is_med=(halfsum>=counter) & (halfsum<=counter+currweights[j]);
     if(is_med){
       meds.push_back(j);
     }
     if((meds.size()==2) || ((meds.size()==1) & (!is_med))){
       break;
     }
     counter=counter+currweights[j];
   }
   out[i]=mean(as<NumericVector>(currvalues[meds]));
 }
 
 return out;
}

//' @rdname maskAggregators
//' @export
//' @examples
//' # Compute the bootstrapped 
//' # standard error of a mean
//' bootmeans<-meansByWeight(currdata$RT,weights)
//' sd(bootmeans) # bootstrapped standard error
//' # exact standard error for comparison
//' sd(currdata$RT)/sqrt(length(currdata$RT)) 
//' 
// [[Rcpp::export]]
NumericVector meansByWeight(NumericVector x, IntegerMatrix weights){
 int tl = weights.ncol();
 NumericVector out (tl);
 
 for(int i=0; i<tl; i++){
   IntegerVector currweights = weights.column(i);
   out[i] = sum(x*as<NumericVector>(currweights))/sum(currweights);
 }
 return out;
}

//' @rdname maskAggregators
//' @export
//' @examples
//' # Use sdsByWeight to compute bootstrapped D-scores
//' bootsds<-sdsByWeight(currdata$RT,weights)
//' # bootstrapped standard error of D-score
//' sd(bootmeans/bootsds)
//' 
// [[Rcpp::export]]
NumericVector sdsByWeight(NumericVector x, NumericMatrix weights){
 int tl = weights.ncol();
 NumericVector out (tl);
 
 for(int i=0; i<tl; i++){
   NumericVector currweights = weights(_,i);
   
   double wtmean = sum(x*currweights)/sum(currweights);
   out[i]=sqrt(sum(currweights*pow(x-wtmean,2))/(sum(currweights)-1));
 }
 return out;
}
