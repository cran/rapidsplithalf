
#include <Rcpp.h>
using namespace Rcpp;


//' Correlate two matrices by column
//' 
//' Correlate each column of 1 matrix with the same column in another matrix
//'
//' @param x,y Matrices whose values to correlate by column.
//' @returns \code{corByColumns()} and \code{corByColumns_mask()} return 
//' a numeric vector of correlations of each pair of columns.
//' 
//' \code{corStatsByColumns()} returns a list with named items:
//' - cormean: the aggregated correlation coefficient of all column pairs (see Details)
//' - allcors: the correlations of each column pair
//' - xvar: the column variances of matrix \code{x}
//' - yvar: the column variances of matrix \code{y}
//' - covar: the covariances of each column pair
//' 
//' @md
//' @details
//' The primary use for these functions is to rapidly compute the correlations
//' between two sets of split-half scores stored in matrix columns.
//' 
//' \code{corStatsByColumns} produces the mean correlation of all column-pairs
//' using the formula \code{mean(covariances) / sqrt(mean(col1variance) * mean(col2variance))}
//' 
//' This method is more accurate than [cormean()] and was suggested by 
//' prof. John Christie of Dalhousie University.
//' 
//' @export
//' @author Sercan Kahveci
//' @examples
//' m1<-matrix((1:9)+rnorm(9),ncol=3)
//' m2<-matrix((9:1)+rnorm(9),ncol=3)
//' corByColumns(m1,m2)
//' 
// [[Rcpp::export]]
NumericVector corByColumns(NumericMatrix x, NumericMatrix y){
 int tl = x.ncol();
 NumericVector out (tl);
 
 for(int i=0; i<tl; i++){
   NumericVector currmat1 = x.column(i);
   NumericVector currmat2 = y.column(i);
   
   LogicalVector incl = (!is_na(currmat1)) & (!is_na(currmat2));
   currmat1 = currmat1[incl];
   currmat2 = currmat2[incl];
   double currn = sum(incl);
   
   out[i] = (1/(currn-1)) *
     sum( ((currmat1-mean(currmat1))/sd(currmat1)) * 
     ((currmat2-mean(currmat2))/sd(currmat2)) );
 }
 return out;
}

//' @rdname corByColumns
//' @param mask Logical matrix marking which data points to include.
//' @export
//' @examples
//' mask<-1-diag(3)
//' corByColumns_mask(m1,m2,mask)
//' 
// [[Rcpp::export]]
NumericVector corByColumns_mask(NumericMatrix x, NumericMatrix y, LogicalMatrix mask){
 int tl = mask.ncol();
 NumericVector out (tl);
 
 for(int i=0; i<tl; i++){
   NumericVector currmat1 = x.column(i);
   NumericVector currmat2 = y.column(i);
   
   LogicalVector incl = mask.column(i);
   currmat1 = currmat1[incl];
   currmat2 = currmat2[incl];
   double currn = sum(incl);
   
   out[i] = (1/(currn-1)) *
     sum( ((currmat1-mean(currmat1))/sd(currmat1)) * 
     ((currmat2-mean(currmat2))/sd(currmat2)) );
 }
 return out;
}

//' @rdname corByColumns
//' @export
//' @examples
//' corStatsByColumns(m1,m2)
//' 
// [[Rcpp::export]]
List corStatsByColumns(NumericMatrix x, NumericMatrix y){
   int tl = x.ncol();
   NumericVector xvars (tl);
   NumericVector yvars (tl);
   NumericVector covs (tl);
   
   for(int i=0; i<tl; i++){
      NumericVector currmat1 = x.column(i);
      NumericVector currmat2 = y.column(i);
      
      LogicalVector incl = (!is_na(currmat1)) & (!is_na(currmat2));
      currmat1 = currmat1[incl];
      currmat2 = currmat2[incl];
      
      currmat1 = currmat1 - mean(currmat1);
      currmat2 = currmat2 - mean(currmat2);
      
      double currdf = sum(incl)-1;
      
      xvars[i] = sum(pow(currmat1,2))/currdf;
      yvars[i] = sum(pow(currmat2,2))/currdf;
      covs[i] = sum(currmat1*currmat2)/currdf;
   }
   List output = 
      List::create(Named("cormean") = mean(covs) / sqrt(mean(xvars)*mean(yvars)),
                   Named("allcors") = covs / sqrt(xvars*yvars),
                   Named("xvar") = xvars,
                   Named("yvar") = yvars,
                   Named("covar") = covs
                   );
   return output;
}
