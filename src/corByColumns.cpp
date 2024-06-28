
#include <Rcpp.h>
using namespace Rcpp;


//' Correlate two matrices by column
//' 
//' Correlate each column of 1 matrix with the same column in another matrix
//'
//' @param x,y Matrices whose values to correlate by column.
//' @returns A numeric vector of correlations per column.
//' @details
//' The primary use for these functions is to rapidly compute the correlations
//' between two sets of split-half scores stored in matrix columns.
//' @export
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

