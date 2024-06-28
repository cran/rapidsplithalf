
#include <Rcpp.h>
using namespace Rcpp;


//' Exclude SD-based outliers
//' 
//' Different masks (columns of a logical matrix) are applied to the same input vector, 
//' and outliers in each resulting subvector are marked with \code{FALSE} in the mask.
//' 
//' @param x Vector to exclude outliers from.
//' @param mask A logical matrix determining which data points to include and which not to.
//' @param sdlim Standard deviation limit to apply; 
//' values beyond are classified as outliers and masked.
//' @returns An updated mask.
//' @export
//' @examples
//' x<-rnorm(50)
//' x[1]<-100
//' x[2]<-50
//' mask<-matrix(TRUE,ncol=3,nrow=50)
//' mask[1,2]<-FALSE
//' mask[2,3]<-FALSE
//' excludeOutliersByMask(x,mask)
// [[Rcpp::export]]
LogicalMatrix excludeOutliersByMask(NumericVector x, LogicalMatrix mask, double sdlim = 3){
 
 int outcols = mask.ncol();
 
 LogicalMatrix newmask = mask;
 
 for(int i=0; i<outcols; i++){
   LogicalVector currmask = mask.column(i);
   NumericVector maskedcol = x[currmask];
   
   double colmean = mean(maskedcol);
   double colsd = sd(maskedcol);
   
   LogicalVector newmaskedmask = 
     (maskedcol > (colmean - sdlim * colsd)) & 
     (maskedcol < (colmean + sdlim * colsd));
   
   currmask[currmask] = newmaskedmask;
   newmask(_,i) = currmask;
 }
 
 return newmask;
}





//' Exclude SD-based outliers in each matrix column
//' 
//' Generate or update a mask matrix based on outlyingness of values in each column.
//' @name OutlierMaskers
//' @param x Matrix in which to mark SD-based outliers by column.
//' @param sdlim Standard deviation limit to apply; 
//' values beyond are classified as outliers and masked.
//' @returns A logical matrix with outliers (and previously masked values) marked as \code{FALSE}.
//' @export
//' @examples
//' # Generate data with outliers
//' testmat<-matrix(rnorm(100),ncol=2)
//' testmat[1,]<-100
//' testmat[2,]<-50
//' 
//' # Detect outliers
//' maskOutliers(testmat)
//' 
// [[Rcpp::export]]
LogicalMatrix maskOutliers(NumericMatrix x, double sdlim = 3){
 
 int outrows = x.nrow();
 int outcols = x.ncol();
 
 LogicalMatrix newmask (outrows,outcols);
 
 for(int i=0; i<outcols; i++){
   NumericVector currcol = x.column(i);
   
   double colmean = mean(currcol);
   double colsd = sd(currcol);
   
   LogicalVector currnewmask = 
     (currcol > (colmean - sdlim * colsd)) & 
     (currcol < (colmean + sdlim * colsd));
   
   newmask(_,i) = currnewmask;
 }
 
 return newmask;
}


//' @rdname OutlierMaskers
//' @param mask A logical matrix determining which data points to include and which not to.
//' @examples
//' # Generate a mask
//' testmask<-matrix(TRUE,ncol=2,nrow=50)
//' testmask[1,1]<-FALSE
//' 
//' # Detect outliers with pre-existing mask
//' maskOutliersMasked(x=testmat, 
//'                    mask=testmask, sdlim = 3)
//' 
//' @export
// [[Rcpp::export]]
LogicalMatrix maskOutliersMasked(NumericMatrix x, LogicalMatrix mask, double sdlim = 3){
   
   int outrows = x.nrow();
   int outcols = x.ncol();
   
   LogicalMatrix newmask (outrows,outcols);
   
   for(int i=0; i<outcols; i++){
      NumericVector currcol = x.column(i);
      LogicalVector currmask = mask.column(i);
      NumericVector maskedcol = currcol[currmask];
      
      double colmean = mean(maskedcol);
      double colsd = sd(maskedcol);
      
      LogicalVector currnewmask = currmask & 
         (currcol > (colmean - sdlim * colsd)) & 
         (currcol < (colmean + sdlim * colsd));
      
      newmask(_,i) = currnewmask;
   }
   
   return newmask;
}



// [[Rcpp::export]]
NumericMatrix ReplaceErrorsFixed(NumericMatrix x,LogicalMatrix mask,
                                 LogicalVector error,double penalty){
   int xcol = x.ncol();
   
   for(int i=0; i<xcol; i++){
      NumericVector currcol=x.column(i);
      LogicalVector currmask=mask.column(i);
      NumericVector correctmasked=currcol[(!error) & currmask];
      double correctmean = mean(correctmasked);
      
      LogicalVector incorrectmask = error & currmask;
      currcol[incorrectmask]=correctmean+penalty;
      
      x(_,i) = currcol;
   }
   return x;
}
