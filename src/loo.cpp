#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector loo_sum(NumericVector x){
  return sum(x)-x;
}

// [[Rcpp::export]]
NumericVector loo_mean(NumericVector x){
  return loo_sum(x)/(x.length()-1);
}

// [[Rcpp::export]]
NumericVector loo_var(NumericVector x){
  NumericVector numer = loo_sum(pow(x,2))-pow(loo_mean(x),2)*(x.length()-1);
  return numer/(x.length()-2);
}

// [[Rcpp::export]]
NumericVector loo_cov(NumericVector x, NumericVector y){
  NumericVector loosx = loo_sum(x);
  NumericVector loosy = loo_sum(y);
  return ((sum(x*y)-x*y)-loosx*loosy/(x.length()-1))/(x.length()-2);
}

// [[Rcpp::export]]
NumericVector loo_cor(NumericVector x, NumericVector y){
  int lmin1 = x.length()-1;
  
  NumericVector loosx = loo_sum(x);
  NumericVector loosy = loo_sum(y);
  
  NumericVector loocov ((sum(x*y)-x*y)-loosx*loosy/lmin1);
  NumericVector looxvar (loo_sum(pow(x,2))-pow(loosx,2)/lmin1);
  NumericVector looyvar (loo_sum(pow(y,2))-pow(loosy,2)/lmin1);
  
  return loocov/(sqrt(looxvar)*sqrt(looyvar));
}

// [[Rcpp::export]]
List looCorStatsByColumns(NumericMatrix x, NumericMatrix y){
  int xcol=x.ncol();
  int xrow=x.nrow();
  NumericMatrix looxvars (xrow,xcol);
  NumericMatrix looyvars (xrow,xcol);
  NumericMatrix loocovs (xrow,xcol);
  
  for(int i=0; i<xcol; i++){
    NumericVector currmat1 = x.column(i);
    NumericVector currmat2 = y.column(i);
    
    LogicalVector incl = (!is_na(currmat1)) & (!is_na(currmat2));
    currmat1 = currmat1[incl];
    currmat2 = currmat2[incl];
    
    looxvars(_,i) = loo_var(currmat1);
    looyvars(_,i) = loo_var(currmat2);
    loocovs(_,i) = loo_cov(currmat1,currmat2);
  }
  
  // create averages
  NumericVector meanxvar (xrow);
  NumericVector meanyvar (xrow);
  NumericVector meancovar (xrow);
  for(int i=0; i<xrow; i++){
    meanxvar[i] = mean(looxvars(i,_));
    meanyvar[i] = mean(looyvars(i,_));
    meancovar[i] = mean(loocovs(i,_));
  }
  
  List output = 
    List::create(Named("meanloocors") = meancovar / sqrt(meanxvar*meanyvar),
                 Named("meanlooxvars") = meanxvar,
                 Named("meanlooyvars") = meanyvar,
                 Named("meanloocovars") = meancovar,
                 Named("loocors") = loocovs / sqrt(looxvars*looyvars),
                 Named("looxvar") = looxvars,
                 Named("looyvar") = looyvars,
                 Named("loocovar") = loocovs
    );
  return output;
}

