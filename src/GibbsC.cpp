
#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param N the number of samples
//' @return a random sample (x,y)
//' @examples
//' \dontrun{
//' GibbsC(100)
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix GibbsC(int N){
  NumericMatrix mat(N,2); 
  double x=1,y=0.5; /*初始化随机数*/
  double a=2,b=3; /*参数设置(double型)*/
  int n=20; /*参数设置(int型)*/
  
  for (int i=0; i<N; i++){
    x = rbinom(1,n,y)[0];
    y = rbeta(1,x+a,n-x+b)[0];
    mat(i,0) = x;
    mat(i,1) = y;
  }
  return mat;
}
