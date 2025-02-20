#include <Rcpp.h>
using namespace Rcpp;

// Source: https://dirk.eddelbuettel.com/papers/useR2019_rcpp_tutorial.pdf p. 42
// Consider a function defined as:
//f(n) such that  n                when n < 2
//                f(n-1) + f(n-2)  when n >= 2

// R implementation:
/*** R
f <- function(n){
  if(n < 2) return(n)
  return(f(n-1) + f(n-2))
} 
# sapply(0:10, f)
*/

// [[Rcpp::export]]
int cpp_f(int n) {
  if(n < 2) return(n);
  return(cpp_f(n-1) + cpp_f(n-2));
}

