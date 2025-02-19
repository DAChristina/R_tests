#include <Rcpp.h>
using namespace Rcpp;

// Source: https://dirk.eddelbuettel.com/papers/useR2019_rcpp_tutorial.pdf p. 42
// Consider a fibonacci function defined as:
//f(n) such that  n               for n <= 1
//                f(n-1) + f(n-2) for n > 1

// R implementation:
/*** R
fibonacci <- function(n){
  if(n<=1) return(n)
  return(f(n-1) + f(n-2))
}
# sapply(0:10, fibonacci)
*/

// [[Rcpp::export]]
int cpp_fib(int n) {
  if(n<=1) return(n);
  return(cpp_fib(n-1) + cpp_fib(n-2));
}

