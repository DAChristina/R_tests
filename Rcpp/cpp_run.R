rm(list=ls())

library(Rcpp)

# Utilise Rcpp:
# Source: https://dirk.eddelbuettel.com/papers/useR2019_rcpp_tutorial.pdf

# Using evalCpp()
Rcpp::evalCpp("2 + 2")
Rcpp::evalCpp("std::numeric_limits<double>::max()")

# Using cppFunction()
Rcpp::cppFunction(
                  "int exampleCpp11() {
                      auto x = 10; // guesses type
                      return x;
                  }", 
                  plugins=c("cpp11")) ## turns on C++11
exampleCpp11()

Rcpp::cppFunction(
                  "int fun(int a, int b) {
                      return(a + b);
                  }")
fun(21, 21)

# Using sourceCpp
sourceCpp("Rcpp/cpp_test_1.cpp")
timesTwo(42) # timesTwo from cpp_test_1.cpp

sourceCpp("Rcpp/cpp_test_2.cpp")
sapply(0:10, cpp_f) # cpp_f from cpp_test_2.cpp

# Deploy the function in R:
Rcpp::cppFunction(
                  "int cpp_f(int n) {
                      if(n < 2) return(n);
                      return(cpp_f(n-1) + cpp_f(n-2));
                  }")
## Using it on first 11 arguments
sapply(0:10, cpp_f)

sourceCpp("Rcpp/cpp_test_3.cpp")
sapply(0:10, cpp_fib) # cpp_fib from cpp_test_3.cpp

