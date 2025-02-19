rm(list=ls())

library(Rcpp)
sourceCpp("Rcpp/cpp_test_1.cpp")
timesTwo(42) # timesTwo from cpp_test_1.cpp
