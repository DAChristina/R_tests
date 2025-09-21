# 2.1.1
# Given the following data frame, how do I create a new column called “3”
# that contains the sum of 1 and 2?
# You may only use $, not [[.
# What makes 1, 2, and 3 challenging as variable names?
df <- data.frame(runif(3), runif(3))
names(df) <- c(1, 2)
# df$3 <- runif(3)

# 2.1.2
# In the following code, how much memory does y occupy?
x <- runif(1e6)
y <- list(x, x, x)
length(x)
length(y)
length(y[[3]])

# 2.1.3
# On which line does a get copied in the following example?
a <- c(1, 5, 3, 2)
b <- a
b[[1]] <- 10
b

################################################################################
library(lobstr)
# dataframes
d1 <- data.frame(x = c(1, 5, 6), y = c(2, 4, 3))
d1
d2 <- d1
d2[, 2] <- d2[, 2] * 2
d2

x <- list(1:10)
x
x[[2]] <- x
x
x[[2]]
x[[2]][[1]]
x[[2]][[1]][[1]]
x[[2]][[1]][[2]]
x[[2]][[1]][2]

################################################################################
a <- runif(1e6)
obj_size(a)

b <- list(a, a)
obj_size(b)
obj_size(a, b)

b[[1]][[1]] <- 10
obj_size(b)
obj_size(a, b)

b[[2]][[1]] <- 10
obj_size(b)
obj_size(a, b)










