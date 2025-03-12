# Quiz
# Answer the following questions to see if you can safely skip this chapter.
# You can find the answers in Section 6.9.

# 1. What are the three components of a function?
# body, arguments, environment
f02 <- function(x, y) {
  # A comment
  x + y
}

formals(f02)
body(f02)
environment(f02) # where you defined the function

attr(f02, "srcref") # source reference

# Primitive function (C)
sum
#> function (..., na.rm = FALSE)  .Primitive("sum")
`[`
#> .Primitive("[")
typeof(sum)
#> [1] "builtin"
typeof(`[`)
#> [1] "special"


# 2. What does the following code return?
x <- 10
f1 <- function(x) {
  function() {
    x + 10
  }
}
f1(1)()

# A lil bit modification:
f1 <- function(x) {
  function(n) {
    x + 10
  }
}

plus1 <- f1(1)
plus1(10)

# 3. How would you usually write this code?
`+`(1, `*`(2, 3))
1 + (2 * 3)

# 4. How could you make this call easier to read?
mean(, TRUE, x = c(1:10, NA))
mean(c(1:10, NA),
     na.rm = TRUE)

# 5. Does the following code throw an error when executed? Why or why not?
f2 <- function(a, b) {
    a * 10
  }
f2(10, stop("This is an error!"))

# 6. What is an infix function? How do you write it?
# What’s a replacement function? How do you write it?
# See Sections 6.8.3 and 6.8.4.
  
# 7. How do you ensure that cleanup action occurs
# regardless of how a function exits?
# You use on.exit(); see Section 6.7.4 for details.


################################################################################
# 6.2.5 Exercises
# 1. Given a name, like "mean", match.fun() lets you find a function.
# Given a function, can you find its name? Why doesn’t that make sense in R?
match.fun("mean")
# No, unless the function is constructed as an object.

# 2. It’s possible (though typically not useful) to call an anonymous function.
# Which of the two approaches below is correct? Why?
function(x) 3()
#> function(x) 3()
(function(x) 3)()
#> [1] 3
(function(x) 3)()
# Coz' x is defined as 3

# 3. A good rule of thumb is that an anonymous function should fit on one line
# and shouldn’t need to use {}. Review your code.
# Where could you have used an anonymous function instead of a named function?
# Where should you have used a named function instead of an anonymous function?
# Anonymous: needed once, defining a function as an object is not necessary.
# Named: when a function is needed many times.

# 4. What function allows you to tell if an object is a function?
# What function allows you to tell if a function is a primitive function?
attr(f02, "srcref") # source reference
typeof(sum)

# 5. This code makes a list of all functions in the base package.
objs <- mget(ls("package:base", all = TRUE), inherits = TRUE)
funs <- Filter(is.function, objs)
# Use it to answer the following questions:
# 5.a. Which base function has the most arguments?
n_args <- funs %>% 
  purrr::map(formals) %>%
  purrr::map_int(length) %>% 
  sort(decreasing = T) # n_args is vector
head(n_args)
glimpse(n_args)

# 5.b. How many base functions have no arguments? What’s special about those functions?
sum(n_args == 0)
n_args2 <- funs %>% 
  purrr::discard(is.primitive) %>% 
  purrr::map(formals) %>% 
  purrr::map_int(length)
sum(n_args2 == 0)
# Most of the functions with no arguments are actually primitive functions.

# 5.c. How could you adapt the code to find all primitive functions?
funs_primitive <- Filter(is.primitive, objs)  
length(funs_primitive)

# 5.d. What are the three important components of a function?
# body, arguments, environment
f02 <- function(x, y) {
  # A comment
  x + y
}

formals(f02)
body(f02)
environment(f02) # where you defined the function

# 5.e. When does printing a function not show the environment it was created in?
# Primitive functions and functions created in the global environment.


################################################################################
# 6.4.5 Exercises
# 1. What does the following code return? Why?
# Describe how each of the three c’s is interpreted.
c <- 10
c(c = c)

# 2. What are the four principles that govern how R looks for values?
# From the deepest function nested inside many layers of another function up to global enviro.

# 3. What does the following function return?
# Make a prediction before running the code yourself.
f <- function(x) {
  f <- function(x) {
    f <- function() {
      x ^ 2
    }
    f() + 1
  }
  f(x) * 2
}
f(10)


################################################################################
# 6.5.4 Exercises
# 1. What important property of && makes x_ok() work?
x_ok <- function(x) {
  !is.null(x) && length(x) == 1 && x > 0
}

x_ok(NULL)
#> [1] FALSE
x_ok(1)
#> [1] TRUE
x_ok(1:3)
#> [1] FALSE

# What is different with this code? Why is this behaviour undesirable here?
x_ok <- function(x) {
  !is.null(x) & length(x) == 1 & x > 0
}

x_ok(NULL)
#> logical(0)
x_ok(1)
#> [1] TRUE
x_ok(1:3)
#> [1] FALSE FALSE FALSE

# 2. What does this function return? Why? Which principle does it illustrate?
f2 <- function(x = z) {
  z <- 100
  x
}
f2()
# lazy evaluation

# 3. What does this function return? Why? Which principle does it illustrate?
y <- 10
f1 <- function(x = {y <- 1; 2}, y = 0) {
  c(x, y)
}
f1()
y
# Name masking

# 4. In hist(), the default value of xlim is range(breaks), 
# the default value for breaks is "Sturges", and
range("Sturges")
#> [1] "Sturges" "Sturges"

# Explain how hist() works to get a correct xlim value.

# 5. Explain why this function works. Why is it confusing?
show_time <- function(x = stop("Error!")) {
  stop <- function(...) Sys.time()
  print(x)
}
show_time()
#> [1] "2021-02-21 19:22:36 UTC"

# 6. How many arguments are required when calling library()?
# No mandatory arguments needed!


################################################################################





# 
