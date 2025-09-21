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
# 6.7.5 Exercises
# 1. What does load() return? Why don’t you normally see these values?
?load()
#  loads objects saved to disk in .Rdata files by save()

# 2. What does write.table() return? What would be more useful?
# a table file on disk.

# 3. How does the chdir parameter of source() compare to with_dir()?
# Why might you prefer one to the other?
# with_dir fun based on this book:
with_dir <- function(dir, code) { # takes a path for a wd as its first argument
  old <- setwd(dir) # wd changed via setwd()
  on.exit(setwd(old))
  
  force(code)
}

# 4. Write a function that opens a graphics device, runs the supplied code,
# and closes the graphics device
# (always, regardless of whether or not the plotting code works).
save_png <- function(code){
  png("test.png")
  on.exit(dev.off(), add = T)
  code
}

save_png(with_dir)

# 5. We can use on.exit() to implement a simple version of capture.output().
capture.output2 <- function(code) {
  temp <- tempfile()
  on.exit(file.remove(temp), add = TRUE, after = TRUE)
  
  sink(temp)
  on.exit(sink(), add = TRUE, after = TRUE)
  
  force(code)
  readLines(temp)
}
capture.output2(cat("a", "b", "c", sep = "\n"))
#> [1] "a" "b" "c"

# Compare capture.output() to capture.output2(). How do the functions differ?
# What features have I removed to make the key ideas easier to see?
# How have I rewritten the key ideas so they’re easier to understand?


################################################################################
# 6.8.6 Exercises
# 1. Rewrite the following code snippets into prefix form:
1 + 2 + 3
'+' ('+' (1, 2), 3)

1 + (2 + 3)
'+'(1, '+'(2, 3))

if (length(x) <= 5) x[[5]] else x[[n]]

# 2. Clarify the following list of odd function calls:
x <- sample(replace = TRUE, 20, x = c(1:10, NA))
y <- runif(min = 0, max = 1, 20)
cor(m = "k", y = y, u = "p", x = x)
# weird
cor(x, y, use = "pairwise.complete.obs", method = "kendall")

# 3. Explain why the following code fails:
# modify from advanced R:
`modify<-` <- function(x, position, value) {
  x[position] <- value
  x
}

modify(get("x"), 1) <- 10
get("x") <- 2
#> Error: target of assignment expands to non-language object

# 4. Create a replacement function that modifies a random location in a vector.
`random <-` <- function(x, value) {
  idx <- sample(length(x), 1)
  x[idx] <- value
  x
}

vec <- c(10, 20, 30, 40, 50)
random(vec) <- 99  # Replace a random element with 99
print(vec)

random <- function(x, value) {
  idx <- sample(length(x), 1)
  x[idx] <- value
  x
}

random(vec, 99)  # Replace a random element with 99
print(vec)

# 5. Write your own version of + that pastes its inputs together
# if they are character vectors but behaves as usual otherwise.
# In other words, make this code work:
1 + 2
#> [1] 3

"a" + "b"
#> [1] "ab"

# override '+' as a function
'+' <- function(a, b = 0L){
  if (is.character(a) && is.character(b)) {
    paste0(a, b)
  } else {
    base::`+`(a, b)
  }
}
1 + 2
"a" + "b"

# 6. Create a list of all the replacement functions found in the base package.
# Which ones are primitive functions? (Hint: use apropos().)
# Replacement functions conventionally end on “<-.”
repls <- apropos("<-", where = TRUE, mode = "function")
head(repls, 10)

repls_base <- repls[names(repls) == length(search())]
repls_base

library(tidyverse)
repls_base_prim <- mget(repls_base, envir = baseenv()) %>%
  Filter(is.primitive, .) %>% 
  names()
repls_base_prim

# 7. What are valid names for user-created infix functions?
# names of infix functions are more flexible than regular R functions:
# they can contain any sequence of characters except “%.” 

# 7. Create an infix xor() operator.
?xor
'%xor%' <- function(a, b) {
  xor(a, b)
}
TRUE %xor% TRUE
FALSE %xor% TRUE

# 8. Create infix versions of the set fun intersect(), union(), & setdiff().
# You might call them %n%, %u%, and %/% to match conventions from mathematics.
# Intersect
'%n%' <- function(a, b){
  intersect(a, b)
}

# Union
'%u%' <- function(a, b){
  union(a, b)
}

# Setdiff
'%/%' <- function(a, b){
  setdiff(a, b)
}

x <- c("a", "b", "d")
y <- c("a", "c", "d")

x %u% y
#> [1] "a" "b" "d" "c"
x %n% y
#> [1] "a" "d"
x %/% y
#> [1] "b"







# 
