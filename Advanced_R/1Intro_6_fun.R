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
  
# 5.b. How many base functions have no arguments? What’s special about those functions?
  
# 5.c. How could you adapt the code to find all primitive functions?
  
# 5.d. What are the three important components of a function?
  
# 5.e. When does printing a function not show the environment it was created in?


