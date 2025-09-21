library(rlang)
# Quiz
# Answer the following questions to see if you can safely skip this chapter.
# You can find the answers in Section 7.7.

# 1. List at least three ways that an environment differs from a list.
# every object in an environment must have a name;
# order doesn’t matter;
# environments have parents;
# environments have reference semantics.

# 2. What is the parent of the global environment?
# What is the only environment that doesn’t have a parent?
# The last package that have loaded.
# Doesn’t have a parent = empty environment.

# 3. What is the enclosing environment of a function? Why is it important?
# The enviro where it was created --> determines where a fun looks for var.

# 4. How do you determine the environment from which a function was called?
# use caller_env() or parent.frame()

# 5. How are <- and <<- different?
# <- always creates a binding in the current environment;
# <<- rebinds an existing name in a parent of the current environment.


################################################################################
#  7.2.7 Exercises
# 1. List three ways in which an environment differs from a list.
# Enviro is kinda like a container, similar to list() but not a list coz:
# order dosen't matter
# enviro have parents
# every object in an enviro must have a name
# enviro have reference semantics

# (environments can only be compared via identical(); not with ==)
# (environments can contain themselves)

# 2. Create an environment as illustrated by this picture
# (enviro that contains itself).
e1 <- rlang::env()
e1$loop <- e1

env_print(e1)
lobstr::ref(e1)

# 3. Create a pair of environments as illustrated by this picture
# (two environments contain each other)
e1 <- rlang::env()
e2 <- rlang::env()

e1$loop <- e2
e2$dedoop <- e1

# 4. Explain why e[[1]] and e[c("a", "b")] don’t make sense
# when e is an environment.
# The first option's elements of an enviro are not ordered.
# The second option would return two objects at the same time.
# What data structure would they be contained inside?

# 5. Create a version of env_poke() that will only bind new names,
# never re-bind old names. Some programming languages only do this,
# and are known as single assignment languages.
?rlang::env_poke()

e3 <- new.env()
env_poke(e3, "a", 100)
e3$a
#> [1] 100
env_poke(e3, "a", 200)
e3$a
#> [1] 200

# 6. What does this function do?
rebind <- function(name, value, env = caller_env()) {
  if (identical(env, empty_env())) {
    stop("Can't find `", name, "`", call. = FALSE)
  } else if (env_has(env, name)) {
    env_poke(env, name, value)
  } else {
    rebind(name, value, env_parent(env))
  }
}
rebind("a", 10)
#> Error: Can't find `a`
a <- 5
rebind("a", 10)
a
#> [1] 10
# How does it differ from <<- and why might you prefer it?
# rebind() will only carry out an assignment when it finds an existing binding;
# unlike <<- it will never create a new one in the global environment.


################################################################################
# 7.3.1 Exercises 
# 1. Modify where() to return all environments that contain a binding for name.
# Carefully think through what type of object the function will need to return.
where <- function(name, env = caller_env()) {
  if (identical(env, empty_env())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)
  } else if (env_has(env, name)) {
    # Success case
    env
  } else {
    # Recursive case
    where(name, env_parent(env))
  }
}
e1a <- rlang::env(empty_env(), a = 1, b = 2)
e1b <- rlang::env(e1a, b = 10, c = 11)
e1c <- rlang::env(e1b, a = 12, d = 13)
where("a", e1c)

where2 <- function(name, env = caller_env(), results = list()) {
  if (identical(env, empty_env())) {
    # Base case
    results
  } else {
    # Recursive case
    if (env_has(env, name)) {
      results <- c(results, env)
    }
    where2(name, env_parent(env), results)
  }
}

where2("a", e1c)

# 2. Write a function called fget() that finds only function objects.
# It should have two arguments, name and env,
# and should obey the regular scoping rules for functions:
# if there’s an object with a matching name that’s not a function,
# look in the parent.
# For an added challenge, also add an inherits argument which controls
# whether the function recurses up the parents or only looks in one environment.
fget <- function(name, env = caller_env(), inherits = TRUE) {
  # Base case
  if (env_has(env, name)) {
    obj <- env_get(env, name)
    
    if (is.function(obj)) {
      return(obj)
    }
  }
  
  if (identical(env, emptyenv()) || !inherits) {
    stop("Could not find a function called \"", name, "\".",
         call. = FALSE
    )
  }
  
  # Recursive Case
  fget(name, env_parent(env))
}

# Test
mean <- 10
fget("mean", inherits = TRUE)


################################################################################
search()
rlang::search_envs()
# 7.4.5 Exercises 
# 1. How is search_envs() different from env_parents(global_env())?
# a chain of environments containing exported functions of attached packages.
# env_parents(global_env()) will list all the ancestors of global,
# therefore the global environment itself is not included.
  
# 2. Draw a diagram that shows the enclosing environments of this function:
f1 <- function(x1) {
  f2 <- function(x2) {
    f3 <- function(x3) {
      x1 + x2 + x3
    }
    f3(3)
  }
  f2(2)
}
f1(1)
# f1 in parent (global) but f2 will only be created if f1 is executed etc.

# 3. Write an enhanced version of str() that provides more information
# about functions.
# Show where the function was found and what environment it was defined in.
# ?????









