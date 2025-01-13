parameters <- R6::R6Class(
  classname = "Parameters",
  portable = F,
  public = list(
    p1 = 0.1,
    p2 = 3,
    p3 = 5,
    p4 = 0.3
  )
)

randomFun <- R6::R6Class(
  classname = "Random Function",
  portable = F,
  public = list(
    p = parameters$new(),
    obj1 = NULL,
    obj2 = NULL,
    obj3 = NULL,
    obj4 = array(),
    obj5 = array(),
    obj6 = array(),
    time = NULL,
    
    save = function(path) {
      saveRDS(self, path)
    },
    
    # Initialise is a function
    initialise = function(obj1){
      # if (is.null(obj1)) stop("obj1 must be set before initialization.")
      obj1 <<- obj1
      par1 <<- p$p1
      
      result1 <<- rgamma(obj1, shape = par1, rate = par1)
      result2 <<- rbinom(obj1, 1, p$p1)
    },
    
    # Other dynamics
    otherFunction = function(){
      #browser()
      result2 <<- ifelse(result2 == 1, 1-p$p4,1)
      result3 <<- result2+1
    },
    
    # Run Timestep 
    runTimestep = function(){
      otherFunction()
    },
    
    # Repeated call runTimestep for j timesteps (j months)
    runjMonths = function(j){
      results <- list()  # Initialize an empty list to store results
      
      for(i in 1:j) {
        runTimestep()
        
        results[[i]] <- list(
          j_month = i,        # Store the current month
          result2 = result2,   # Store result2
          result3 = result3    # Store result3
        )
      }
      
      return(results)  # Return the results after all timesteps
    },
    
    # Run the burn in for lb months (default is 1200 months or 100 years)
    runBurnin = function(lb = 1200){
      for(i in 1:lb) {
        otherFunction()
      }
      time <<- 0
    }
    )
  )