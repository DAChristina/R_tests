f1 <- factor(letters)
f1
levels(f1) <- rev(levels(f1))
f1

# Two scalar arguments specify row and column sizes
x <- matrix(1:6, nrow = 2, ncol = 3)
x
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6

# One vector argument to describe all dimensions
y <- array(1:12, c(2, 3, 2))
y

# You can also modify an object in place by setting dim()
z <- 1:6
dim(z) <- c(3, 2)
z

################################################################################
# S3, S4 and R6 

# S3
create_patient_S3 <- function(ID, Name, Age, Serotype, Resistance) {
  obj <- list(ID = ID, Name = Name, Age = Age, Serotype = Serotype, Resistance = Resistance)
  class(obj) <- "PatientS3"
  return(obj)
}

# Define a print method for the S3 class
print.PatientS3 <- function(x) {
  cat("Patient ID:", x$ID, "\n",
      "Name:", x$Name, "\n",
      "Age:", x$Age, "\n",
      "Serotype:", x$Serotype, "\n",
      "Resistance:", x$Resistance, "\n")
}

# Create an S3 object
patient1 <- create_patient_S3(1, "Alice", 35, "3", "Resistant")

# Print the object (calls the custom print method)
print(patient1)


# S4
methods::setClass("PatientS4",
                  slots = list(
                    ID = "integer",
                    Name = "character",
                    Age = "numeric",
                    Serotype = "character",
                    Resistance = "character"
                    ))

# Define an S4 method for displaying patient info
methods::setMethod("show", "PatientS4",
                   function(object) {
                     cat("Patient ID:", object@ID, "\n",
                         "Name:", object@Name, "\n",
                         "Age:", object@Age, "\n",
                         "Serotype:", object@Serotype, "\n",
                         "Resistance:", object@Resistance, "\n")
                     })

# Create an S4 object
patient2 <- new("PatientS4", ID = 2L, Name = "Bob", Age = 45, Serotype = "19A", Resistance = "Susceptible")

# Show the object (calls the custom method)
patient2


# R6
PatientR6 <- R6::R6Class("PatientR6",
                     public = list(
                       ID = NULL,
                       Name = NULL,
                       Age = NULL,
                       Serotype = NULL,
                       Resistance = NULL,
                       
                       # Constructor
                       initialize = function(ID, Name, Age, Serotype, Resistance) {
                         self$ID <- ID
                         self$Name <- Name
                         self$Age <- Age
                         self$Serotype <- Serotype
                         self$Resistance <- Resistance
                       },
                       
                       # Method to print patient info
                       print_info = function() {
                         cat("Patient ID:", self$ID, "\n",
                             "Name:", self$Name, "\n",
                             "Age:", self$Age, "\n",
                             "Serotype:", self$Serotype, "\n",
                             "Resistance:", self$Resistance, "\n")
                       }
                     )
)

# Create an R6 object
patient3 <- PatientR6$new(3, "Charlie", 50, "6B", "Resistant")

# Call the method
patient3$print_info()

