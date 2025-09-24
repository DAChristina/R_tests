# Problem 2.2 ##################################################################
# Genetic drift simulation

genetic_drift <- function(p0, N, generations){
  # p0 = initial allele frequency
  # N = population size (diploid = 2N alleles)
  # generations = number of generations to simulate
  
  p <- numeric(generations + 1)
  p[1] <- p0
  
  for (t in 2:(generations + 1)) {
    # Sample 2N alleles from a binomial distribution
    k <- rbinom(1, 2*N, p[t-1])
    p[t] <- k/(2*N)
  }
  
  return(p)
}

set.seed(123)
N <- as.numeric(1e+05)
genera <- as.numeric(1e+04)

p <- genetic_drift(p0 = 0.6,
                   N = N,
                   generations = genera)

# Plot allele frequency trajectory
plot(0:genera, p, type="l", lwd=2, col="blue",
     xlab="Generation", ylab="Allele frequency",
     main="Genetic Drift Simulation")
abline(h=0.5, col="red", lty=2)


# Problem 2.2 ##################################################################