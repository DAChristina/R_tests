# Problem 1.4 ##################################################################
# 1. homo & hetero as a function of p (allele freq); Hardy-Weinberg equilibrium
# when homozygote = AA; Pr(AA) = p^2;   f_homo_AA     = p^2
#                   aa; Pr(aa) = q^2;   f_homo_aa     = q^2
# hetero = Aa         ; Pr(Aa) = 2pq;   f_hetero      = 2p(1-p)

p <- seq(0, 1, by = 0.001)
# q <- (1-p)

homo_AA <- p^2
homo_aa <- (1-p)^2
hetero_Aa <- 2*p*(1-p)

# 2. Maximum heterozygote frequency
# derivatives of hetero_Aa (find max value of p)
max_p <- p[which.max(hetero_Aa)]

# insert p to f_hetero
max_hetero <- max(hetero_Aa)

# simple plot
plot(p, homo_AA,
     type = "l", col = "steelblue", ylim = c(0,1),
     ylab = "Genotype frequency",
     xlab = "Allele frequency p")
lines(p, homo_aa, col = "maroon")
lines(p, hetero_Aa, col = "darkgreen",
      lwd = 2)

legend("top", legend = c("AA (p^2)", "Aa (2pq)", "aa (q^2)"),
       col = c("steelblue", "darkgreen", "maroon"),
       lty = 1, lwd = c(1,2,1))


# Problem 1.5 ##################################################################
# ratio f_A1A2 to F_A2A2 as a function of q; using both exact and approximate fr
# f_A1A2 = 2q(1-q)
# f_A2A2 = q^2

q <- seq(0.01, 1, by = 0.0001)
R_exact <- 2*(1-q)/q
R_approx <- 2/q

plot(q, R_exact, type="l", col="steelblue", lwd=2,
     ylab="Ratio (A1A2 / A2A2)", xlab="q (freq of A2)",
     main="Exact vs Approx")
lines(q, R_approx, col="red", lty=2, lwd=2)
legend("topright", legend=c("Exact", "Approx"),
       col=c("steelblue", "red"), lty=c(1,2), lwd=2)



# Additional plot for function in p vs. q
q <- seq(0.01, 0.99, by = 0.0001)
p <- 1 - q

# ratios
R_q <- 2*(1-q)/q
R_p <- 2*p/(1-p)

# Plot R(q)
par(mfrow = c(1, 2))
plot(q, R_q, type="l", col="steelblue", lwd=2,
     xlab="q (freq of A2)", ylab="Ratio",
     main="R(q) = 2(1-q)/q")
abline(h=0, col="gray")

# Plot R(p)
plot(p, R_p, type="l", col="red", lwd=2,
     xlab="p (freq of A1)", ylab="Ratio",
     main="R(p) = 2p/(1-p)")
abline(h=0, col="gray")

par(mfrow = c(1, 1))


