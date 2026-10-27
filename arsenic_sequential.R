# Bayesian updating: arsenic in one well, measured 10 times
# theta = probability that a measurement exceeds the EPA limit

epa  <- 10                                          # EPA limit, µg/L
conc <- c(14, 7, 12, 18, 11, 15, 9, 13, 14, 14)     # 10 measurements (illustrative)
above <- as.integer(conc > epa)                     # 1 = above EPA, 0 = below
above                                               # 1 0 1 1 1 1 0 1 1 1

# function: prior (red, dotted) and posterior (green)

plot_update <- function(a, b, x, n, title = "") {
  theta <- seq(0, 1, 0.001)
  plot(theta, dbeta(theta, a, b), type = "l", lty = 3, lwd = 3, col = "red",
       ylim = c(0, 4), bty = "l", xlab = expression(theta), ylab = "",
       main = title)                                            # prior
  if (n > 0)                                                    # no data, no posterior
    lines(theta, dbeta(theta, a + x, b + n - x), lwd = 3, col = "darkgreen")
}

# 1. Flat prior, all measurements so far

par(mfrow = c(3, 4), mar = c(4, 2, 2, 1))

for (i in 0:10) {
  x <- sum(above[seq_len(i)])                       # exceedances so far (0 when i = 0)
  plot_update(1, 1, x, i,
              title = ifelse(i == 0, "No measurement yet",
                             paste(x, "of", i, "above EPA")))
}

# 2. Yesterday's posterior is today's prior

par(mfrow = c(3, 4), mar = c(4, 2, 2, 1))

a <- 1; b <- 1                                      # flat start
plot_update(a, b, 0, 0, title = "No measurement yet")   # prior only

for (i in 1:10) {
  plot_update(a, b, above[i], 1,
              title = paste0("#", i, ": ", conc[i], " ug/L",
                             ifelse(above[i] == 1, " (above)", " (below)")))
  a <- a + above[i]                                 # posterior ...
  b <- b + 1 - above[i]                             # ... becomes next prior
}

c(a, b)                                             # Beta(9, 3), same as part 1

# 3. Reading the posterior

a / (a + b)                                         # posterior mean
qbeta(c(0.025, 0.975), a, b)                        # 95% credible interval
1 - pbeta(0.5, a, b)                                # P(theta > 0.5): above EPA more often than not

# Try: start from Beta(1, 4) (regulator expects few exceedances)
