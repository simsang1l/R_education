install.packages(c("ade4", "psych"))

n <- 100
x1 <- rnorm(n, 1, 0.2)
x2 <- 0.5 + 0.5*x1
par(mfrow = c(1, 2))
