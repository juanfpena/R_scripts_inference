set.seed(7)

rate <- 0.5
rates_sec <- seq(0.3, 0.7, by = 0.01)
alpha <- 0.05
ns <- c(10, 30, 100, 1000)
results <- list(c(), c(), c(), c())

for (i in 1:4) {
    n <- ns[i]
    x <- rexp(n, rate)
    for (r in rates_sec) {
        t <- (sqrt(n) * (mean(x) - (1 / r)) / (1 / (r^2)))
        k <- pnorm(t)
        results[[i]] <- c(results[[i]], k)
    }
}

plot(rates_sec,
    results[[1]],
    type = "l",
    col = "cyan",
    xlab = "Lambda",
    ylab = "Power Function",
    ylim = c(0, 1)
)
lines(rates_sec, results[[2]], col = "violet")
lines(rates_sec, results[[3]], col = "purple")
lines(rates_sec, results[[4]], col = "blue")
legend(
    "topleft",
    legend = c(
        "n = 10",
        "n = 30",
        "n = 100",
        "n = 1000"
    ),
    fill = c("cyan", "violet", "purple", "blue")
)
