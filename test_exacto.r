set.seed(7)

rate <- 0.5
rates_sec <- seq(0.2, 0.8, by = 0.01)
alpha <- 0.05
ns <- c(10, 30, 100, 1000)
results <- list(c(), c(), c(), c())

for (i in 1:4) {
    n <- ns[i]
    q <- qgamma(alpha, n, rate)
    for (r in rates_sec) {
        k <- pgamma(q, n, r)
        results[[i]] <- c(results[[i]], k)
    }
}

plot(rates_sec,
    results[[1]],
    type = "l",
    col = "cyan",
    xlab = "Lambda",
    ylab = "Power Function"
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
