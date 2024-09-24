p = seq(0, 1, 0.00001)
odds = p/(1-p)
logodds = log(odds)

plot(logodds ~ p, xlab="Probability", ylab="Log Odds", type="l")