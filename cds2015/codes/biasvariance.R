library(ggplot2)

# The "Real" Model
set.seed(12345)
dat <- data.frame(x = runif(1000, 0, 12))
dat$x <- sort(dat$x)
dat$y <- with(dat, sin(x * 1.3) * 15 + 3 * (x - 4)^2)
ggplot(dat, aes(x = x, y = y)) + geom_point(colour = 'red', size = 1) 

# Noise in Observing the Real Model
sigma <- with(dat, (exp(x - 5)/(1 + exp(x - 5)) - exp(x - 7)/(1 + exp(x - 7)) * 2) + 1.4) * 6
ggplot(dat, aes(x = x, y = sigma)) + geom_point(colour = 'black', size = 1)

# Observed Data = Real Model + Noise
dat$yobs <- dat$y + rnorm(nrow(dat), mean = 0, sd = sigma)
p <- ggplot(dat, aes(x = x, y = yobs)) + geom_point(alpha = 0.5, color = 'blue', size = 2)
p
p + geom_line(aes(x = dat$x, y = dat$y), color = 'red', size = 1)

# Fitting models using LOESS function
library(RColorBrewer)
plot(yobs ~ x, dat, pch = 16, cex = 0.5)

spans <- seq(0.01, 1, length.out = 8)
colors <- brewer.pal(length(spans), "YlGnBu")
for (i in 1:length(spans)) {
  lines(dat$x, predict(loess(yobs ~ x, dat, span = spans[i])), lwd = 1, col = colors[i])
}

# Too simple a model -- underfitting
p <- ggplot(dat, aes(x = x, y = yobs)) + geom_point(alpha = 0.5, color = 'blue', size = 2)
p + geom_line(aes(x = dat$x, y = predict(loess(yobs ~ x, dat, span = 1.0))), 
              color = 'red', size = 1)

# Too complex a model -- overfitting
p <- ggplot(dat, aes(x = x, y = yobs)) + geom_point(alpha = 0.5, color = 'blue', size = 2)
p + geom_line(aes(x = dat$x, y = predict(loess(yobs ~ x, dat, span = 0.01))), 
              color = 'red', size = 1)

# Somewhat balanced a model -- may be a good fit
p <- ggplot(dat, aes(x = x, y = yobs)) + geom_point(alpha = 0.5, color = 'blue', size = 2)
p + geom_line(aes(x = dat$x, y = predict(loess(yobs ~ x, dat, span = 0.25))), 
              color = 'red', size = 1)
