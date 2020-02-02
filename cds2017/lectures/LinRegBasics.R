# =======================================================
# Linear Regression : Part 2 : Basic Concepts
# =======================================================

# Load the dataset
advData <- read.csv("Advertising.csv", header = TRUE)
str(advData)
summary(advData)

# Sales vs TV Advertisement
plot(advData$TV, advData$Sales,
     pch = 19, col = "red",
     xlab = "TV Advertisement", ylab = "Sales")

# Simple linear regression
lmFit <- lm(Sales ~ TV, data = advData)
lmFit$coefficients
abline(lmFit$coefficients, col="black", lwd=3)

# Explore the lm() output
lmFit$coefficients                          # coefficients (beta)
plot(advData$TV, lmFit$fitted.values)       # fitted values (yhat)
plot(advData$TV, lmFit$residuals)           # residuals (y - yhat)

summary(lmFit)
plot(lmFit)


# Sales vs TV and Radio Advertisement
# install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(advData$TV, advData$Radio, advData$Sales, angle = 45,
              pch = 19, highlight.3d = TRUE, box = FALSE,
              xlab = "TV", ylab = "Radio", zlab = "Sales")

# Multiple linear regression
lmFit <- lm(Sales ~ TV + Radio, data = advData)
lmFit$coefficients
plot3d <- scatterplot3d(advData$TV, advData$Radio, advData$Sales, angle = 45,
                        pch = 19, highlight.3d = TRUE, box = FALSE,
                        xlab = "TV", ylab = "Radio", zlab = "Sales")
plot3d$plane3d(lmFit$coefficients)

summary(lmFit)
plot(lmFit)


# Multiple linear regression with ALL features
lmFit <- lm(Sales ~ TV + Radio + Newspaper, data = advData)
summary(lmFit)

# Checking for cross-correlations
# install.packages("corrplot")
library(corrplot)
corrplot.mixed(cor(advData[,2:5]))

# Checking for multicollinearity
# install.packages("car")
library(car)
vif(lmFit)
