# =======================================================
# Linear Regression : Part 3 : Confidence and Prediction
# =======================================================

# Load the dataset
advData <- read.csv("Advertising.csv", header = TRUE)
summary(advData)

# Simple linear regression
lmFit <- lm(Sales ~ TV, data = advData)
summary(lmFit)

# Predict using the model with desired "level" of precision
newData <- advData[c(50,100,150),]
predict(lmFit, newdata = newData, interval = "confidence", level = 0.95)
predict(lmFit, newdata = newData, interval = "prediction", level = 0.95)
# and notice that prediction interval is larger than confidence interval

# Confidence interval estimates the expectation E(y|x)
# Prediction interval estimates a single output y|x
# Both depend on (in different manners) the following
coef(summary(lmFit))    # the standard errors of coefficients
confint(lmFit)          # the confidence intervals of coefficients
var(lmFit$residuals)    # variance of residuals


# Picture worth a thousand words
plot(advData$TV, advData$Sales,
     pch = 19, col = "lightblue",
     xlab = "TV Advertisement", ylab = "Sales")
abline(lmFit$coefficients, col = "red", lwd = 2)
confint <- predict(lmFit, newdata = advData, interval = "confidence", level = 0.95)
points(advData$TV, confint[,2], col="red", pch = 46)
points(advData$TV, confint[,3], col="red", pch = 46)
predint <- predict(lmFit, newdata = advData, interval = "prediction", level = 0.95)
points(advData$TV, predint[,2], col="green", pch = 46)
points(advData$TV, predint[,3], col="green", pch = 46)


# Multiple linear regression with two features
lmFit <- lm(Sales ~ TV + Radio, data = advData)
summary(lmFit)

# Predict using the model
newData <- advData[c(50,100,150),]
predict(lmFit, newdata = newData, interval = "confidence", level = 0.95)
predict(lmFit, newdata = newData, interval = "prediction", level = 0.95)

coef(summary(lmFit))    # the standard errors of coefficients
var(lmFit$residuals)    # variance of residuals


# Multiple linear regression with ALL features
lmFit <- lm(Sales ~ TV + Radio + Newspaper, data = advData)
summary(lmFit)

# Predict using the model
newData <- advData[c(50,100,150),]
predict(lmFit, newdata = newData, interval = "confidence", level = 0.95)
predict(lmFit, newdata = newData, interval = "prediction", level = 0.95)

coef(summary(lmFit))    # the standard errors of coefficients
var(lmFit$residuals)    # variance of residuals
