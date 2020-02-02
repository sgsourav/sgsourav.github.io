# =======================================================
# Cross-Validation for Linear Regression
# =======================================================

# Load the dataset
advData <- read.csv("Advertising.csv", header = TRUE)
summary(advData)

# Eight models for linear regression
lmFit0 <- lm(Sales ~ 1, data = advData)
summary(lmFit0)

lmFit1 <- lm(Sales ~ TV, data = advData)
summary(lmFit1)

lmFit2 <- lm(Sales ~ Radio, data = advData)
summary(lmFit2)

lmFit3 <- lm(Sales ~ Newspaper, data = advData)
summary(lmFit3)

lmFit4 <- lm(Sales ~ TV + Radio, data = advData)
summary(lmFit4)

lmFit5 <- lm(Sales ~ TV + Newspaper, data = advData)
summary(lmFit5)

lmFit6 <- lm(Sales ~ Radio + Newspaper, data = advData)
summary(lmFit6)

lmFit7 <- lm(Sales ~ TV + Radio + Newspaper, data = advData)
summary(lmFit7)


# Training Set : Validation Set = 70 : 30 (random)
train <- sample(nrow(advData), 0.7*nrow(advData), replace = FALSE)
lmFit0 <- lm(Sales ~ 1, data = advData, subset = train)
lmFit1 <- lm(Sales ~ TV, data = advData, subset = train)
lmFit2 <- lm(Sales ~ Radio, data = advData, subset = train)
lmFit3 <- lm(Sales ~ Newspaper, data = advData, subset = train)
lmFit4 <- lm(Sales ~ TV + Radio, data = advData, subset = train)
lmFit5 <- lm(Sales ~ TV + Newspaper, data = advData, subset = train)
lmFit6 <- lm(Sales ~ Radio + Newspaper, data = advData, subset = train)
lmFit7 <- lm(Sales ~ TV + Radio + Newspaper, data = advData, subset = train)

# Compute Training and Validation MSE over different Models
nModels <- 7
nTrials <- 10
trnMSE <- matrix(data = NA, nrow = nTrials, ncol = nModels)
valMSE <- matrix(data = NA, nrow = nTrials, ncol = nModels)

for (t in 1:nTrials) {
  set.seed(t)
  train <- sample(nrow(advData), 0.7*nrow(advData), replace = FALSE)
  
  lmFit1 <- lm(Sales ~ TV, data = advData, subset = train)
  lmFit2 <- lm(Sales ~ Radio, data = advData, subset = train)
  lmFit3 <- lm(Sales ~ Newspaper, data = advData, subset = train)
  lmFit4 <- lm(Sales ~ TV + Radio, data = advData, subset = train)
  lmFit5 <- lm(Sales ~ TV + Newspaper, data = advData, subset = train)
  lmFit6 <- lm(Sales ~ Radio + Newspaper, data = advData, subset = train)
  lmFit7 <- lm(Sales ~ TV + Radio + Newspaper, data = advData, subset = train)
  
  error1 <- (advData$Sales - predict(lmFit1,advData))
  trnMSE[t,1] <- mean(error1[train]^2)
  valMSE[t,1] <- mean(error1[-train]^2)
  
  error2 <- (advData$Sales - predict(lmFit2,advData))
  trnMSE[t,2] <- mean(error2[train]^2)
  valMSE[t,2] <- mean(error2[-train]^2)

  error3 <- (advData$Sales - predict(lmFit3,advData))
  trnMSE[t,3] <- mean(error3[train]^2)
  valMSE[t,3] <- mean(error3[-train]^2)

  error4 <- (advData$Sales - predict(lmFit4,advData))
  trnMSE[t,4] <- mean(error4[train]^2)
  valMSE[t,4] <- mean(error4[-train]^2)

  error5 <- (advData$Sales - predict(lmFit5,advData))
  trnMSE[t,5] <- mean(error5[train]^2)
  valMSE[t,5] <- mean(error5[-train]^2)

  error6 <- (advData$Sales - predict(lmFit6,advData))
  trnMSE[t,6] <- mean(error6[train]^2)
  valMSE[t,6] <- mean(error6[-train]^2)
  
  error7 <- (advData$Sales - predict(lmFit7,advData))
  trnMSE[t,7] <- mean(error7[train]^2)
  valMSE[t,7] <- mean(error7[-train]^2)
}

# Plot the average Training and Validation MSE for different Models
plot(1:nModels, colMeans(trnMSE), 
     lwd = 2, col = "green", type = "line", 
     ylim = c(min(trnMSE),max(valMSE)), 
     xlab = "Model Complexity", ylab = "Mean Squared Error")
lines(1:nModels, colMeans(valMSE), lwd = 2, col = "red", type = "line")

# Plot the actual Training and Validation MSE for different Models
for (t in 1:nTrials) {
  lines(1:nModels, trnMSE[t,], lwd = 0.5, col = "limegreen", type = "line")
  lines(1:nModels, valMSE[t,], lwd = 0.5, col = "pink", type = "line")
}
