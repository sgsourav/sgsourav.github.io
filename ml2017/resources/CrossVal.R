# =======================================================
# Cross-Validation for Linear Regression
# =======================================================

# Load the dataset and convert to dataframe
library(MASS)
attach(Boston)
boston <- data.frame(Boston)
boston$chas <- as.factor(boston$chas) # qualitative predictor
boston$rad <- as.factor(boston$rad) # qualitative predictor
str(boston)

# Target Linear Regression Problem
plot(boston$lstat, boston$medv, 
     col = "grey", pch = 21,
     xlab = "lstat", ylab = "medv")

# Validation Set Approach
# Training Set : Validation Set = 1 : 1 (random)
set.seed(1)
train <- sample(nrow(boston), 0.5*nrow(boston), replace = FALSE)
points(boston[train,]$lstat, boston[train,]$medv, col = "turquoise", pch = 20)

# Fit linear model only on the Training Data
glmFit <- glm(medv ~ lstat, data = boston, subset = train)
abline(glmFit$coefficients, lwd = 3, col = "steelblue")
errors <- (boston$medv - predict(glmFit,boston))

# Compare the two MSE
mean(errors[train]^2) # Training data
mean(errors[-train]^2) # Validation data

# Compute Training and Validation MSE over different Models
nModels <- 7
nTrials <- 5
trnMSE <- matrix(data = NA, nrow = nTrials, ncol = nModels)
valMSE <- matrix(data = NA, nrow = nTrials, ncol = nModels)

for (t in 1:nTrials) {
  set.seed(t)
  train <- sample(nrow(boston), 0.5*nrow(boston), replace = FALSE)
  for (m in 1:nModels) {
    glmFit <- glm(medv ~ poly(lstat,m), data = boston, subset = train)
    errors <- (boston$medv - predict(glmFit,boston))
    trnMSE[t,m] <- mean(errors[train]^2)
    valMSE[t,m] <- mean(errors[-train]^2)
  }
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

# LOOCV (Leave One Out Cross Validation) Approach
library(boot)
glmFit <- glm(medv ~ lstat, data = boston)
cvMSE <- cv.glm(boston, glmFit)
cvMSE$delta[1]

# Compute LOOCV MSE over different Models
nModels <- 7
cvMSE <- rep(0, nModels)
  
for (m in 1:nModels) {
  glmFit <- glm(medv ~ poly(lstat,m), data = boston)
  cvMSE[m] <- cv.glm(boston, glmFit)$delta[1]
}
cvMSE
lines(1:nModels, cvMSE, lwd = 2, col = "steelblue", type = "line", 
      xlab = "Model Complexity", ylab = "LOOCV MSE")

# k-Fold Cross Validation Approach
# Compute k-Fold CV MSE over different Models
nModels <- 7
kcvMSE <- rep(0, nModels)

for (m in 1:nModels) {
  glmFit <- glm(medv ~ poly(lstat,m), data = boston)
  kcvMSE[m] <- cv.glm(boston, glmFit, K = k)$delta[1]
}
kcvMSE
lines(1:nModels, kcvMSE, lwd = 2, col = "magenta", type = "line", 
      xlab = "Model Complexity", ylab = "k-Fold CV MSE")

# Using the "caret" package
library(caret)

# Split the data into Train : Test = 75% : 25%
train <- createDataPartition(boston$medv, p=0.75, list=FALSE)
trainData <- boston[train,]
testData <- boston[-train,]

# Set the validation method (LOOCV, cv, repeatedcv, boot)
fitControl <- trainControl(method = "cv", number = 10)

# Train a linear model accordingly
glmFit <- train(medv ~ lstat, data = trainData, 
                method = "glm", 
                trControl = fitControl)

glmFit$finalModel
