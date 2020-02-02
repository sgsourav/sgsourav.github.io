# =======================================================
# Boosting and Shrinkage : Part 1 : Gradient Boosting
# =======================================================

# Load and explore the dataset
library(MASS)
attach(Boston)
str(Boston)
help(Boston)

# Convert to a suitable dataframe
boston <- data.frame(Boston)
boston$chas <- as.factor(boston$chas) # qualitative predictor
boston$rad <- as.factor(boston$rad) # qualitative predictor
str(boston)


# Build a Random Forest Model on full data
# Each node splits with subset of features
# install.packages("randomForest")
library(randomForest)
rfFit <- randomForest(medv ~ .,                          # formula
                      data = boston,                     # data set
                      ntree = 500,                       # number of trees
                      mtry = 6,                          # variables for split
                      importance = TRUE)                 # importance recorded                 
rfFit

importance(rfFit)         # importance of the variables in the model (values)
varImpPlot(rfFit)         # importance of the variables in the model (visual)

rfFit$mse[500]            # Out-Of-Bag (OOB) MSE after fitting all 500 trees
plot(rfFit$mse, type="l") # Out-Of-Bag (OOB) MSE after fitting trees 1-by-1

# Find the optimal value of mtry
# Experiment with various values
# Number of trees is fixed (500)
oobError <- double(13)
for (mt in 1:13) {
  rfFit <- randomForest(medv ~ ., data = boston, ntree = 500, mtry = mt)
  oobError[mt] <- rfFit$mse[500]     # OOB MSE after fitting all 500 trees
  cat("Experiment completed with mtry = ", mt, "\n")
}
plot(1:13, oobError, pch=19, type="b",
     xlab = "Value of mtry", ylab = "Out-Of-Bag Error")

# One may choose mtry that produces the minimum in this plot


# Build a Gradient Boosted Model on full data
# Trees built sequentially to fit the residuals
# install.packages("gbm")
library("gbm")
gbmFit <- gbm(medv ~ .,                                  # formula
              data = boston,                             # full data set
              distribution = "gaussian",                 # distribution of response
              n.trees = 5000,                            # number of trees
              interaction.depth = 7,                     # depth of the trees
              shrinkage = 0.01,                          # shrinkage parameter
              cv.folds = 10,                             # k-fold cross validation
              n.cores = 1)                               # number of cores to use

gbmFit                                                   # Optimal GBM information
summary(gbmFit)                                          # Variable Importance Factor

# Plot Cross-Validated MSE (cv.error) vs. Number of Trees
plot(1:5000, gbmFit$cv.error, type = "l",
     xlab = "Number of Trees", ylab = "Mean Squared Error")

# Plot selected Cross-Validated MSE for clear visualization
plot(1000:5000, gbmFit$cv.error[1000:5000], type = "l",
     xlab = "Number of Trees", ylab = "Mean Squared Error")

# One may choose n.trees that produces the minimum in this plot
# GBM returns this as "The best cross-validation iteration was ..."




# =======================================================
# Boosting and Shrinkage : Part 2 : Shrinkage Methods
# =======================================================

# install.packages("glmnet")
library(glmnet)

# Cook up a data for example
dsize <- 50
xvals <- sort(runif(dsize, 0, 1))
yvals <- sin(4*xvals) + rnorm(dsize, 0, 0.2)
plot(xvals, yvals, pch = 19, xlab = "X", ylab = "Y")

# Build Polynomial Regression Models of varying degree
degPoly <- 1
polyFit <- lm(yvals ~ poly(xvals, degree = degPoly, raw = TRUE))
as.data.frame(polyFit$coefficients)


# Build a Shrunken Polynomial Regression Model
# L2 regularization for shrinking -- Ridge
degPoly <- 10
x <- model.matrix(yvals ~ poly(xvals, degree = degPoly, raw = TRUE))
y <- as.matrix(yvals)
ridgeFit <- glmnet(x, y, alpha = 0)
plot(ridgeFit)

ridgeFitCV <- cv.glmnet(x, y, alpha = 0)
plot(ridgeFitCV)
ridgeFitCV$lambda.min
coef(ridgeFitCV, s = ridgeFitCV$lambda.min)
ridgeFitCV$lambda.1se
coef(ridgeFitCV, s = ridgeFitCV$lambda.1se)


# Build a Shrunken Polynomial Regression Model
# L1 regularization for shrinking -- Lasso
degPoly <- 10
x <- model.matrix(yvals ~ poly(xvals, degree = degPoly, raw = TRUE))
y <- as.matrix(yvals)
lassoFit <- glmnet(x, y, alpha = 1)
plot(lassoFit)

lassoFitCV <- cv.glmnet(x, y, alpha = 1)
plot(lassoFitCV)
lassoFitCV$lambda.min
coef(lassoFitCV, s = lassoFitCV$lambda.min)
lassoFitCV$lambda.1se
coef(lassoFitCV, s = lassoFitCV$lambda.1se)



# =======================================================
# Boosting and Shrinkage : Part 3 : Model Selection
# =======================================================

# install.packages("leaps")
# install.packages("glmnet")
library(leaps)
library(glmnet)

# Load and explore the dataset
library(MASS)
attach(Boston)
str(Boston)
help(Boston)

# Convert to a suitable dataframe
boston <- data.frame(Boston)
boston$chas <- as.factor(boston$chas) # qualitative predictor
boston$rad <- as.factor(boston$rad) # qualitative predictor
str(boston)


# Complete Subset Selection for choosing model
regFitComplete <- regsubsets(medv ~ ., data = boston, nvmax = 20)
regFitSummary <- summary(regFitComplete)
regFitSummary
plot(regFitSummary$adjr2, pch = 19, type = "b", 
     xlab = "Number of Variables ", ylab = "Adjusted R-Squared")

# Forward Subset Selection for choosing model
regFitForward <- regsubsets(medv ~ ., data = boston, nvmax = 20, method = "forward")
regFitSummary <- summary(regFitForward)
regFitSummary
plot(regFitSummary$adjr2, pch = 19, type = "b", 
     xlab = "Number of Variables ", ylab = "Adjusted R-Squared")

# Backward Subset Selection for choosing model
regFitBackward <- regsubsets(medv ~ ., data = boston, nvmax = 20, method = "backward")
regFitSummary <- summary(regFitBackward)
regFitSummary
plot(regFitSummary$adjr2, pch = 19, type = "b", 
     xlab = "Number of Variables ", ylab = "Adjusted R-Squared")

# L2 regularization for shrinking -- Ridge
x <- model.matrix(medv ~ ., boston)[,-21]
y <- boston$medv
ridgeFit <- glmnet(x, y, alpha = 0)
plot(ridgeFit)

ridgeFitCV <- cv.glmnet(x, y, alpha = 0)
plot(ridgeFitCV)
ridgeFitCV$lambda.min
coef(ridgeFitCV, s = ridgeFitCV$lambda.min)
ridgeFitCV$lambda.1se
coef(ridgeFitCV, s = ridgeFitCV$lambda.1se)

# L1 regularization for shrinking -- Lasso
x <- model.matrix(medv ~ ., boston)[,-21]
y <- boston$medv
lassoFit <- glmnet(x, y, alpha = 1)
plot(lassoFit)

lassoFitCV <- cv.glmnet(x, y, alpha = 1)
plot(lassoFitCV)
lassoFitCV$lambda.min
coef(lassoFitCV, s = lassoFitCV$lambda.min)
lassoFitCV$lambda.1se
coef(lassoFitCV, s = lassoFitCV$lambda.1se)
