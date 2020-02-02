# =======================================================
# Model Selection : Part 1 : Subset Selection
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

# Check the distribution of response variable
hist(boston$medv, breaks = 20, prob=TRUE, col="grey") 
lines(density(boston$medv), col="blue", lwd=2)
lines(density(boston$medv, adjust=2), lty="dotted", col="red", lwd=2)

# If it looks skewed, try to check a transformation
medv.trans <- log(boston$medv)
hist(medv.trans, breaks = 20, prob=TRUE, col="grey") 
lines(density(medv.trans), col="blue", lwd=2)
lines(density(medv.trans, adjust=2), lty="dotted", col="red", lwd=2)

# Check for skewness numerically to be sure
# install.packages("moments")
library(moments)
skewness(boston$medv)
skewness(medv.trans)

# Check the features
# install.packages("corrplot")
library(corrplot)
corrplot.mixed(cor(boston[,-c(4,9)]))
corrplot(cor(boston[,-c(4,9,14)]), order ="hclust")

# -------------------------------------------------------
# Complete Subset Selection for choosing model

# Use log(medv) in the model as it is more symmetric
regFitComplete <- regsubsets(log(medv) ~ ., data = boston, nvmax = 20)
regFitSummary <- summary(regFitComplete)
regFitSummary
plot(regFitSummary$adjr2, pch = 19, type = "b", 
     xlab = "Number of Variables ", ylab = "Adjusted R-Squared")
plot(regFitComplete, scale = "adjr2")

# -------------------------------------------------------
# Forward Subset Selection for choosing model

# Use log(medv) in the model as it is more symmetric
regFitForward <- regsubsets(log(medv) ~ ., data = boston, 
                            nvmax = 20, method = "forward")
regFitSummary <- summary(regFitForward)
regFitSummary
plot(regFitSummary$adjr2, pch = 19, type = "b", 
     xlab = "Number of Variables ", ylab = "Adjusted R-Squared")
plot(regFitForward, scale = "adjr2")

# -------------------------------------------------------
# Backward Subset Selection for choosing model

# Use log(medv) in the model as it is more symmetric
regFitBackward <- regsubsets(log(medv) ~ ., data = boston, 
                             nvmax = 20, method = "backward")
regFitSummary <- summary(regFitBackward)
regFitSummary
plot(regFitSummary$adjr2, pch = 19, type = "b", 
     xlab = "Number of Variables ", ylab = "Adjusted R-Squared")
plot(regFitBackward, scale = "adjr2")


# =======================================================
# Model Selection : Part 2 : Shrinkage or Regularization
# =======================================================

# install.packages("glmnet")
library(glmnet)

# -------------------------------------------------------
# Shrinkage on a cooked-up data (just as an example)

dsize <- 50
xvals <- sort(runif(dsize, 0, 1))
yvals <- sin(4*xvals) + rnorm(dsize, 0, 0.2)
plot(xvals, yvals, pch = 19, xlab = "X", ylab = "Y")

# Build Polynomial Regression Models of varying degree
degPoly <- 10
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

# -------------------------------------------------------
# Shrinkage on the Boston dataset
  
# L2 regularization for shrinking -- Ridge
# Use log(medv) in the model as it is more symmetric
x <- model.matrix(log(medv) ~ ., boston)
y <- log(boston$medv)
ridgeFit <- glmnet(x, y, alpha = 0)
plot(ridgeFit)

ridgeFitCV <- cv.glmnet(x, y, alpha = 0)
plot(ridgeFitCV)
ridgeFitCV$lambda.min
coef(ridgeFitCV, s = ridgeFitCV$lambda.min)
ridgeFitCV$lambda.1se
coef(ridgeFitCV, s = ridgeFitCV$lambda.1se)

# L1 regularization for shrinking -- Lasso
# Use log(medv) in the model as it is more symmetric
x <- model.matrix(log(medv) ~ ., boston)
y <- log(boston$medv)
lassoFit <- glmnet(x, y, alpha = 1)
plot(lassoFit)

lassoFitCV <- cv.glmnet(x, y, alpha = 1)
plot(lassoFitCV)
lassoFitCV$lambda.min
coef(lassoFitCV, s = lassoFitCV$lambda.min)
lassoFitCV$lambda.1se
coef(lassoFitCV, s = lassoFitCV$lambda.1se)
