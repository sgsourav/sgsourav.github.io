# =======================================================
# Linear Regression Lab : Advertising Data
# =======================================================

# Load and explore the dataset
advData <- read.csv("Advertising.csv", header = TRUE)
str(advData)
summary(advData)

# Check the features
# install.packages("corrplot")
library(corrplot)
corrplot.mixed(cor(advData[,-c(1)]))
corrplot(cor(advData[,-c(1,5)]), order ="hclust")

# First Model (FULL)
lmFit1 <- lm(Sales ~ TV + Radio + Newspaper, data = advData)
summary(lmFit1)
plot(lmFit1)

# Check for heteroskedasticity
library(car)
ncvTest(lmFit1)

# Check for multicollinearity
vif(lmFit1)

# Second Model
# Discard the features with low (> 0.1) significance
lmFit2 <- update(lmFit1, ~ . - (Newspaper))
summary(lmFit2)
vif(lmFit2)

# Check plots for potential non-linearity
plot(lmFit2)
# It seems that there is non-linear interaction

# Plot the remaining features against response
plot(advData$TV, advData$Sales)
plot(advData$Radio, advData$Sales)
# install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(advData$TV, advData$Radio, advData$Sales, angle = 45,
              pch = 19, highlight.3d = TRUE, box = FALSE,
              xlab = "TV", ylab = "Radio", zlab = "Sales")
# Seems that the interaction TV-Radio is non-linear

# Include non-linear interaction terms
lmFit3 <- update(lmFit2, ~ . + I(TV*Radio))
summary(lmFit3)
anova(lmFit2, lmFit3, test = "F")
# Seems much better than lmFit2

# Remove outliers and high-leverage points
cd <- cooks.distance(lmFit3)
advData.clean <- advData[abs(cd) < 4/nrow(advData), ]
nrow(advData.clean)

# Fit the best model to the clean data
formula(lmFit3)
lmFit4 <- lm(formula(lmFit3), data = advData.clean)
summary(lmFit4)
plot(lmFit4)

# Final model : lmFit4
summary(lmFit4)



# =======================================================
# Linear Regression Lab : Boston Housing Data
# =======================================================

# Load and explore the dataset
# install.packages("MASS")
library(MASS)
attach(Boston)
str(Boston)
help(Boston)

# Convert to a suitable dataframe
boston <- data.frame(Boston)
boston$chas <- as.factor(boston$chas) # qualitative predictor
boston$rad <- as.factor(boston$rad)   # qualitative predictor
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

# First Model (FULL)
# Use log(medv) in the model as it is more symmetric
lmFit1 <- lm(log(medv) ~ ., data = boston)
summary(lmFit1)
plot(lmFit1)

# Check for heteroskedasticity
library(car)
ncvTest(lmFit1)

# Check for multicollinearity
vif(lmFit1)

# Second Model
# Discard the features with GVIF^(1/(2*Df)) > 3
# Discard the features with low (> 0.1) significance
lmFit2 <- update(lmFit1, ~ . - (tax + indus + age))
summary(lmFit2)
vif(lmFit2)

# Third Model
# Discard the features with low (> 0.05) significance
lmFit3 <- update(lmFit2, ~ . - (zn + rad))
summary(lmFit3)
vif(lmFit3)

# Fourth Model
# Discard the features with GVIF^(1/(2*Df)) > 3
# You may want to keep the high significance ones
lmFit4 <- update(lmFit3, ~ . - (nox))
summary(lmFit4)
vif(lmFit4)

# Fifth Model
# Updating the Third Model -- with "nox" considered
# Discard the features with low (> 0.001) significance
lmFit5 <- update(lmFit3, ~ . - (chas))
summary(lmFit5)
vif(lmFit5)

# Sixth Model
# Updating the Fourth Model -- with "nox" removed
# Discard the features with low (> 0.001) significance
lmFit6 <- update(lmFit4, ~ . - (chas))
summary(lmFit6)
vif(lmFit6)

# Compare models using ANOVA
anova(lmFit3, lmFit5, test = "F")   # Fifth Model >> Third Model
anova(lmFit4, lmFit6, test = "F")   # Sixth Model > Fourth Model
anova(lmFit5, lmFit6, test = "F")   # Sixth Model >> Fifth Model

# Sixth Model is the best model so far
summary(lmFit6)
vif(lmFit6)

# Check plots for potential non-linearity
plot(lmFit6)
# It seems that there is non-linear interaction

# Plot the remaining features against response
plot(boston$crim, boston$medv)
plot(boston$chas, boston$medv)
plot(boston$rm, boston$medv)
plot(boston$dis, boston$medv)
plot(boston$ptratio, boston$medv)
plot(boston$black, boston$medv)
plot(boston$lstat, boston$medv)
# lstat may have a non-linear relation with medv

# Include initial non-linear power term
lmFit7 <- update(lmFit6, ~ . + I(lstat^2))
summary(lmFit7)
anova(lmFit6, lmFit7, test = "F")
# Seems much better than lmFit6

# Include higher non-linear power term
lmFit8 <- update(lmFit7, ~ . + I(lstat^3))
summary(lmFit8)
anova(lmFit7, lmFit8, test = "F")
# Seems worse compared to lmFit7

# Include non-linear interaction terms
lmFit9 <- update(lmFit7, ~ . + I(lstat*rm))
summary(lmFit9)
anova(lmFit7, lmFit9, test = "F")
# Seems much better than lmFit7

# Tenth Model
# Discard the features with low (> 0.05) significance
# Note that lstat needs to be there for the interaction
lmFit10 <- update(lmFit9, ~ . - I(lstat^2))
summary(lmFit10)
plot(lmFit10)

# Remove outliers and high-leverage points
cd <- cooks.distance(lmFit10)
boston.clean <- boston[abs(cd) < 4/nrow(boston), ]
nrow(boston.clean)

# Fit the best model to the clean data
formula(lmFit10)
lmFit11 <- lm(formula(lmFit10), data = boston.clean)
summary(lmFit11)
plot(lmFit11)

# Remove outliers and high-leverage points (one final time)
cd <- cooks.distance(lmFit11)
boston.final <- boston.clean[abs(cd) < 4/nrow(boston.clean), ]
nrow(boston.final)

# Fit the final model to the final (clean) data
formula(lmFit11)
lmFit <- lm(formula(lmFit11), data = boston.final)
summary(lmFit)
plot(lmFit)
