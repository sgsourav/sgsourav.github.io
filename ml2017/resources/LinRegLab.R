# =======================================================
# Linear Regression Lab
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
library(moments)
skewness(boston$medv)
skewness(medv.trans)

# Check the features
library(corrplot)
corrplot(cor(boston[,-c(4,9,14)]), order ="hclust")
corrplot.mixed(cor(boston[,-c(4,9)]))

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
lmFit2 <- update(lmFit1, ~ . - tax)
summary(lmFit2)
vif(lmFit2)

# Third Model
# Discard the features with low (> 0.05) significance
lmFit3 <- update(lmFit2, ~ . - (indus + age))
summary(lmFit3)
vif(lmFit3)

# Fourth Model
# Discard the features with low (> 0.01) significance
lmFit4 <- update(lmFit3, ~ . - (zn + rad))
summary(lmFit4)
vif(lmFit4)

# Fifth Model
# Discard the features with GVIF^(1/(2*Df)) > 3
lmFit5 <- update(lmFit4, ~ . - (nox))
summary(lmFit5)
vif(lmFit5)

# Sixth Model
# Discard the features with low (> 0.001) significance
lmFit6 <- update(lmFit5, ~ . - (chas))
summary(lmFit6)
vif(lmFit6)

# Compare models using ANOVA
anova(lmFit1,lmFit2,lmFit3,lmFit4,lmFit5,lmFit6, test = "F")
# It seems that Fifth Model was the best one compared to FULL

# Fifth Model is the best model so far
summary(lmFit5)
vif(lmFit5)

# Check plots for potential non-linearity
plot(lmFit5)
# It seems that there is non-linear interaction

# Include initial non-linear power term
lmFit7 <- update(lmFit5, ~ . + I(lstat^2))
summary(lmFit7)
anova(lmFit5, lmFit7, test = "F")
# Seems that this is better than lmFit5

# Include higher non-linear power term
lmFit8 <- update(lmFit7, ~ . + I(lstat^3))
summary(lmFit8)
anova(lmFit7, lmFit8, test = "F")
# Seems that this is worse than lmFit7

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

# Final Model
# Discard the features with low (> 0.001) significance
lmFit12 <- update(lmFit11, ~ . - chas)
summary(lmFit12)
plot(lmFit12)

# Remove outliers and high-leverage points
cd <- cooks.distance(lmFit12)
boston.final <- boston.clean[abs(cd) < 4/nrow(boston), ]
nrow(boston.final)

# Fit the final model to the clean (final) data
formula(lmFit12)
lmFit <- lm(formula(lmFit12), data = boston.final)
summary(lmFit)
plot(lmFit)
