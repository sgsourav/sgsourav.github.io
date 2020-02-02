# =======================================================
# Multiple Linear Regression
# =======================================================

# Load the dataset
advData <- read.csv("Advertising.csv", header = TRUE)

# Basic exploration
dim(advData)
names(advData)
str(advData)
head(advData)
summary(advData)

# Sales vs each features
plot(advData$TV, advData$Sales)
plot(advData$Radio, advData$Sales)
plot(advData$Newspaper, advData$Sales)

# Each feature taken independently
lmFit <- lm(Sales ~ TV, data = advData)
summary(lmFit)
lmFit <- lm(Sales ~ Radio, data = advData)
summary(lmFit)
lmFit <- lm(Sales ~ Newspaper, data = advData)
summary(lmFit)

# Features taken in pairs
lmFit <- lm(Sales ~ TV + Radio, data = advData)
summary(lmFit)
lmFit <- lm(Sales ~ Radio + Newspaper, data = advData)
summary(lmFit)
lmFit <- lm(Sales ~ TV + Newspaper, data = advData)
summary(lmFit)

# All features taken together
lmFit <- lm(Sales ~ TV + Radio + Newspaper, data = advData)
summary(lmFit)

# We find that Newspaper is not significant
# Exploring lm() plots with TV and Radio
lmFit <- lm(Sales ~ TV + Radio, data = advData)
summary(lmFit)
plot(lmFit)

# Introducing non-linear interaction term (TV * Radio)
lmFit <- lm(Sales ~ TV + Radio + I(TV * Radio), data = advData)
summary(lmFit)
plot(lmFit)

# Remove the high-leverage outliers
modData <- advData[-c(131,156),]
lmFit <- lm(Sales ~ TV + Radio + I(TV * Radio), data = modData)
summary(lmFit)
plot(lmFit)
