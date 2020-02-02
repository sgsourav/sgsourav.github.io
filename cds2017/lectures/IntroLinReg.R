# =======================================================
# Linear Regression : Part 1 : Introduction
# =======================================================

# Install and Load Packages
# install.packages("vioplot")
# install.packages("corrplot")
library(vioplot)
library(corrplot)

# Load the dataset
advData <- read.csv("Advertising.csv", header = TRUE)

# Basic exploration
dim(advData)
names(advData)
str(advData)
head(advData)
summary(advData)

# Exploring Sales
boxplot(advData$Sales, horizontal=TRUE, 
        col = "lightblue", main = "Boxplot of Sales")
hist(advData$Sales, prob = TRUE, 
     col = "lightgreen", main = "Histogram of Sales")
lines(density(advData$Sales), col = "red", lwd = 3)
lines(density(advData$Sales, adjust = 2), 
      lty = "dotted", col = "magenta", lwd = 3)
vioplot(advData$Sales, horizontal=TRUE, col = "pink")
title(main = "Violin Plot of Sales")

# Exploring each Feature
par(mfrow=c(2,2))
boxplot(advData$TV, horizontal=TRUE, 
        col = "lightblue", main = "Boxplot")
hist(advData$TV, prob = TRUE, col = "lightgreen", 
     main = "Histogram", xlab = "", ylab = "")
lines(density(advData$TV), col = "red", lwd = 3)
vioplot(advData$TV, horizontal=TRUE, col = "pink")
title(main = "Violin Plot")
plot(advData$TV, advData$Sales, pch = 19, col = "red",
     main = "TV vs Sales", xlab = "", ylab = "")
par(mfrow=c(1,1))

# Sales vs TV Advertisement
plot(advData$TV, advData$Sales,
     pch = 19, col = "red",
     xlab = "TV Advertisement", ylab = "Sales")

# Sales vs Radio Advertisement
plot(advData$Radio, advData$Sales, 
     pch = 19, col = "red",
     xlab = "Radio Advertisement", ylab = "Sales")

# Sales vs Newspaper Advertisement
plot(advData$Newspaper, advData$Sales, 
     pch = 19, col = "red",
     xlab = "Newspaper Advertisement", ylab = "Sales")

# Quick Pairwise Plots
pairs(advData[,2:5], pch = 19, col = "red")

# Quick Correlation
corrplot.mixed(cor(advData[,2:5]))

# Simple linear regression
plot(advData$TV, advData$Sales,
     pch = 19, col = "blue",
     xlab = "TV Advertisement", ylab = "Sales")

lmFit <- lm(Sales ~ TV, data = advData)
lmFit$coefficients
abline(lmFit$coefficients, col="black", lwd=3)

# Create a random Train Data
trainIndex <- sample(1:nrow(advData),100,replace = FALSE)
advTrain <- advData[trainIndex,]
plot(advData$TV, advData$Sales, 
     col = "pink", pch = 21, lwd = 2,
     xlab = "TV", ylab = "Sales")
points(advTrain$TV, advTrain$Sales, 
       type="p", pch=21, col="red", bg="red")

# Simple linear regression on Train Data
lmTrain <- lm(Sales ~ TV, data = advTrain)
abline(lmTrain$coefficients, col="red", lwd="2")

# Simple linear regression on Full Data
lmData <- lm(Sales ~ TV, data = advData)
abline(lmData$coefficients, col="black", lwd="2")

# Compare the coefficients
lmTrain$coefficients
lmData$coefficients

# Simple linear regression with many random Train Datasets
plot(advData$TV, advData$Sales, 
     type="p", pch = 21, col="pink", lwd = 2)
lmData <- lm(Sales ~ TV, data = advData)
lmCoef <- as.array(lmData$coefficients)
for (i in 1:100) {
  trainIndex <- sample(1:nrow(advData),100,replace = FALSE)
  advTrain <- advData[trainIndex,]
  lmTrain <- lm(Sales ~ TV, data = advTrain)
  trainCoef <- as.array(lmTrain$coefficients)
  lmCoef <- rbind(lmCoef, trainCoef)
  abline(lmTrain$coefficients, col="red", lwd="0.1")
}

# Simple linear regression on Full Data
abline(lmData$coefficients, col="black", lwd="3")

# Check the distribution of the coefficients
beta.0 <- lmCoef[2:nrow(lmCoef),1]
beta.1 <- lmCoef[2:nrow(lmCoef),2]
summary(beta.0)
summary(beta.1)
vioplot(beta.0, horizontal=TRUE, col = "pink")
title(main = "Distribution of Intercept")
vioplot(beta.1, horizontal=TRUE, col = "pink")
title(main = "Distribution of Slope")

# Explore the complete lm() output
summary(lmFit)
plot(lmFit)
