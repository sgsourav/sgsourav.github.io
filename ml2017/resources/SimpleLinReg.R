# =======================================================
# Simple Linear Regression
# =======================================================

# Load the dataset
advData <- read.csv("Advertising.csv", header = TRUE)

# Basic exploration
dim(advData)
names(advData)
str(advData)
head(advData)
summary(advData)

# Sales vs TV Advertisement
plot(advData$TV, advData$Sales)

# Simple linear regression
lmFit <- lm(Sales ~ TV, data = advData)
lmFit$coefficients
abline(lmFit$coefficients, col="red", lwd=3)

# =======================================================
# Distribution of Coefficients
# =======================================================

# Create a random Train Data
trainIndex <- sample(1:nrow(advData),100,replace = FALSE)
advTrain <- advData[trainIndex,]
plot(advData$TV, advData$Sales)
points(advTrain$TV, advTrain$Sales, type="p", pch=21, col="red", bg="red")

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
plot(advData$TV, advData$Sales, type="p", col="black")
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
hist(beta.0, col = "green", main = "Distribution of Intercept")
hist(beta.1, col = "green", main = "Distribution of Slope")

# Explore the complete lm() output
summary(lmFit)
