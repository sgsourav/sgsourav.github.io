# =======================================================
# Tree Model : Part 3 : Random Forest (Classification)
# =======================================================

# install.packages("randomForest")
library(randomForest)

# Load the dataset and explore
carData <- read.csv("CarEvaluation.csv", header = TRUE)
str(carData)
summary(carData)

# -------------------------------------------------------
# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
train <- sample(nrow(carData), 0.7*nrow(carData), replace = FALSE)
carTrain <- carData[train,]
carValid <- carData[-train,]
summary(carTrain)
summary(carValid)


# -------------------------------------------------------
# Build a Bagging Model on train set
# Each node splits with all features

bagFit <- randomForest(Condition ~ .,                    # formula
                       data = carTrain,                  # data set
                       ntree = 500,                      # number of trees
                       mtry = 6,                         # variables for split
                       importance = TRUE)                # importance recorded
bagFit

predTrain <- predict(bagFit, carTrain, type = "class")   # prediction on train set
mean(predTrain == carTrain$Condition)                    # classification accuracy
predValid <- predict(bagFit, carValid, type = "class")   # prediction on validation set
mean(predValid == carValid$Condition)                    # classification accuracy

importance(bagFit)        # importance of the variables in the model (values)
varImpPlot(bagFit)        # importance of the variables in the model (visual)


# -------------------------------------------------------
# Build a Random Forest Model on train set
# Each node splits with subset of features

rfFit <- randomForest(Condition ~ .,                     # formula
                      data = carTrain,                   # data set
                      ntree = 500,                       # number of trees
                      mtry = 3,                          # variables for split
                      importance = TRUE)                 # importance recorded                 
rfFit

predTrain <- predict(rfFit, carTrain, type = "class")    # prediction on train set
mean(predTrain == carTrain$Condition)                    # classification accuracy
predValid <- predict(rfFit, carValid, type = "class")    # prediction on validation set
mean(predValid == carValid$Condition)                    # classification accuracy

importance(rfFit)         # importance of the variables in the model (values)
varImpPlot(rfFit)         # importance of the variables in the model (visual)


# -------------------------------------------------------
# Boruta : Variable Importance at a Glance

# install.packages("Boruta")
library("Boruta")
borutaFit <- Boruta(Condition ~ .,                       # formula
                    data = carTrain)                     # data set

getSelectedAttributes(borutaFit)                         # extract important variables
attStats(borutaFit)                                      # full finalBoruta statistics
borutaFit                                                # importance of variables (values)
plot(borutaFit)                                          # importance of variables (visual)



# =======================================================
# Tree Model : Part 4 : Random Forest (Regression)
# =======================================================

# install.packages("randomForest")
library(randomForest)

# Load the dataset and explore
advData <- read.csv("Advertising.csv", header = TRUE)
str(advData)
summary(advData)

# -------------------------------------------------------
# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
train <- sample(nrow(advData), 0.7*nrow(advData), replace = FALSE)
advTrain <- advData[train,]
advValid <- advData[-train,]
summary(advTrain)
summary(advValid)


# -------------------------------------------------------
# Build a Bagging Model on train set
# Each node splits with all features

bagFit <- randomForest(Sales ~ TV + Radio + Newspaper,   # formula
                       data = advTrain,                  # data set
                       ntree = 500,                      # number of trees
                       mtry = 3)                         # variables for split
bagFit

predTrain <- predict(bagFit, advTrain, type="response")  # prediction on train set
sum((predTrain - advTrain$Sales)^2)                      # RSS (or deviance)
predValid <- predict(bagFit, advValid, type="response")  # prediction on validation set
sum((predValid - advValid$Sales)^2)                      # RSS (or deviance)
varImpPlot(bagFit)        # importance of the variables in the model (visual)


# -------------------------------------------------------
# Build a Random Forest Model on train set
# Each node splits with subset of features

rfFit <- randomForest(Sales ~ TV + Radio + Newspaper,    # formula
                      data = advTrain,                   # data set
                      ntree = 500,                       # number of trees
                      mtry = 2)                          # variables for split
rfFit

predTrain <- predict(rfFit, advTrain, type="response")   # prediction on train set
sum((predTrain - advTrain$Sales)^2)                      # RSS (or deviance)
predValid <- predict(rfFit, advValid, type="response")   # prediction on validation set
sum((predValid - advValid$Sales)^2)                      # RSS (or deviance)
varImpPlot(rfFit)         # importance of the variables in the model (visual)


# -------------------------------------------------------
# Boruta : Variable Importance at a Glance

# install.packages("Boruta")
library("Boruta")
borutaFit <- Boruta(Sales ~ TV + Radio + Newspaper,      # formula
                    data = advTrain)                     # data set

getSelectedAttributes(borutaFit)                         # extract important variables
attStats(borutaFit)                                      # full finalBoruta statistics
borutaFit                                                # importance of variables (values)
plot(borutaFit)                                          # importance of variables (visual)
