# =======================================================
# Classification : Bagging, Random Forest, Boosting
# =======================================================

# install.packages("randomForest")
library(randomForest)

# Load the dataset and explore
carData <- read.csv("CarEvaluation.csv", header = TRUE)
str(carData)
summary(carData)

# Split into Train and Validation sets
# Training Set : Validation Set = 1 : 1 (random)
train <- sample(nrow(carData), 0.5*nrow(carData), replace = FALSE)
carTrain <- carData[train,]
carValid <- carData[-train,]
summary(carTrain)
summary(carValid)


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


# Variable Importance using Boruta
# randomForest wrapper with Shadow
# install.packages("Boruta")
library("Boruta")
firstBoruta <- Boruta(Condition ~ .,                     # formula
                      data = carData)                    # data set

firstBoruta               # importance of the variables (values)
plot(firstBoruta)         # importance of the variables (visual)

finalBoruta <- TentativeRoughFix(firstBoruta)            # if "tentative" attributes exist
getSelectedAttributes(finalBoruta)                       # extract important variables
attStats(finalBoruta)                                    # full finalBoruta statistics
finalBoruta
plot(finalBoruta)


# Build a Gradient Boosted Model on full data
# Choice of optimal model by cross validation

# install.packages("gbm")
library("gbm")
gbmFit <- gbm(Condition ~ .,                             # formula
              data = carData,                            # full data set
              distribution = "multinomial",              # distribution of response
              n.trees = 5000,                            # number of trees
              interaction.depth = 1,                     # depth of the trees
              shrinkage = 0.1,                           # shrinkage parameter
              cv.folds = 10,                             # k-fold cross validation
              n.cores = 1)                               # number of cores to use

gbmFit                  # the final optimized model after k-fold cross validation
summary(gbmFit)         # importance of the variables in the model (values and visual)

probData <- predict(gbmFit, carData, type = "response")  # probabilities of classes
predData <- apply(probData, 1, which.max)                # prediction (majority class)
mean(predData == as.numeric(carData$Condition))          # classification accuracy
