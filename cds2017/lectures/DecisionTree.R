# =======================================================
# Tree Model : Part 1 : Decision Tree (Classification)
# =======================================================

# Load the dataset and explore
carData <- read.csv("CarEvaluation.csv", header = TRUE)
str(carData)
summary(carData)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
train <- sample(nrow(carData), 0.7*nrow(carData), replace = FALSE)
carTrain <- carData[train,]
carValid <- carData[-train,]
summary(carTrain)
summary(carValid)

# Visualize the train dataset
plot(carTrain$BuyingPrice, carTrain$Condition, ylab="Condition", xlab="Buying Price")
plot(carTrain$Maintenance, carTrain$Condition, ylab="Condition", xlab="Maintenance")
plot(carTrain$NumDoors, carTrain$Condition, ylab="Condition", xlab="Number of Doors")
plot(carTrain$NumPersons, carTrain$Condition, ylab="Condition", xlab="Number of Persons")
plot(carTrain$BootSpace, carTrain$Condition, ylab="Condition", xlab="Boot Space")
plot(carTrain$Safety, carTrain$Condition, ylab="Condition", xlab="Safety")


# =======================================================
# Tree Model : Part 1A : Building a Classification Tree
# =======================================================

# install.packages("tree")
library("tree")
treeFit <- tree(Condition ~ ., data = carTrain)

# Visualize the tree
plot(treeFit)
text(treeFit, pretty = FALSE)

# Predict using the tree model
predTrain <- predict(treeFit, carTrain, type = "class")  # on train set
predValid <- predict(treeFit, carValid, type = "class")  # on validation set

# Confusion matrix for predictions
table(carTrain$Condition, predTrain)      # on train set
table(carValid$Condition, predValid)      # on validation set

# Classification accuracy
mean(predTrain == carTrain$Condition)     # on train set
mean(predValid == carValid$Condition)     # on validation set

# Check the output
summary(treeFit)
treeFit


# =======================================================
# Tree Model : Part 1B : Pruning a Classification Tree
# =======================================================

# install.packages("tree")
library("tree")

# Load the dataset and explore
carData <- read.csv("CarEvaluation.csv", header = TRUE)
str(carData)
summary(carData)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
train <- sample(nrow(carData), 0.7*nrow(carData), replace = FALSE)
carTrain <- carData[train,]
carValid <- carData[-train,]
summary(carTrain)
summary(carValid)


# -------------------------------------------------------
# Build a "default" Classification Tree
treeFit <- tree(Condition ~ ., data = carTrain)
plot(treeFit)

predTrain <- predict(treeFit, carTrain, type = "class")  # prediction on train set
mean(predTrain == carTrain$Condition)                    # classification accuracy
predValid <- predict(treeFit, carValid, type = "class")  # prediction on validation set
mean(predValid == carValid$Condition)                    # classification accuracy


# -------------------------------------------------------
# Build a "large" Classification Tree
ltreeFit <- tree(Condition ~ ., data = carTrain, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(carTrain),  # number of sample points
                                        mincut = 1,             # minimum points in each child
                                        minsize = 2,            # minimum points in each parent
                                        mindev = 0))            # minimum information gain to split
plot(ltreeFit)

predTrain <- predict(ltreeFit, carTrain, type = "class")  # prediction on train set
mean(predTrain == carTrain$Condition)                     # classification accuracy
predValid <- predict(ltreeFit, carValid, type = "class")  # prediction on validation set
mean(predValid == carValid$Condition)                     # classification accuracy


# -------------------------------------------------------
# Build a "small" Classification Tree
streeFit <- tree(Condition ~ ., data = carTrain, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(carTrain),
                                        mincut = 0.1 * nrow(carTrain),
                                        minsize = 0.2 * nrow(carTrain),
                                        mindev = 0.1))
plot(streeFit)

predTrain <- predict(streeFit, carTrain, type = "class")  # prediction on train set
mean(predTrain == carTrain$Condition)                     # classification accuracy
predValid <- predict(streeFit, carValid, type = "class")  # prediction on validation set
mean(predValid == carValid$Condition)                     # classification accuracy


# -------------------------------------------------------
# Build a "pruned" Classification Tree over train set
ltreeFit <- tree(Condition ~ ., data = carTrain, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(carTrain),
                                        mincut = 1,
                                        minsize = 2,
                                        mindev = 0))
plot(ltreeFit)

cvTree <- cv.tree(ltreeFit, FUN = prune.misclass, K = 10) # K-fold Cross-Validation
cbind(cvTree$size, cvTree$dev, cvTree$k)                  # check cvTree output
plot(cvTree$size, cvTree$dev, type="b")                   # plot deviance vs size
plot(cvTree$k, cvTree$dev, type="b")                      # plot deviance vs alpha

bestSize <- 15  # choose this parameter carefully, based on the cvTree output
ptreeFit <- prune.misclass(ltreeFit, best = bestSize)     # prune tree to best size
plot(ptreeFit)
text(ptreeFit, pretty = FALSE)

predTrain <- predict(ptreeFit, carTrain, type = "class")  # prediction on train set
mean(predTrain == carTrain$Condition)                     # classification accuracy
predValid <- predict(ptreeFit, carValid, type = "class")  # prediction on validation set
mean(predValid == carValid$Condition)                     # classification accuracy



# =======================================================
# Tree Model : Part 2 : Decision Tree (Regression)
# =======================================================

# Load the dataset and explore
advData <- read.csv("Advertising.csv", header = TRUE)
str(advData)
summary(advData)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
train <- sample(nrow(advData), 0.7*nrow(advData), replace = FALSE)
advTrain <- advData[train,]
advValid <- advData[-train,]
summary(advTrain)
summary(advValid)


# =======================================================
# Tree Model : Part 2A : Building a Regression Tree
# =======================================================

# install.packages("tree")
library("tree")
treeFit <- tree(Sales ~ TV + Radio + Newspaper, data = advTrain)

# Visualize the tree
plot(treeFit)
text(treeFit, pretty = FALSE)

# Check the output
summary(treeFit)
treeFit

# Predict using the tree model
predTrain <- predict(treeFit, advTrain, type = "vector")  # on train set
predValid <- predict(treeFit, advValid, type = "vector")  # on validation set

# Prediction accuracy using RSS
sum((predTrain - advTrain$Sales)^2)     # on train set
sum((predValid - advValid$Sales)^2)     # on validation set


# =======================================================
# Tree Model : Part 2B : Pruning a Regression Tree
# =======================================================

# Load the dataset and explore
advData <- read.csv("Advertising.csv", header = TRUE)
str(advData)
summary(advData)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
train <- sample(nrow(advData), 0.7*nrow(advData), replace = FALSE)
advTrain <- advData[train,]
advValid <- advData[-train,]
summary(advTrain)
summary(advValid)


# -------------------------------------------------------
# Build a "default" Regression Tree
treeFit <- tree(Sales ~ TV + Radio + Newspaper, data = advTrain)
plot(treeFit)

predTrain <- predict(treeFit, advTrain, type = "vector")  # prediction on train set
sum((predTrain - advTrain$Sales)^2)                       # RSS (or deviance)
predValid <- predict(treeFit, advValid, type = "vector")  # prediction on validation set
sum((predValid - advValid$Sales)^2)                       # RSS (or deviance)


# -------------------------------------------------------
# Build a "large" Regression Tree
ltreeFit <- tree(Sales ~ TV + Radio + Newspaper, data = advTrain, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(advTrain),
                                        mincut = 1,
                                        minsize = 2,
                                        mindev = 0))
plot(ltreeFit)

predTrain <- predict(ltreeFit, advTrain, type = "vector")  # prediction on train set
sum((predTrain - advTrain$Sales)^2)                        # RSS (or deviance)
predValid <- predict(ltreeFit, advValid, type = "vector")  # prediction on validation set
sum((predValid - advValid$Sales)^2)                        # RSS (or deviance)


# -------------------------------------------------------
# Build a "small" Regression Tree
streeFit <- tree(Sales ~ TV + Radio + Newspaper, data = advTrain, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(advTrain),
                                        mincut = 0.1 * nrow(advTrain),
                                        minsize = 0.2 * nrow(advTrain),
                                        mindev = 0.1))
plot(streeFit)

predTrain <- predict(streeFit, advTrain, type = "vector")  # prediction on train set
sum((predTrain - advTrain$Sales)^2)                        # RSS (or deviance)
predValid <- predict(streeFit, advValid, type = "vector")  # prediction on validation set
sum((predValid - advValid$Sales)^2)                        # RSS (or deviance)


# -------------------------------------------------------
# Build a "pruned" Regression Tree over train set
ltreeFit <- tree(Sales ~ TV + Radio + Newspaper, data = advTrain, 
                 split = "deviance",
                 method = "recursive.partition",
                 control = tree.control(nobs = nrow(advTrain),
                                        mincut = 1,
                                        minsize = 2,
                                        mindev = 0))
plot(ltreeFit)

cvTree <- cv.tree(ltreeFit, FUN = prune.tree, K = 10)     # K-fold Cross-Validation
cbind(cvTree$size, cvTree$dev, cvTree$k)                  # check cvTree output
plot(cvTree$size, cvTree$dev, type="b")                   # plot deviance vs size
plot(cvTree$k, cvTree$dev, type="b")                      # plot deviance vs alpha

bestSize <- 10  # choose this parameter carefully, based on the cvTree output
ptreeFit <- prune.tree(ltreeFit, best = bestSize)         # prune tree to best size
plot(ptreeFit)
text(ptreeFit, pretty = FALSE)

predTrain <- predict(ptreeFit, advTrain, type = "vector")  # prediction on train set
sum((predTrain - advTrain$Sales)^2)                        # RSS (or deviance)
predValid <- predict(ptreeFit, advValid, type = "vector")  # prediction on validation set
sum((predValid - advValid$Sales)^2)                        # RSS (or deviance)
