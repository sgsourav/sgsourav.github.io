# =======================================================
# Decision Tree Lab : Classification
# =======================================================

# Load the dataset and explore
carData <- read.csv("CarEvaluation.csv", header = TRUE)
str(carData)
summary(carData)

# Split into Train and Validation sets
# Training Set : Validation Set = 1 : 1 (random)
set.seed(1)
train <- sample(nrow(carData), 0.5*nrow(carData), replace = FALSE)
carTrain <- carData[train,]
carValid <- carData[-train,]
summary(carTrain)
summary(carValid)

# Visualize the train dataset
plot(carTrain$BuyingPrice, carTrain$Condition)
plot(carTrain$Maintenance, carTrain$Condition)
plot(carTrain$NumDoors, carTrain$Condition)
plot(carTrain$NumPersons, carTrain$Condition)
plot(carTrain$BootSpace, carTrain$Condition)
plot(carTrain$Safety, carTrain$Condition)

# Build a Classification Tree
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

# Calculate information for "Condition" at the root
numsCondition <- table(carTrain$Condition)
freqCondition <- numsCondition / length(carTrain$Condition)
- sum( freqCondition * log2(freqCondition) )     # Shannon Entropy
- 2 * sum( numsCondition * log(freqCondition) )  # Deviance


# =======================================================
# Decision Tree Lab : Regression
# =======================================================

# Load the dataset and explore
advData <- read.csv("Advertising.csv", header = TRUE)
str(advData)
summary(advData)

# Split into Train and Validation sets
# Training Set : Validation Set = 1 : 1 (random)
set.seed(1)
train <- sample(nrow(advData), 0.5*nrow(advData), replace = FALSE)
advTrain <- advData[train,]
advValid <- advData[-train,]
summary(advTrain)
summary(advValid)

# Build a Regression Tree
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

# Calculate deviance for "Sales" at the root
sum((mean(advTrain$Sales) - advTrain$Sales)^2)   # Our old enemy, RSS
