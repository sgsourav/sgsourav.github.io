# =======================================================
# Classification : Decision Tree
# =======================================================

# Load the dataset and explore
help(iris)
irisData <- data.frame(iris)
str(irisData)
summary(irisData)

# Split into Train and Validation sets
# Training Set : Validation Set = 1 : 1 (random)
set.seed(1)
train <- sample(nrow(irisData), 0.5*nrow(irisData), replace = FALSE)
irisTrain <- irisData[train,]
irisValid <- irisData[-train,]
summary(irisTrain)
summary(irisValid)

# Visualize the train dataset
pairs(irisTrain[,1:4], col = as.numeric(irisTrain$Species) + 1)

# Build a decision tree using "tree" library
# install.packages("tree")
library("tree")
treeFit <- tree(Species ~ ., data = irisTrain)
plot(treeFit)
text(treeFit, pretty = FALSE)

# Predict using the model
predTrain <- predict(treeFit, irisTrain, type = "class")  # on train set
predValid <- predict(treeFit, irisValid, type = "class")  # on validation set

# Confusion matrix for predictions
table(irisTrain$Species, predTrain)      # on train set
table(irisValid$Species, predValid)      # on validation set

# Classification accuracy
mean(predTrain == irisTrain$Species)     # on train set
mean(predValid == irisValid$Species)     # on validation set
summary(treeFit)

# Build a decision tree using "party" library
# install.packages("party")
library(party)
treeFit <- ctree(Species ~ ., data = irisTrain)
plot(treeFit, type = "simple")
plot(treeFit)

# Predict using the model
predTrain <- predict(treeFit, irisTrain, type = "response")  # on train set
predValid <- predict(treeFit, irisValid, type = "response")  # on validation set

# Confusion matrix for predictions
table(irisTrain$Species, predTrain)      # on train set
table(irisValid$Species, predValid)      # on validation set

# Classification accuracy
mean(predTrain == irisTrain$Species)     # on train set
mean(predValid == irisValid$Species)     # on validation set
