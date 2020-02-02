# Read the CSV files for Titanic dataset
titanicTrain = read.csv("train.csv", header = TRUE)
titanicTest = read.csv("test.csv", header = TRUE)

# Quick look at the training dataset
names(titanicTrain)
table(titanicTrain$Survived)
prop.table(table(titanicTrain$Survived))

# Include the rpart library for decision trees
library(rpart)

# Build a decision tree using method "class"
titanicFormula <- Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked
titanicFit <- rpart(titanicFormula, 
                    data=titanicTrain, method="class")

# Include rendering packages
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Render a nicer representation of rpart
fancyRpartPlot(titanicFit)

# Use the rpart decision tree for prediction
trainPred <- predict(titanicFit, titanicTrain, type = "class")
table(trainPred, titanicTrain$Survived)
prop.table(table(trainPred, titanicTrain$Survived))


testPred <- predict(titanicFit, titanicTest, type = "class")
prop.table(table(testPred))

# Quick test on the IRIS dataset
names(iris)
table(iris$Species)
prop.table(table(iris$Species))

set.seed(12345)
coin <- sample(2, nrow(iris), replace=TRUE, prob=c(0.6, 0.4))
irisTrain <- iris[coin==1,]
irisTest <- iris[coin==2,]

irisFit <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
             data=irisTrain, method="class")

fancyRpartPlot(irisFit)

trainPred <- predict(irisFit, irisTrain, type = "class")
table(trainPred, irisTrain$Species)
prop.table(table(trainPred, irisTrain$Species), 1)

testPred <- predict(irisFit, irisTest, type = "class")
table(testPred, irisTest$Species)
prop.table(table(testPred, irisTest$Species), 1)



# IRIS classification tree with "party" library
#install.packages("party")
library(party)

set.seed(12345)
coin <- sample(2, nrow(iris), replace=TRUE, prob=c(0.6, 0.4))
irisTrain <- iris[coin==1,]
irisTest <- iris[coin==2,]

irisFit <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                 data=irisTrain)

plot(irisFit)
plot(irisFit, type="simple")

trainPred <- predict(irisFit, irisTrain)
table(trainPred, irisTrain$Species)
prop.table(table(trainPred, irisTrain$Species), 1)

testPred <- predict(irisFit, irisTest)
table(testPred, irisTest$Species)
prop.table(table(testPred, irisTest$Species), 1)



# IRIS classification using Random Forests
#install.packages("party")
library(party)

set.seed(12345)
coin <- sample(2, nrow(iris), replace=TRUE, prob=c(0.6, 0.4))
irisTrain <- iris[coin==1,]
irisTest <- iris[coin==2,]

irisRF <- cforest(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                  data = irisTrain, 
                  controls=cforest_unbiased(ntree=2000, mtry=2))

trainPred <- predict(irisRF, newdata = irisTrain)
table(trainPred, irisTrain$Species)
prop.table(table(trainPred, irisTrain$Species), 1)

testPred <- predict(irisRF, newdata = irisTest)
table(testPred, irisTest$Species)
prop.table(table(testPred, irisTest$Species), 1)
