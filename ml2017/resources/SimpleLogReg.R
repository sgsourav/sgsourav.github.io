# =======================================================
# Classification : Simple Logistic Regression
# =======================================================

# Load the dataset and explore
library("ISLR")
attach(Default)
help(Default)
defData <- data.frame(Default)
str(defData)
summary(defData)

# Split into Train and Validation sets
# Training Set : Validation Set = 1 : 1 (random)
set.seed(1)
train <- sample(nrow(defData), 0.5*nrow(defData), replace = FALSE)
defTrain <- defData[train,]
defValid <- defData[-train,]
summary(defTrain)
summary(defValid)

# Default vs Balance (train set)
plot(defTrain$default, defTrain$balance)
plot(defTrain$balance, defTrain$default, 
     pch = 20, lwd = 0.1,
     col = as.integer(defTrain$default)+1,
     xlab = "Balance", ylab = "Default (YES / NO)")

# Fit logistic model for Default ~ Balance
logFit <- glm(default ~ balance, 
              data = defTrain,                   # fit on the train set
              family = binomial(link='logit'))   # link function: logit
summary(logFit)

# Predict using the model
probTrain <- predict(logFit, type = "response")  # on train set
probValid <- predict(logFit, type = "response", 
                     newdata = defValid)  # on validation set

# Convert probabilities to predictions
contrasts(default)

predTrain <- rep("No", nrow(defTrain))
predTrain[probTrain > 0.5] = "Yes"      # set threshold 0.5
predTrain <- as.factor(predTrain)

predValid <- rep("No", nrow(defValid))
predValid[probValid > 0.5] = "Yes"      # set threshold 0.5
predValid <- as.factor(predValid)

# Confusion matrix for predictions
table(defTrain$default, predTrain)      # on train set
table(defValid$default, predValid)      # on validation set

# Classification accuracy
mean(predTrain == defTrain$default)     # on train set
mean(predValid == defValid$default)     # on validation set
# Should match with (TP + TN) / Total from confusion matrix

# Performance measures for Prediction (on train set)
cm <- table(defTrain$default, predTrain) # confusion matrix on train set
cm

# Elements of the confusion matrix
TP <- cm[2,2]  # True Positive (Yes predicted as Yes)
TN <- cm[1,1]  # True Negative (No predicted as No)
FP <- cm[1,2]  # False Positive (No predicted as Yes) -- Type I error
FN <- cm[2,1]  # False Negative (Yes predicted as No) -- Type II error

# Classification Accuracy
(TN + TP) / (TN + TP + FN + FP)  # Correct Classification / Total

# False Positive Rate (fpr) / Type I error
FP / (TN + FP)      # False Positive / Total Negative
1 - FP / (TN + FP)  # Specificity = 1 - (Type I error)

# True Positive Rate / Sensitivity / Recall / Power
TP / (TP + FN)      # True Positive / Total Positive
1 - TP / (TP + FN)  # Type II error = 1 - Sensitivity


# Performance measures for Prediction (on validation set)
cm <- table(defValid$default, predValid) # confusion matrix on validation set
cm

# Elements of the confusion matrix
TP <- cm[2,2]  # True Positive (Yes predicted as Yes)
TN <- cm[1,1]  # True Negative (No predicted as No)
FP <- cm[1,2]  # False Positive (No predicted as Yes) -- Type I error
FN <- cm[2,1]  # False Negative (Yes predicted as No) -- Type II error

# Classification Accuracy
(TN + TP) / (TN + TP + FN + FP)  # Correct Classification / Total

# False Positive Rate (fpr) / Type I error
FP / (TN + FP)      # False Positive / Total Negative
1 - FP / (TN + FP)  # Specificity = 1 - (Type I error)

# True Positive Rate / Sensitivity / Recall / Power
TP / (TP + FN)      # True Positive / Total Positive
1 - TP / (TP + FN)  # Type II error = 1 - Sensitivity

# Check Receiver Operating Characteristic (ROC) on train set
library(ROCR)
probTrain <- predict(logFit, type = "response")  # on train set
predTrain <- prediction(probTrain, defTrain$default)
perfTrain <- performance(predTrain, measure = "tpr", x.measure = "fpr")
plot(perfTrain, main = "ROC Curve for Train Set", colorize = TRUE)

# Check Receiver Operating Characteristic (ROC) on validation set
library(ROCR)
probValid <- predict(logFit, type = "response", 
                     newdata = defValid)  # on validation set
predValid <- prediction(probValid, defValid$default)
perfValid <- performance(predValid, measure = "tpr", x.measure = "fpr")
plot(perfValid, main = "ROC Curve for Validation Set", colorize = TRUE)
