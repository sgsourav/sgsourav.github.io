# =======================================================
# Tuning and Boosting : Part 1 : Tune Random Forest
# =======================================================

# Load and explore the dataset
library(MASS)
attach(Boston)
str(Boston)
help(Boston)

# Convert to a suitable dataframe
boston <- data.frame(Boston)
boston$chas <- as.factor(boston$chas) # qualitative predictor
boston$rad <- as.factor(boston$rad) # qualitative predictor
str(boston)


# -------------------------------------------------------
# Build a Random Forest Model on full data
# Each node splits with subset of features

# install.packages("randomForest")
library(randomForest)
rfFit <- randomForest(medv ~ .,                   # formula
                      data = boston,              # data set
                      ntree = 500,                # number of trees
                      mtry = 5,                   # variables for split
                      importance = TRUE)          # importance recorded                 
rfFit
varImpPlot(rfFit)         # importance of the variables in the model (visual)

rfFit$mse                 # Out-Of-Bag (OOB) MSEs after fitting trees 1-by-1
plot(rfFit$mse, type="l") # One may estimate the "sufficient" value of ntree


# -------------------------------------------------------
# Find the optimal value of mtry through experiments

trials <- 13              # Number of trials for mtry
nTree <- 500              # Fixing the value of ntree

oobError <- double(trials)
for (mt in 1:trials) {
  rfFit <- randomForest(medv ~ ., data = boston, ntree = nTree, mtry = mt)
  oobError[mt] <- rfFit$mse[nTree]     # OOB MSE after fitting all trees
  cat("Experiment completed with mtry = ", mt, "\n")
}
plot(1:trials, oobError, pch=19, type="b",
     xlab = "Value of mtry", ylab = "Out-Of-Bag Error")

# One may choose mtry that produces the minimum in this plot


# -------------------------------------------------------
# Find the optimal value of mtry through Grid Search

# install.packages("caret")
library(caret)
control <- trainControl(method="repeatedcv",      # Repeated Cross-Validation
                        number=10,                # k-Fold CV with k = number
                        repeats=3,                # Number of repeats for kCV
                        search="grid")            # Set Grid Search algorithm

mtryGrid <- expand.grid(.mtry=c(4:8))             # Construct a Grid for mtry

rfTune <- train(medv ~ .,                         # formula
                data = boston,                    # dataset
                method = "rf",                    # RFmodel
                metric = "RMSE",                  # CV RMSE
                tuneGrid = mtryGrid,              # gridset
                trControl=control)                # control

print(rfTune)
plot(rfTune)



# =======================================================
# Tuning and Boosting : Part 2 : Gradient Boosting (GBM)
# =======================================================

# Build a Gradient Boosted Model on full data
# Trees built sequentially to fit the residuals

# install.packages("gbm")
library("gbm")
gbmFit <- gbm(medv ~ .,                           # formula
              data = boston,                      # full data set
              distribution = "gaussian",          # distribution of response
              n.trees = 5000,                     # number of trees
              interaction.depth = 7,              # depth of the trees
              shrinkage = 0.01,                   # shrinkage parameter
              cv.folds = 10,                      # k-fold cross validation
              n.cores = 1)                        # number of cores to use

gbmFit                                            # Optimal GBM information
summary(gbmFit)                                   # Variable Importance Factor

# Plot Cross-Validated MSE (cv.error) vs. Number of Trees
plot(1:5000, gbmFit$cv.error, type = "l",
     xlab = "Number of Trees", ylab = "Mean Squared Error")

# Plot selected Cross-Validated MSE for clear visualization
zoom = c(1500:2500)
plot(zoom, gbmFit$cv.error[zoom], type = "l",
     xlab = "Number of Trees", ylab = "Mean Squared Error")

# One may choose n.trees that produces the minimum in this plot
# GBM returns this as "The best cross-validation iteration was ..."
