# =======================================================
# Recommender Systems : Matrix Factorization
# =======================================================

# Load the dataset and explore
jesterData <- read.csv("jesterRatings.csv", header = FALSE, na.strings = "99")
dim(jesterData)

# Subset with Random 100 Users + Random 50 Jokes
userIndex <- sample(23500, 100, replace = FALSE)
jokeIndex <- sample(2:101, 50, replace = FALSE)
jesterMat <- as.matrix(jesterData[userIndex,jokeIndex])
jesterMat[1:5,1:5]
heatmap(jesterMat, Rowv = NA, Colv = NA, na.rm = TRUE, scale = "none")

# Adding Biases -- Global Mean, Joke Biases and User Biases
globMean <- mean(rowMeans(jesterMat, na.rm = TRUE), na.rm = TRUE)     # Global Mean
jokeMean <- colMeans(jesterMat, na.rm = TRUE)                         # Joke Means
jokeBias <- jokeMean - globMean                                       # Joke Biases
userMean <- rowMeans(jesterMat, na.rm = TRUE)                         # User Means
userBias <- userMean - globMean                                       # User Biases

# Smooth out Missing Ratings by adding Biases
biasesMat <- matrix(globMean, nrow = nrow(jesterMat), ncol = ncol(jesterMat))
colnames(biasesMat) <- colnames(jesterMat)
rownames(biasesMat) <- rownames(jesterMat)
biasesMat[1:5,1:5]
heatmap(biasesMat, Rowv = NA, Colv = NA, na.rm = TRUE, scale = "none")

biasesMat <- sweep(biasesMat, 2, jokeBias, "+")
biasesMat[1:5,1:5]
heatmap(biasesMat, Rowv = NA, Colv = NA, na.rm = TRUE, scale = "none")

biasesMat <- sweep(biasesMat, 1, userBias, "+")
biasesMat[1:5,1:5]
heatmap(biasesMat, Rowv = NA, Colv = NA, na.rm = TRUE, scale = "none")

# Original ratings, as available
jesterMat[1:5,1:5]
heatmap(jesterMat, Rowv = NA, Colv = NA, na.rm = TRUE, scale = "none")

# Remaining User-Joke Interaction-Effects
intEffect <- jesterMat - biasesMat
intEffect[is.na(intEffect)] <- 0
intEffect[1:5,1:5]
heatmap(intEffect, Rowv = NA, Colv = NA, na.rm = TRUE, scale = "none")

# Compute SVD of Interaction-Effects
intEffectSVD <- svd(intEffect)
plot(intEffectSVD$d * intEffectSVD$d, type = 'b')
lowRankIndex <- 10
intEffectLow <- intEffectSVD$u[,1:lowRankIndex] %*% diag(intEffectSVD$d[1:lowRankIndex]) %*% t(intEffectSVD$v[,1:lowRankIndex])
intEffectLow[1:5,1:5]
heatmap(intEffectLow, Rowv = NA, Colv = NA, na.rm = TRUE, scale = "none")

# Recreate the Ratings by adding Biases
ratingMat <- intEffectLow + biasesMat
ratingMat[1:5,1:5]
heatmap(ratingMat, Rowv = NA, Colv = NA, na.rm = TRUE, scale = "none")

# Original ratings, as available
jesterMat[1:5,1:5]
heatmap(jesterMat, Rowv = NA, Colv = NA, na.rm = TRUE, scale = "none")

# Sustitute Missing Ratings by Recreated Ratings
jesterMatPred <- jesterMat
jesterMatPred[is.na(jesterMatPred)] <- ratingMat[is.na(jesterMatPred)]
jesterMatPred[1:5,1:5]
heatmap(jesterMatPred, Rowv = NA, Colv = NA, na.rm = TRUE, scale = "none")
