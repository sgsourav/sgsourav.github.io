# =======================================================
# Dimensionality Reduction : PCA and t-SNE
# =======================================================

# Load and explore the dataset
attach(iris)
help(iris)
str(iris)

# Convert and visualize the dataset
irisData <- as.data.frame(iris)
pairs(irisData[,1:4], pch = 19, col = as.numeric(irisData$Species) + 1)


# Perform Principal Component Analysis
irisPCA <- prcomp(irisData[,1:4], center = TRUE, scale. = TRUE)
irisPCA
summary(irisPCA)

# Projection on Comp.1 and Comp.2
# install.packages("ggfortify")
library(ggfortify)
autoplot(irisPCA, col = as.numeric(irisData$Species) + 1,
         loadings = TRUE, loadings.label = TRUE)


# Perform t-SNE analysis
# install.packages("Rtsne")
library(Rtsne)
irisUnique <- unique(irisData)
irisTSNE <- Rtsne(irisUnique[,1:4])

# Projection on Comp.1 and Comp.2
plot(irisTSNE$Y, pch = 19, col = as.numeric(irisUnique$Species) + 1,
     xlab = "t-SNE Comp.1", ylab = "t-SNE Comp.2")
