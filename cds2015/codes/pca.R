# Load the dataset
data("iris")

# Read details of the dataset
help(iris)

# View components of the dataset
names(iris)

# View summary of the dataset
summary(iris)

# Plot summary of the dataset
pairs(iris[1:4])

# Plot summary of the dataset with Species = Colors
pairs(iris[1:4],col=as.numeric(iris$Species)+1)

# Extract only the numeric data
irisdat <- iris[1:4]
head(irisdat)

# Build the covariance matrix
iriscov <- cov(irisdat)
iriscov

# Check the variance of each component
irisvar <- apply(irisdat,2,var)
irisvar

# Compute eigenvalues of covariance matrix
iriseig <- eigen(iriscov)
iriseig$values
iriseig$vectors

# Compute PCA on the dataset
irispca <- princomp(irisdat,cor="False")
irispca

# View the details of PCA data
summary(irispca)
names(irispca)

# Check if SDs in PCA match the Eigenvalues of Cov Matrix
iriseig$values
irispca$sdev^2

# Compute PCA on the Covariance Matrix
irispcacov <- princomp(covmat=iriscov)

# Check if SDs match the Eigenvalues
iriseig$values
irispcacov$sdev^2

# View the cumulative proportion
screeplot(irispca)
screeplot(irispca, type="lines")

# Choose Loadings for the First Principal Component from PCA result
irispc <- irispca$loadings[,1]
irispc

# Choose the First Eigenvector from Eigenvalue decomposition
irispc <- iriseig$vectors[,1]
irispc

# Calculate the First Principal Component as scores from the Loadings / Eigenvectors
irismat <- as.matrix(irisdat)
irisscores <- irismat %*% irispc
irisscores

# Compute the correlation of the First Principal Component with the original features
cor(irisscores, irisdat)

# View the BiPLot of PC representing the First Two Principal Components
biplot(irispca)
abline(h = 0, v = 0, lty = 2, col = 8)

# View the effect of PCA on classification of the data
plot(irisscores, col=as.numeric(iris$Species)+1)

# Run Naive Bayes classifier to Iris data
library(class)
library(e1071)
classifier<-naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,1:4]), iris[,5])

# Run Naive Bayes classifier to Iris First PC
classifier<-naiveBayes(irisscores, iris[,5])
table(predict(classifier, irisscores), iris[,5])
