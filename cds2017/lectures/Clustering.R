# =======================================================
# Clustering Methods : Part 1 : K-Means Clustering
# =======================================================

# Load and explore the dataset
attach(iris)
str(iris)

# Subset the features to get 2-D dataset
irisData <- as.data.frame(iris[,c(1,3)])
plot(irisData, pch = 19)

# Set color palette for visualization
# install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
myPal <- brewer.pal(n = 9, name = "Set1")


# -------------------------------------------------------
# K-Means Clustering

K <- 2
kMeansFit <- kmeans(irisData, centers = K)
kMeansFit
plot(irisData, pch = 19, col = palette(myPal)[as.numeric(kMeansFit$cluster)])
points(kMeansFit$centers, col = palette(myPal)[1:K], pch = 9, cex = 5)

# Experiment with K
kMin <- 1
kMax <- 20
withinSS <- double(kMax - kMin + 1)
betweenSS <- double(kMax - kMin + 1)

for (K in kMin:kMax) {
  kMeansFit <- kmeans(irisData, centers = K)
  withinSS[K] <- sum(kMeansFit$withinss)
  betweenSS[K] <- kMeansFit$betweenss
}

plot(kMin:kMax, withinSS, pch=19, type="b", col="red",
     xlab = "Value of K", ylab = "Sum of Squares (Within and Between)")
points(kMin:kMax, betweenSS, pch=19, type="b", col="green")

# Choose an "optimal" K
K <- 3
kMeansFit <- kmeans(irisData, centers = K)
kMeansFit
plot(irisData, pch = 19, col = palette(myPal)[as.numeric(kMeansFit$cluster)])
points(kMeansFit$centers, col = palette(myPal)[1:K], pch = 9, cex = 5)


# -------------------------------------------------------
# K-Means Clustering with multiple random starts

K <- 2
kMeansFit <- kmeans(irisData, centers = K, nstart = 20)
kMeansFit
plot(irisData, pch = 19, col = palette(myPal)[as.numeric(kMeansFit$cluster)])
points(kMeansFit$centers, col = palette(myPal)[1:K], pch = 9, cex = 5)

# Experiment with K
kMin <- 1
kMax <- 20
withinSS <- double(kMax - kMin + 1)
betweenSS <- double(kMax - kMin + 1)

for (K in kMin:kMax) {
  kMeansFit <- kmeans(irisData, centers = K, nstart = 20)
  withinSS[K] <- sum(kMeansFit$withinss)
  betweenSS[K] <- kMeansFit$betweenss
}

plot(kMin:kMax, withinSS, pch=19, type="b", col="red",
     xlab = "Value of K", ylab = "Sum of Squares (Within and Between)")
points(kMin:kMax, betweenSS, pch=19, type="b", col="green")

# Choose an "optimal" K
K <- 3
kMeansFit <- kmeans(irisData, centers = K)
kMeansFit
plot(irisData, pch = 19, col = palette(myPal)[as.numeric(kMeansFit$cluster)])
points(kMeansFit$centers, col = palette(myPal)[1:K], pch = 9, cex = 5)



# =======================================================
# Clustering Methods : Part 2 : Hierarchical Clustering
# =======================================================

# Load and explore the dataset
attach(iris)
str(iris)

# Subset the features to get 2-D dataset
irisData <- as.data.frame(iris[,c(1,3)])
plot(irisData, pch = 19)

# Set color palette for visualization
# install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
myPal <- brewer.pal(n = 9, name = "Set1")


# -------------------------------------------------------
# Hierarchical Clustering : Minimum Variance
# Prioratizes compact and spherical clusters

hiercFit <- hclust(dist(irisData), method="ward.D")
hiercFit
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)

K <- 2
plot(hiercFit, main="Minimum Variance", xlab="", ylab="", sub="", cex =.5)
rect.hclust(hiercFit, k = K)
plot(irisData, pch = 19, col = palette(myPal)[as.numeric(cutree(hiercFit, k = K))])


# -------------------------------------------------------
# Hierarchical Clustering : Complete Linkage
# Farthest Neighbour Method -- Max dist(x,y)
# Compact clusters of approx equal diameters

hiercFit <- hclust(dist(irisData), method="complete")
hiercFit
plot(hiercFit, main="Complete Linkage", xlab="", ylab="", sub="", cex =.5)

K <- 2
plot(hiercFit, main="Complete Linkage", xlab="", ylab="", sub="", cex =.5)
rect.hclust(hiercFit, k = K)
plot(irisData, pch = 19, col = palette(myPal)[as.numeric(cutree(hiercFit, k = K))])


# -------------------------------------------------------
# Hierarchical clustering : Single Linkage
# Closest Neighbour Method -- Min dist(x,y)
# Tends to produce long thin cluster chains

hiercFit <- hclust(dist(irisData), method="single")
hiercFit
plot(hiercFit, main="Single Linkage", xlab="", ylab="", sub="", cex =.5)

K <- 2
plot(hiercFit, main="Single Linkage", xlab="", ylab="", sub="", cex =.5)
rect.hclust(hiercFit, k = K)
plot(irisData, pch = 19, col = palette(myPal)[as.numeric(cutree(hiercFit, k = K))])


# -------------------------------------------------------
# Hierarchical clustering : Average Linkage
# Averages the pairwise distances dist(x,y)

hiercFit <- hclust(dist(irisData), method="average")
hiercFit
plot(hiercFit, main="Average Linkage", xlab="", ylab="", sub="", cex =.5)

K <- 2
plot(hiercFit, main="Average Linkage", xlab="", ylab="", sub="", cex =.5)
rect.hclust(hiercFit, k = K)
plot(irisData, pch = 19, col = palette(myPal)[as.numeric(cutree(hiercFit, k = K))])



# =======================================================
# Clustering Methods : Part 3 : Graph Clustering
# =======================================================

# install.packages("igraph")
library(igraph)

# Example Graph : Zachary's Karate Club
graphZKC <- make_graph("Zachary")
plot(graphZKC, 
     layout = layout.kamada.kawai,
     vertex.size = 7,
     vertex.color = "darkgray",
     edge.width = 1,  
     edge.color = "darkgray",
     vertex.label = NA)

# Set color palette for visualization
# install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
myPal <- brewer.pal(n = 9, name = "Set1")

# -------------------------------------------------------
# Fast Greedy Clustering on the Graph

fgcFit <- cluster_fast_greedy(graphZKC)
fgcFit
membership(fgcFit)
plot(graphZKC,
     layout = layout.kamada.kawai,
     vertex.size = 7,
     vertex.color = palette(myPal)[as.numeric(membership(fgcFit))],
     edge.width = 1,  
     edge.color = "darkgray",
     vertex.label = NA)
