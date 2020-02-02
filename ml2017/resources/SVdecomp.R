# =======================================================
# Singular Value Decomposition
# =======================================================

# Load the dataset and convert to dataframe
library(MASS)
attach(Boston)
boston <- data.frame(Boston)
boston$chas <- as.factor(boston$chas) # qualitative predictor
boston$rad <- as.factor(boston$rad) # qualitative predictor
str(boston)

# Remove Factor variables and the Response variable
# Convert dataframe to matrix and check dimensions
bostonMat <- as.matrix(boston[,-c(4,9,14)])
dim(bostonMat)

# Seems like the data is 11-dimensional (11 variables)
# We will check this with Singular Value Decomposition
bostonSVD <- svd(bostonMat)
?svd # check what the svd function does

# Check the components of bostonSVD (note the dimensions)
names(bostonSVD)
str(bostonSVD)

# Check that multiplying the components give you the same matrix
# We are basically checking the formula M = U * Sigma * trans(V)
multMat <- bostonSVD$u %*% diag(bostonSVD$d) %*% t(bostonSVD$v)
diffMat <- bostonMat - multMat
norm(diffMat, type = "F") # close to zero 
?norm # check what the function norm does
# If norm(diffMat) ~ 0, then bostonMat ~ multMat

# Check the number of slices of bostonMat 
length(bostonSVD$d) # number of nonzero d values
ncol(bostonSVD$u) # number of columns of u
ncol(bostonSVD$v) # number of columns of v
qr(bostonMat)$rank # rank of the matrix
# All of the above should be identical = 11

# Find out the distribution of the d values
bostonSVD$d
plot(bostonSVD$d, type = 'h')

# Extract the individual Slices
slice01 <- bostonSVD$d[1] * (bostonSVD$u[,1] %*% t(bostonSVD$v[,1]))
slice02 <- bostonSVD$d[2] * (bostonSVD$u[,2] %*% t(bostonSVD$v[,2]))
slice03 <- bostonSVD$d[3] * (bostonSVD$u[,3] %*% t(bostonSVD$v[,3]))
slice04 <- bostonSVD$d[4] * (bostonSVD$u[,4] %*% t(bostonSVD$v[,4]))
slice05 <- bostonSVD$d[5] * (bostonSVD$u[,5] %*% t(bostonSVD$v[,5]))
slice06 <- bostonSVD$d[6] * (bostonSVD$u[,6] %*% t(bostonSVD$v[,6]))
slice07 <- bostonSVD$d[7] * (bostonSVD$u[,7] %*% t(bostonSVD$v[,7]))
slice08 <- bostonSVD$d[8] * (bostonSVD$u[,8] %*% t(bostonSVD$v[,8]))
slice09 <- bostonSVD$d[9] * (bostonSVD$u[,9] %*% t(bostonSVD$v[,9]))
slice10 <- bostonSVD$d[10] * (bostonSVD$u[,10] %*% t(bostonSVD$v[,10]))
slice11 <- bostonSVD$d[11] * (bostonSVD$u[,11] %*% t(bostonSVD$v[,11]))

# Check the norms of the slices
norm(slice01, type = "F")
norm(slice02, type = "F")
norm(slice03, type = "F")
norm(slice04, type = "F")
norm(slice05, type = "F")
norm(slice06, type = "F")
norm(slice07, type = "F")
norm(slice08, type = "F")
norm(slice09, type = "F")
norm(slice10, type = "F")
norm(slice11, type = "F")
# Match with the d values
bostonSVD$d

# Norm of the complete matrix bostonMat
norm(bostonMat, type = "F")
# Match with the L2 norm of the d values
sqrt(t(bostonSVD$d) %*% bostonSVD$d)

# Approximate bostonMat by slice01
approxMat <- slice01
# Proportion of information in approxMat compared to bostonMat
norm(approxMat, type = "F") / norm(bostonMat, type = "F")
# More than 96% of the information recovered

# Approximate bostonMat by (slice01 + slice02)
approxMat <- (slice01 + slice02)
# Proportion of information in approxMat compared to bostonMat
norm(approxMat, type = "F") / norm(bostonMat, type = "F")
# More than 99% of the information recovered

# Approximate bostonMat by (slice01 + slice02 + slice03)
approxMat <- (slice01 + slice02 + slice03)
# Proportion of information in approxMat compared to bostonMat
norm(approxMat, type = "F") / norm(bostonMat, type = "F")
# Close to 100% of the information recovered

# Thus we understand that, even if the rank of bostonMat is 11,
# it is actually very close to a 3-dimensional matrix/subspace.
# In fact, even a 2-dimensional approximation is good enough.
approxMat <- (slice01 + slice02)
# Proportion of information in approxMat compared to bostonMat
norm(approxMat, type = "F") / norm(bostonMat, type = "F")
# approxMat is a good low-dimensional approximation of bostonMat
# Now try this technique with any other dataset of your choice!
