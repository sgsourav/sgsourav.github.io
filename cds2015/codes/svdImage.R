# Install the package, if required
# source("http://bioconductor.org/biocLite.R")
# biocLite()
# biocLite("EBImage")

# Set the working directory
# setwd("~/Desktop/Teaching/PGDBA/Resources/RCodes")

# Load the image
origImage <- EBImage::readImage("ElvisNixon.jpg","JPG")
dim(origImage)

# Display the original image
EBImage::display(origImage)

# Compute the PCA on original image
origImage.pca <- princomp(origImage)
screeplot(origImage.pca, type="lines")

# Compute the SVD of original image
origImage.svd <- svd(origImage)
recImage <- origImage.svd$u %*% diag(origImage.svd$d) %*% t(origImage.svd$v)
EBImage::display(recImage)

# Compress down to 10 Principal Components
pc10Image <- origImage.svd$u[,1:10] %*% diag(origImage.svd$d[1:10]) %*% t(origImage.svd$v[,1:10])
EBImage::display(pc10Image)

# Compress down to 20 Principal Components
pc20Image <- origImage.svd$u[,1:20] %*% diag(origImage.svd$d[1:20]) %*% t(origImage.svd$v[,1:20])
EBImage::display(pc20Image)

# Compress down to i = 10, 20, ..., 120 Principal Components
for (i in seq(10, 120, 10)) { 
  pciImage <- origImage.svd$u[,1:i] %*% diag(origImage.svd$d[1:i]) %*% t(origImage.svd$v[,1:i])
  EBImage::display(pciImage)
}

# Compress down to 100 Principal Components
pc100Image <- origImage.svd$u[,1:100] %*% diag(origImage.svd$d[1:100]) %*% t(origImage.svd$v[,1:100])
EBImage::display(pc100Image)
EBImage::display(origImage)
