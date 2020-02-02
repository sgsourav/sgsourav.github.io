# Read the training data set of characters
train <- read.csv("train.csv", header=TRUE)
train <- as.matrix(train)

# Color ramp definition
colors <- c('white','black')
cus_col <- colorRampPalette(colors=colors)

# Compute the mean of each digit
par(mfrow=c(4,3), pty='s', mar=c(1,1,1,1), xaxt='n', yaxt='n')
digit_mean <- array(dim = c(10,28*28))

for(di in 0:9) {
  print(di)
  digit_mean[di+1,] <- apply(train[train[,1] == di, -1], 2, sum)
  digit_mean[di+1,] <- (digit_mean[di+1,] / max(digit_mean[di+1,])) * 255
  
  z <- array(digit_mean[di+1,], dim = c(28,28))
  z <- z[,28:1]
  image(1:28, 1:28, z, main=di, col=cus_col(256))
}

# Compute the variance of each digit
par(mfrow=c(4,3), pty='s', mar=c(1,1,1,1), xaxt='n', yaxt='n')
digit_var <- array(dim = c(10,28*28))

for(di in 0:9) {
  print(di)
  digit_var[di+1,] <- apply(train[train[,1] == di, -1], 2, var)
  digit_var[di+1,] <- (digit_var[di+1,] / max(digit_var[di+1,])) * 255
  
  z <- array(digit_var[di+1,], dim = c(28,28))
  z <- z[,28:1]
  image(1:28, 1:28, z, main=di, col=cus_col(256))
}

# Compute the k-rank approximation of each digit
k <- 3
digit_pc <- array(dim = c(28*28,k,10))

for(di in 0:9) {
  tempsvd <- svd(t(train[train[,1] == di, -1]))
  digit_pc[,,di+1] <- tempsvd$u[,1:k]
  cat(sprintf("Digit %d : Completed PC generation\n", di))
}

# Try to use the PC matrix above to recognize digits