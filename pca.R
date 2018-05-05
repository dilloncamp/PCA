library(R.matlab)
library(stats)
library(png)
library(BBmisc)
library(png)

data <- readMat("ex7faces.mat")
data <- data$X
data_norm <- normalize(data)

pca <- function(x){
  m <- dim(x)[1]
  n <- dim(x)[2]
  sigma <- (1/m) * (t(x) %*% x)
  s <- svd(sigma)
  s
}

b <- pca(data_norm)
U <- b$u
S <- diag(b$d)

# Z is equivalent to lower_dimension_data above
# Z is 5000x353. You can alter the columns of U used
# to change the number of principal components
Z <- data_norm %*% U[,1:100]
# datarec is equivalent to approximation. It is the
# projection of the reduced data onto the original features
datarec <- Z %*% t(U[,1:100])
# Changing the dimensions from 5000x1024 to 5000x32x32 for display
dim(datarec) <- c(5000,32,32)
dim(data_norm) <- c(5000,32,32)

#Force plot area to be square so images display properly
par(pty="s",pin=c(1,1))
image(data_norm[1,,], useRaster=TRUE, axes=FALSE,col=gray((0:32)/32))
image(datarec[1,,], useRaster=TRUE, axes=FALSE,col=gray((0:32)/32))

variation <- c()
for (i in 1:dim(S)[1]){
  variation[i] <- sum(S[1:i,])/sum(S)
}