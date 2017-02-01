rm(list=ls())
#Using the pixmap library to read the images
library(pixmap)

#Setting the working directory
setwd("~/Desktop/hwfds45/faces-corrected")

#Specifying the pattern of images to be read
list.files(pattern = ".pgm$")
l.files <- list.files(pattern = ".pgm$")

#Initialising variable to store the images
l.data <- list()
pdata <- list()
mat = matrix(nrow = 45045, ncol =0)

#Reading the images from the folder
for (i in 1:length(l.files))
{
  l.data[i] <- read.pnm(l.files[i])
}

# Converting the images to vectors and adding the vectors to a matrix
for (i in 1:length(l.files))
{
  xy <- getChannels(l.data[[i]])
  dim(xy) <- c(dim(xy)[1]*dim(xy)[2], 1)
  xy = xy - mean(xy)     # Centralizing every vector representing an image
  mat = cbind(mat, xy)
}

# Calculating the covariance matrix
Cov = t(mat) %*% mat

# Computing Eigen vectors of the covariance matrix
par = eigen(Cov)$vectors

# Projecting the data to get the eigennfaces
final = mat %*% par
efaces <- list()

# Extracting the eigen faces and saving it on disk
ab = matrix(nrow = 45045, ncol = 0)
#Setting the directory to save the generated eigenfaces
setwd("~/Desktop/hwfds45/faces-generated")
rotate <- function(x) t(apply(x, 2, rev))
for (i in 1:length(l.files))
{
  ab = matrix(final[,i], nrow = 45045, ncol = 1)
  dim(ab) <- c(231, 195)
  rotate(ab)
  cd <- ab
  z <- pixmapGrey(cd)
  write.pnm(z,file = l.files[i])
}




