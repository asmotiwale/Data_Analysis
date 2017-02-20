######
# Bisecting K-Means
#####
rm(list = ls(all = T))

library(RColorBrewer)

# set seed to ensure consistent results
set.seed(100)

data.df <- read.csv('hw2-data.csv')

bisectingkmeans <- function(data.df, trials.max, k){
 #Initializing list of clusters to contain all cluster of all the points
 list_cluster <- kmeans(data.df[,2:3], 1)
 data.df["cluster"] <- list_cluster$cluster
 sse <- data.frame(matrix(data=c(1, sum(list_cluster$withinss)), nrow = 1, ncol = 2))
 
 
 #data.df
 #num_cluster <- 1
 for(i in 1:((k-1))){
   m <- sse[sse$X2 == max(sse$X2), 1:2]
   new.df <- data.df[data.df$cluster == m$X1, 1:3]
   low <- .Machine$double.xmax
   #Bisecting the cluster with highest sse. Selecting the best k-means result with lowest sse by performing k-means trials.max times.
   for(j in 1:trials.max){
     c <- kmeans(new.df[,2:3], 2)
     if(low > sum(c$withinss)){
       low <- sum(c$withinss)
       temp <- c
     }
   }
   new.df["cluster"] <- temp$cluster
   
  #Replacing the cluster assignment in the original data frame
  cluster_max <- max(data.df["cluster"])
  for(k in 1:nrow(new.df)){
    for(t in 1:nrow(data.df)){
      if(data.df[t,1] == new.df[k,1]){
        #assigning new clusters to the data points
        data.df[t,4] <- (temp$cluster[k]+cluster_max)
      }
    }
  }
  #Adding newly selected 2 clusters to the list
  sse <- rbind(sse, c(1+cluster_max, temp$withinss[1]))
  sse <- rbind(sse, c(2+cluster_max, temp$withinss[2]))
  #Removing the cluster which is bisected
  sse <- sse[!(sse$X1 == m$X1),]
 }
 data.df$cluster <- factor(data.df$cluster)
 levels(data.df$cluster) <- c(1:5)
 list1 <- list(data.df$cluster,sse$X2)
 return(list1)
}

# Code for comparing result from bisecting kmeans
kmeans_comparison <- function(data.df, result, k){
  #ID's of the initial centroids
  vec <- c(210, 247, 265, 278, 288)
  list_centres <- matrix(NA,ncol=2)
  #Adding the initial centroid data points to the matrix
  for (i in vec){
    list_centres <- rbind(list_centres, c(data.df$band1[data.df$ID == i], data.df$band2[data.df$ID == i]))
  }
  list_centres <- list_centres[-1,]
  #Performing k-means
  result_kmeans <- kmeans(data.df[,2:3], centers = list_centres)
  plot(data.df[, -1], col = brewer.pal(k, "Set3")[result_kmeans$cluster], pch = '.', cex = 3)
}

k=5
#Assigned a value as I was getting the error - "Error in bisectingkmeans(data.df, iter.max = 25, k):unused argument (iter.max = 25)"
iter.max <- 25
result <- bisectingkmeans(data.df, iter.max, k)
plot(data.df[, -1], col = brewer.pal(k, "Set3")[result[[1]]], pch = '.',
     cex = 3)

kmeans_comparison(data.df, result, k)