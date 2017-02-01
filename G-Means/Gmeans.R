####################################################################
# Implement G-means algorithm                                      #
####################################################################
rm(list=ls())

library(ellipse)
library(mvnTest)
library(MASS)
#Set the directory where the data is present
setwd("~/Desktop/hwfds45/")

# X is the input dataset(it is a dataframe contain multiple cols reading from csv file)
Gmeans <- function(X,alpha = 0.0001,k=1){
  #Adding the column in data to represent the cluster number
  cluster_column <- ncol(X) + 1

  # Initially we start with only 1 cluster, hence every row belongs to only 1 cluster
  X[, cluster_column] <- rep(1, nrow(X))
  #Setting the header name for the cluster column
  names(X)[cluster_column] <- "cluster"
 
  
  # Finding the first center as the initial number of clusters = 1
  first_center <- kmeans(X[, -cluster_column], centers = k)
  cluster_centers <- first_center$centers
  
  #Counting the clusters
  cluster = 1   #Initially we start with just 1 cluster
  
  # Checking for every centre if it can be further divided into two centers
  while(1)
  {
    centers <- nrow(cluster_centers)
    #Variable indicating whether to stop or not
    stop = 0
    
    #Checking for every center if it correctly represents the data or do we need to split the center
    for(i in 1:centers){
      #Considering the centres of every dimension
      c <- cluster_centers[i, ]
      
      #Create the subset of data which belong to that cluster for the respective center
      Z <- subset(X, cluster == i)
      print(nrow(Z))
      if(nrow(Z) <= 15)
      {
        next
      }
      
      #Test the data for to detect if it follows a Gaussian distribution
      #test <- ad.test(Z[, -cluster_column])
      test <- AD.test(Z[, -cluster_column], qqplot = FALSE)
     
      # Checking if the test value is less than the critical value for alpha = 0.00001
      if((test@p.value > 0.00001))
      {
        #Performing kmeans on Z to find two new centers for the current center
        k <- kmeans(Z[, -cluster_column], centers = 2)
        stop = 1
        cdash <- k$centers    #New centers
        new_cluster <- k$cluster    #New clusters
        cluster = cluster + 1
        cluster_centers[i, ] = cdash[1, ]
        cluster_centers <- rbind(cluster_centers, cdash[2, ])  #Adding the new centers to the old center data
        
        # Now we order the clusters
        m = dist(rbind(X[which(new_cluster == 1)[1], -cluster_column], cdash[1, ]))
        n = dist(rbind(X[which(new_cluster == 1)[1], -cluster_column], cdash[2, ]))
        print("Helloo")
        if(m < n)
        {
          new_cluster[new_cluster == 2] = cluster
          new_cluster[new_cluster == 1] = i
        }
        else
        {
          new_cluster[new_cluster == 1] = cluster
          new_cluster[new_cluster == 2] = i
        }
        X[which(X$cluster == i), cluster_column] = new_cluster     #Putting the updated cluster number back to the original data
       
       
        #Plotting the clusters
        par(mfrow=c(2,2))
        for(j in 1:(cluster_column - 2))
        {
          plot(X[, c(j, j + 1)], col = X$cluster, pch = '.', cex = 5)
          
          for(m in 1:cluster)
          {
            l <- cov(X[which(X$cluster == m), c(j, j + 1)])
            ellps <- ellipse(l, centre = cluster_centers[m, c(j, j + 1)])
            lines(ellps, col = m, lwd = 3)
            points(x = cluster_centers[m, j], y = cluster_centers[m, j + 1], pch = '*', cex = 2)

          }
         
        }
       
      }
      
    }
    if(stop == 0)
    {
      break;
    }
    
  }

}

#Reading the data
data = read.table("hw45-r3b-test-data.csv",sep = ",",header = T)
#Calling the Gmeans function
Gmeans(data,0.0001,1)
