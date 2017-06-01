########
# Maximum Likelihood Classifier
# Student Name: Anuraag Motiwale
# Student Unity ID: asmotiwa
######




require(mixtools) #for bivariate density plots/ellipses


# Run startup script to load functions to read geotiff files
source('./startup.R')

start.time <- Sys.time()
# Read data for training and testing
data <- readCSV()

# tr contains training data, first element of data
# te contains testing data, second element of data
tr <- data[[1]]
te <- data[[2]]

# Read the image(Geotiff file)
img <- readTif()

# Count the number of training instances
ntraining <- nrow(tr)

# Count the number of features
nfeatures <- ncol(tr) - 1

# Count the number of classes
nclasses <- length(unique(tr$Class))

# tr_splits is a list, where each element of the list contains 
# information pertaining a single class
tr_splits <- split(tr, tr$Class)



# Function to compute the mean of each feature in a data frame
###
compute_means <- function(df){
    return(c(mean(df$R),mean(df$G),mean(df$B)))
}

# Function to compute the covariance matrix between features in a data frame
###
compute_covs <- function(df){
    return(cov(df[,-4]))
}

# Args:
# A data frame 'df' of training data pertaining to a single class of size
# ntraining, the number of training data points
# Returns:
# Probability of class contained in df
###
compute_apriori <- function(df, ntraining){
    return(nrow(df) / ntraining)
}

# tr_means is a list containing mean vector of attributes per class. 
tr_means <- lapply(tr_splits, compute_means) 

# tr_covs is a list containing covariance matrices of attributes per class. 
tr_covs <- lapply(tr_splits, compute_covs) 

# tr_apriori is a list containing prior probabilities per class.
tr_apriori <- lapply(tr_splits, compute_apriori, ntraining) 

# function to implement bivariate density plots (ellipses)
# Plot the training data points using only feat1 and feat2
# Using the means and covs you calculated in the previous steps, 
# plot ellipses for feat1 and feat2.
###

bvd <- function(feat1, feat2, tr_means = tr_means, tr_covs = tr_covs,
                colors = colors){
    
   plot(tr[,c(1,3)], xlab = "R", ylab = "B", main = "Plot for dimensions- 1,3")
   mean_data <- lapply(tr_means, "[", c(feat1, feat2))
   cov_data <- lapply(tr_covs, function(x) x[c(feat1, feat2),c(feat1, feat2)])

   e1 <- ellipse(mu = mean_data$`1`, sigma = cov_data$`1`,col = colors[1])
   e2 <- ellipse(mu = mean_data$`2`, sigma = cov_data$`2`,col = colors[2])
   e3 <- ellipse(mu = mean_data$`3`, sigma = cov_data$`3`,col = colors[3])
   e4 <- ellipse(mu = mean_data$`4`, sigma = cov_data$`4`,col = colors[4])
   e5 <- ellipse(mu = mean_data$`5`, sigma = cov_data$`5`,col = colors[5])
   legend(60, 190, # places a legend at the appropriate place 
          c(1,2,3,4,5), # puts text in the legend
          
          lty=c(1,1,1,1,1), # gives the legend appropriate symbols (lines)
          
          lwd=c(2.5,2.5),col=c(colors[1], colors[2], colors[3], colors[4], colors[5]))
}

# Call the bvd density plot.
# 'feat1','feat2' variables refers to the dimensions
# Input dimension numbers. For eg: bvd(1, 2) refers to bvd 
# over the first two features R and G.
bvd(1, 3, tr_means, tr_covs, colors)

# MLC
# Args:
# means - list containing mean vectors for each class
# covs - list containing covariance matrices for each class
# aprioris - list comprising of apriori probabilities for each class
# tedata - data frame of test data without the labels column
# Returns:
# predicted_cl - Vector of class assignments of length nrow(tedata) 
# You should implement MLC manually, do not use any library
###
MLC <- function(means, covs, aprioris, tedata){
    
    # vector to store predicted classes for each instance in test
    predicted_cl <- rep(0, nrow(tedata))
    # Iterating over the entire testdata
    for(n in (1:nrow(tedata))){
      max_ml <- 0
      max_cl <- 0
      # Calculating the value for decision rule for each class.
      for(cl in (1:length(means))){
        # Setting the value for parameter d
        d <- length(means[[cl]])
        #Calculating class conditional probability
        first_term <- (1/sqrt(((2*pi)^d)*det(covs[[cl]])))
        covariance_inverse <- data.matrix(solve(covs[[cl]]))
        second_term <- (tedata[n,] - means[[cl]])
        class_conditional_distribution <- first_term * (exp(-0.5*(((data.matrix(second_term) %*% covariance_inverse) %*% data.matrix(t(second_term))))))
        decision <- class_conditional_distribution * aprioris[[cl]]
        
        # Finding the max value of decision rule
        if(decision > max_ml){
          max_ml <- decision
          max_cl <- cl
        }
        
      }
      # Assigning the class with maximum value of decision rule to the test data record.
      predicted_cl[n] <- max_cl
    }
    
    # Return the vector of predicted labels
    return(predicted_cl)
}

pred_te_labels <- MLC(tr_means, tr_covs, tr_apriori, te[, -ncol(te)])


# Generate confusion matrix - Variable 'tab' stores this confusion matrix
tab <- table(as.factor(te$Class), as.factor(pred_te_labels))

# Using the confusion matrix tab, compute overall accuracy
###
compute_overall_accuracy <- function(tab){
    return(sum(diag(tab)) / sum(tab))
    
}

# Compute accuracy of a particular class using 'tab'
# Args:
# class - refers to the class number
# tab - confusion matrix
###
compute_class_accuracy <- function(class, tab){
    
   return(tab[class, class] / sum(tab[class, ]))
}

# Call overall class accuracy computation function
overall_te_accuracy <- compute_overall_accuracy(tab)

# Compute individual class accuracy for all classes
individual_te_accuracy <- sapply(1:nclasses, compute_class_accuracy, tab)

pred_img_labels <- MLC(tr_means, tr_covs, tr_apriori, img)
end.time <- Sys.time()
time.taken <- end.time - start.time

# Plot the classification result on entire image
# Check your working directory for 'mlc.tif'!
plotTif(pred_img_labels)