# importing the library 'ISLR'
library(ISLR)
# Summary of the default dataset which contains the columns 
# of credit card default, student, balance and income
summary(Default)

# Setting a random seed
set.seed(5)

# Fitting the logistic model to the default dataset
glm_fit = glm(default ~ balance + income, data = Default, family = binomial)
glm_fit

validset = function(){
# Now validation set approach is performed to calculate test error
# Step 1: Dividing the dataset into training set (75% of the actual dataset) and validation set(25% of the actual dataset)
sets <- sample(2, dim(Default)[1]/2, replace = TRUE, prob = c(0.50, 0.50))
training_data <- Default[sets == 1,]
validation_data <- Default[sets == 2,]

training_fit <- glm(default ~ balance + income, training_data, family = binomial)

probs = predict(training_fit, validation_data)

pred = rep("No", dim(validation_data)[1])
pred[probs > 0.5] = "Yes"

return(mean(pred != validation_data$default))
}
validset()

validset()
validset()
validset()



  # Now validation set approach is performed to calculate test error including the dummy variable "student"
  # Step 1: Dividing the dataset into training set (75% of the actual dataset) and validation set(25% of the actual dataset)
  sets <- sample(2, dim(Default)[1]/2, replace = TRUE, prob = c(0.50, 0.50))
  training_data <- Default[sets == 1,]
  validation_data <- Default[sets == 2,]
  
  training_fit <- glm(default ~ balance + income + student, training_data, family = binomial)
  
  probs = predict(training_fit, validation_data)
  
  pred = rep("No", dim(validation_data)[1])
  pred[probs > 0.5] = "Yes"
  
  (mean(pred != validation_data$default))

