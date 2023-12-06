# HW 2
#1----
library(ISLR)
library(class)
library(MASS)

#A)
pairs(Weekly)
cor(Weekly[,-9])
dim(Weekly)
attach(Weekly)

#B)
glm_full = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(glm_full)

#C)
glm_probs = predict(glm_full, type = "response")
contrasts(Direction)
glm_pred = rep("Down", 1089)
glm_pred[glm_probs > .5] = "Up"
table(glm_pred, Direction)
mean(glm_pred == Direction)

#D)
# build the training and testing data. training data is a vector, not the acutal training data
training_data = (Year >= 1990 & Year <= 2008)
testing_data = Weekly[!training_data, ]
dim(testing_data) # gives dimension for matrix

# use subset to input our training data vector as a criteria for the dataset
glm_Lag2 = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = training_data)
glm_Lag2_Probs = predict(glm_Lag2, testing_data, type = "response")

# build the matrix
glm_Lag2_Pred = rep("Down", 104)
glm_Lag2_Pred[glm_Lag2_Probs > .5] = "Up"
table(glm_Lag2_Pred, testing_data$Direction)

#calculate the accuracy. same as do ing the 
mean(glm_Lag2_Pred == testing_data$Direction)


#E)
#create the lda model
lda_Lag2 = lda(Direction ~ Lag2, data = Weekly, subset = training_data)
lda_Lag2

lda_Lag2_Pred = predict(lda_Lag2, testing_data)
lda_class = lda_Lag2_Pred$class
table(lda_class, testing_data$Direction)
mean(lda_class == testing_data$Direction)

#F)
#create the qda
qda_Lag2 = qda(Direction ~ Lag2, data = Weekly, subset = training_data)
qda_Lag2_Pred = predict(qda_Lag2, testing_data)
qda_class = qda_Lag2_Pred$class
table(qda_class, testing_data$Direction)
mean(qda_class == testing_data$Direction)

#G)
library(class)


training_data <- (Year >= 1990 & Year <= 2008)
testing_data <- Weekly[!training_data, ]

# Create matrices for "Lag2" as predictors
train.X <- matrix(Weekly$Lag2[training_data], ncol = 1)
test.X <- matrix(Weekly$Lag2[!training_data], ncol = 1)

# Extract the "Direction" for training data
train.Direction <- Weekly$Direction[training_data]

# Set the value of k (number of nearest neighbors)
k_value <- 1  # You can adjust this value as needed

# Set a seed for reproducibility (optional)
set.seed(1)

# Perform k-NN classification
knn_pred <- knn(train.X, test.X, train.Direction, k = k_value)
table(knn_pred, testing_data$Direction)

mean(knn_pred == testing_data$Direction)

#I)
#different values for KNN
set.seed(1)
k_value <- 20
knn_pred <- knn(train.X, test.X, train.Direction, k = k_value)
table(knn_pred, testing_data$Direction)
mean(knn_pred == testing_data$Direction)

#Interaction for logistic

# build the training and testing data. training data is a vector, not the acutal training data
training_data = (Year >= 1990 & Year <= 2008)
testing_data = Weekly[!training_data, ]
dim(testing_data) # gives dimension for matrix
set.seed(1)
# use subset to input our training data vector as a criteria for the dataset
glm_Lag24 = glm(Direction ~ Lag2*Lag4, data = Weekly, family = binomial, subset = training_data)
glm_Lag24_Probs = predict(glm_Lag24, testing_data, type = "response")

# build the matrix
glm_Lag24_Pred = rep("Down", 104)
glm_Lag24_Pred[glm_Lag24_Probs > .5] = "Up"
table(glm_Lag24_Pred, testing_data$Direction)

#calculate the accuracy. same as do ing the 
mean(glm_Lag24_Pred == testing_data$Direction)


#Transform the LDA Predictor (Lag2)

lda_Lag2 = lda(Direction ~ log(abs(Lag2)), data = Weekly, subset = training_data)
lda_Lag2

lda_Lag2_Pred = predict(lda_Lag2, testing_data)
lda_class = lda_Lag2_Pred$class
table(lda_class, testing_data$Direction)
mean(lda_class == testing_data$Direction)



#2----

#A)
library(boot)
glm_12 = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
coef(glm_12)
cv.err = cv.glm(Weekly, glm_12)
cv.err$delta

#B)
missing_first = Weekly[-1, ]
glm_12_mifi = glm(Direction ~ Lag1 + Lag2, data = missing_first, family = binomial)
coef(glm_12_mifi)

#C)
first = Weekly[1, ]
first
prediction_prob = predict(glm_12_mifi, newdata = first, type = "response")

if (prediction_prob > 0.5) {
  classified_direction = "Up"
} else {
  classified_direction = "Down"
}

# Check if the first observation was correctly classified
correct_classification = (classified_direction == first$Direction)
# Print the classification result
cat("Predicted Direction:", classified_direction, "\n")

# Print whether the first observation was correctly classified
cat("Correctly Classified:", correct_classification, "\n")
prediction_prob

#D)
n = nrow(Weekly)
n

error_indicators <- rep(0, n)

# Fit the logistic regression model and make predictions for each observation
for (i in 1:n) {
  # Exclude the ith observation from the data
  subset_data = Weekly[-i, ]
  
  # Fit a logistic regression model using Lag1 and Lag2
  model = glm(Direction ~ Lag1 + Lag2, data = subset_data, family = binomial)
  
  # Extract the ith observation for prediction
  observation_i = Weekly[i, ]
  
  # Predict the probability that Direction is "Up" for the ith observation
  prediction_prob = predict(model, newdata = observation_i, type = "response")
  
  # Classify the ith observation based on the threshold
  if (prediction_prob > 0.5) {
    classified_direction = "Up"
  } else {
    classified_direction = "Down"
  }
  
  # Check if an error was made in predicting the direction for the ith observation
  error_made = (classified_direction != observation_i$Direction)
  
  # Store the error indicator (1 for error, 0 for correct)
  error_indicators[i] = error_made
}

# Summarize the error indicators
error_summary = table(error_indicators)

# Print the results
cat("Error Summary:\n")
print(error_summary)

#2E)
loocv_error_estimate = mean(error_indicators)

# Print the LOOCV error estimate
cat("LOOCV Estimate for Test Error:", loocv_error_estimate, "\n")
#3----
#A)

library(MASS)

# Access the Boston dataset
data(Boston)

# Calculate the sample mean for the "medv" variable
mu_hat = mean(Boston$medv)


# Print the estimated population mean
cat("Estimated Population Mean (mu_hat) of 'medv':", mu_hat, "\n")

#B)
# Calculate the standard error of the sample mean
n <- length(Boston$medv)  # Number of observations
sample_std_dev <- sd(Boston$medv)  # Sample standard deviation
standard_error <- sample_std_dev / sqrt(n)

# Print the standard error
cat("Standard Error of mu_hat:", standard_error, "\n")

#C)
num_bootstraps = 1000

# Initialize a vector to store bootstrap sample means
bootstrap_sample_means = numeric(num_bootstraps)

# Perform the bootstrap procedure
set.seed(1)  
for (i in 1:num_bootstraps) {
  # Resample the data with replacement
  bootstrap_sample = sample(Boston$medv, replace = TRUE)
  
  # Calculate the sample mean for the bootstrap sample
  bootstrap_sample_means[i] = mean(bootstrap_sample)
}

# Calculate the standard error of mu hat using the bootstrap
bootstrap_standard_error = sd(bootstrap_sample_means)

# Print the bootstrap standard error
cat("Bootstrap Standard Error of mu_hat:", bootstrap_standard_error, "\n")

#D)
muhat = mean(Boston$medv)
se_muhat = bootstrap_standard_error
# Calculate the lower and upper bounds of the bootstrap confidence interval
lower_bound = muhat - 2 * se_muhat
upper_bound = muhat + 2 * se_muhat

# Print the bootstrap confidence interval
cat("Bootstrap 95% Confidence Interval:", lower_bound, "to", upper_bound, "\n")

# Calculate a 95% confidence interval using t.test
conf_interval = t.test(Boston$medv)$conf.int

# Print the t-test confidence interval
cat("t.test 95% Confidence Interval:", conf_interval[1], "to", conf_interval[2], "\n")

#E)

# Calculate the sample median for the "medv" variable
sample_median = median(Boston$medv)

# Assign the sample median to the estimate mu hat_med
mu_hat_med = sample_median

# Print the estimated population median
cat("Estimated Population Median (mu_hat_med) of 'medv':", mu_hat_med, "\n")

#F)

# Set the number of bootstrap iterations
num_bootstraps = 1000  

# Initialize a vector to store bootstrap sample medians
bootstrap_sample_medians = numeric(num_bootstraps)

# Perform the bootstrap procedure
set.seed(1)  # Set a random seed for reproducibility
for (i in 1:num_bootstraps) {
  # Resample the 'medv' values with replacement
  bootstrap_sample <- sample(Boston$medv, replace = TRUE)
  
  # Calculate the sample median for the bootstrap sample
  bootstrap_sample_medians[i] <- median(bootstrap_sample)
}

# Calculate the standard error of mu hat_med using the bootstrap
bootstrap_standard_error_med <- sd(bootstrap_sample_medians)

# Print the bootstrap standard error for the median
cat("Bootstrap Standard Error of mu hat_med (Median):", bootstrap_standard_error_med, "\n")

#G)
mu_hat_.1 = quantile(Boston$medv, probs = .1)
mu_hat_.1

#H)
# Set the number of bootstrap iterations
num_bootstraps <- 1000  # You can adjust this as needed

# Initialize a vector to store bootstrap sample tenth percentiles
bootstrap_sample_percentiles <- numeric(num_bootstraps)

# Perform the bootstrap procedure
set.seed(1)  # Set a random seed for reproducibility
for (i in 1:num_bootstraps) {
  # Resample the 'medv' values with replacement
  bootstrap_sample <- sample(Boston$medv, replace = TRUE)
  
  # Calculate the tenth percentile for the bootstrap sample
  bootstrap_sample_percentiles[i] <- quantile(bootstrap_sample, probs = 0.1)
}

# Calculate the standard error of mu hat_0.1 using the bootstrap
bootstrap_standard_error_0.1 <- sd(bootstrap_sample_percentiles)

# Print the bootstrap standard error for the tenth percentile
cat("Bootstrap Standard Error of mu hat_0.1 (Tenth Percentile):", bootstrap_standard_error_0.1, "\n")






