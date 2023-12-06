#Final Stat 4700
#1----
#a)
set.seed(42)
BodyDF=data.frame(read.table("body.dat.txt",header=F))
colnames(BodyDF) = c("BiacromialDiam", "BiiliacDiam", "BitrochantericDiam", "ChestDepth", "ChestDiam", "ElbowDiam", "WristDiam", "KneeDiam", "AnkleDiam",
                     "ShoulderGirth", "ChestGirth", "WaistGirth", "NavelGirth", "HipGirth", "ThighGirth", "BicepGirth", "ForearmGirth", "KneeGirth",
                     "CalfMaxGirth", "AnkleMinGirth", "WristMinGirth",
                     "Age", "Weight", "Height", "Gender")

Y=cbind(BodyDF$Age, BodyDF$Weight, BodyDF$Height, BodyDF$Gender)
Y = BodyDF$Weight
X=cbind(BodyDF$BiacromialDiam,BodyDF$BiiliacDiam,BodyDF$BitrochantericDiam,BodyDF$ChestDepth,BodyDF$ChestDiam,BodyDF$ElbowDiam,
        BodyDF$WristDiam,BodyDF$KneeDiam,BodyDF$AnkleDiam,BodyDF$ShoulderGirth,BodyDF$ChestGirth,BodyDF$WaistGirth,BodyDF$NavelGirth,BodyDF$HipGirth,
        BodyDF$ThighGirth,BodyDF$BicepGirth,BodyDF$ForearmGirth,BodyDF$KneeGirth,BodyDF$CalfMaxGirth,BodyDF$AnkleMinGirth,BodyDF$WristMinGirth)

# Since males tend to be taller, we could create boxplots for both and see where the mean values are. More than likely, the larger mean value is the male columns. 
boxplot(Height ~ Gender, data = BodyDF, col = c("pink", "blue"), xlab = "Gender", ylab = "Height", main = "Distribution of Heights by Gender")

# From these results we can see that more than likely the 0's are for females, and 1's are for males.  

#b)

# Create logical vectors for training and test sets with 200 observations in the test set
test_indices <- sample(1:nrow(BodyDF), 200)
train <- rep(TRUE, nrow(BodyDF))
train[test_indices] <- FALSE
test <- !train

# Split the data into training and test sets
train_data <- BodyDF[train, ]
test_data <- BodyDF[test, ]

# Extract predictor variables (X) and response variable (Y) for training and test sets
X_train <- train_data[, c("BiacromialDiam", "BiiliacDiam", "BitrochantericDiam", "ChestDepth", "ChestDiam", "ElbowDiam", "WristDiam", "KneeDiam", "AnkleDiam",
                          "ShoulderGirth", "ChestGirth", "WaistGirth", "NavelGirth", "HipGirth", "ThighGirth", "BicepGirth", "ForearmGirth", "KneeGirth",
                          "CalfMaxGirth", "AnkleMinGirth", "WristMinGirth")]

Y_train <- train_data$Weight

X_test <- test_data[, c("BiacromialDiam", "BiiliacDiam", "BitrochantericDiam", "ChestDepth", "ChestDiam", "ElbowDiam", "WristDiam", "KneeDiam", "AnkleDiam",
                        "ShoulderGirth", "ChestGirth", "WaistGirth", "NavelGirth", "HipGirth", "ThighGirth", "BicepGirth", "ForearmGirth", "KneeGirth",
                        "CalfMaxGirth", "AnkleMinGirth", "WristMinGirth")]

Y_test <- test_data$Weight

# Load the necessary library
library(pls)

# Fit PCR model
pcr_model <- pcr(Weight ~ ., data = train_data, scale = TRUE, validation = "CV")

# Fit PLSR model
plsr_model <- plsr(Weight ~ ., data = train_data, scale = TRUE, validation = "CV")
# It makes sense to scale our data because the predictors have different units for measurements. Also, these models are impacted negativly by unscaled data. 

#c)

# Print summaries of the models
summary(pcr_model)
summary(plsr_model)
# It is important to scale our variables because we don't want one variable to dominate other variables because it uses a different scale.
# Most model's are susceptible to this and need scaled datasets. PCR and PLSR are no exception. 
# The patterning is very similar which isn't too surprising since these are both dimension reduction modeling methods. They take different approaches, but same idea which is why I believe our results are very similar. 


#d)
validationplot(pcr_model, val.type = "MSEP")
validationplot(plsr_model, val.type = "MSEP")
# After looking at these plots, I would say that 5 components for the pcr model is the best and 4 components for the plsr model is the best.
# The reason being that even though the model's get better, it is not much better than at 4 and 5 components, and it keeps the complexity of the model low. 

#e)
# We can't limit the variable selection other than just removing variables our self with pcr and plsr.
# Let's try a lasso and see if that will reduce the number of predictors in the model. 

# Load the necessary library
library(glmnet)
set.seed(42)
# Convert the data to a matrix format for glmnet
X_train_matrix <- as.matrix(X_train)

Y_train_vector <- train_data$Weight
# Fit Lasso regression model
lasso_model <- cv.glmnet(X_train_matrix, Y_train_vector, alpha = 1)

# Extract coefficients from the Lasso model
lasso_coef <- coef(lasso_model)

# Print the coefficients
print(lasso_coef)
## From this print out, we can see that six predictors were dropped in the model due to regularization.

#f)
set.seed(42)

# Predictions using PCR on the test set
pcr_pred_test <- predict(pcr_model, newdata = test_data, ncomp = 5)

# Predictions using PLSR on the test set
plsr_pred_test <- predict(plsr_model, newdata = test_data, ncomp = 4)

# Predictions using Lasso on the test set
X_test_matrix <- as.matrix(X_test)
lasso_pred <- predict(lasso_model, newx = X_test_matrix, s = "lambda.min")

Y_test <- test_data$Weight
# Calculate the Mean Squared Error (MSE) for Lasso on the test set
mse_lasso <- mean((lasso_pred - Y_test)^2)
mse_pcr_test <- mean((pcr_pred_test - Y_test)^2)
mse_plsr_test <- mean((plsr_pred_test - Y_test)^2)

cat("MSE for Lasso on the test set:", mse_lasso, "\n")
cat("MSE for PCR on the test set:", mse_pcr_test, "\n")
cat("MSE for PLSR on the test set:", mse_plsr_test, "\n")

#2----

library(randomForest)

set.seed(42)
BodyDF=data.frame(read.table("body.dat.txt",header=F))
colnames(BodyDF) = c("BiacromialDiam", "BiiliacDiam", "BitrochantericDiam", "ChestDepth", "ChestDiam", "ElbowDiam", "WristDiam", "KneeDiam", "AnkleDiam",
                     "ShoulderGirth", "ChestGirth", "WaistGirth", "NavelGirth", "HipGirth", "ThighGirth", "BicepGirth", "ForearmGirth", "KneeGirth",
                     "CalfMaxGirth", "AnkleMinGirth", "WristMinGirth",
                     "Age", "Weight", "Height", "Gender")

Y=cbind(BodyDF$Age, BodyDF$Weight, BodyDF$Height, BodyDF$Gender)
Y = BodyDF$Weight
X=cbind(BodyDF$BiacromialDiam,BodyDF$BiiliacDiam,BodyDF$BitrochantericDiam,BodyDF$ChestDepth,BodyDF$ChestDiam,BodyDF$ElbowDiam,
        BodyDF$WristDiam,BodyDF$KneeDiam,BodyDF$AnkleDiam,BodyDF$ShoulderGirth,BodyDF$ChestGirth,BodyDF$WaistGirth,BodyDF$NavelGirth,BodyDF$HipGirth,
        BodyDF$ThighGirth,BodyDF$BicepGirth,BodyDF$ForearmGirth,BodyDF$KneeGirth,BodyDF$CalfMaxGirth,BodyDF$AnkleMinGirth,BodyDF$WristMinGirth)

# Create logical vectors for training and test sets with 200 observations in the test set
test_indices <- sample(1:nrow(BodyDF), 200)
train <- rep(TRUE, nrow(BodyDF))
train[test_indices] <- FALSE
test <- !train

# Split the data into training and test sets
train_data <- BodyDF[train, ]
test_data <- BodyDF[test, ]

# Extract predictor variables (X) and response variable (Y) for training and test sets
X_train <- train_data[, c("BiacromialDiam", "BiiliacDiam", "BitrochantericDiam", "ChestDepth", "ChestDiam", "ElbowDiam", "WristDiam", "KneeDiam", "AnkleDiam",
                          "ShoulderGirth", "ChestGirth", "WaistGirth", "NavelGirth", "HipGirth", "ThighGirth", "BicepGirth", "ForearmGirth", "KneeGirth",
                          "CalfMaxGirth", "AnkleMinGirth", "WristMinGirth")]

Y_train <- train_data$Weight

X_test <- test_data[, c("BiacromialDiam", "BiiliacDiam", "BitrochantericDiam", "ChestDepth", "ChestDiam", "ElbowDiam", "WristDiam", "KneeDiam", "AnkleDiam",
                        "ShoulderGirth", "ChestGirth", "WaistGirth", "NavelGirth", "HipGirth", "ThighGirth", "BicepGirth", "ForearmGirth", "KneeGirth",
                        "CalfMaxGirth", "AnkleMinGirth", "WristMinGirth")]

Y_test <- test_data$Weight

# Bagging Model
num_predictors <- ncol(X_train)
print(num_predictors)

# Set mtry to the total number of predictors
mtry_value <- num_predictors

# Fit bagged model with mtry = num_predictors
bagging_model <- randomForest(Weight ~ ., data = train_data, mtry = mtry_value, importance = TRUE)

#RF Model
num_predictors <- round(sqrt(ncol(X_train)))
mtry_value <- num_predictors
rf_model <- randomForest(Weight ~ ., data = train_data, mtry = mtry_value, importance = TRUE)



#a)
num_trees <- seq(1, 500, by = 10)  

bagging_errors <- numeric(length(num_trees))
random_forest_errors <- numeric(length(num_trees))

for (i in seq_along(num_trees)) {
  # Bagging
  bagging_model <- randomForest(Weight ~ ., data = train_data, mtry = num_predictors, ntree = num_trees[i])
  yhat_bag <- predict(bagging_model, newdata = test_data)
  bagging_errors[i] <- mean((yhat_bag - test_data$Weight)^2)
  
  # Random Forest
  rf_model <- randomForest(Weight ~ ., data = train_data, mtry = num_predictors, ntree = num_trees[i])
  yhat_rf <- predict(rf_model, newdata = test_data)
  random_forest_errors[i] <- mean((yhat_rf - test_data$Weight)^2)
}

# Plot the test errors against the number of trees
plot(num_trees, bagging_errors, type = "l", col = "black", xlab = "Number of Trees",
     ylab = "Test MSE", main = "Bagging and Random Forests",
     ylim = c(0, max(bagging_errors, random_forest_errors)))

lines(num_trees, random_forest_errors, col = "red", lty = 2)

legend("topright", legend = c("Bagging", "Random Forest"), col = c("black", "orange"), lty = 1:2)

#b)
importance(rf_model)
importance(bagging_model)

varImpPlot(rf_model)
varImpPlot(bagging_model)

# Both models put WaistGirth, ChestGirth, and BicepGirth as the top three predictors.
# From there are are many differences, but it is interesting how they have the same top three. 

#C 
set.seed(42)
num_predictors <- round(sqrt(ncol(X_train)))
mtry_value <- num_predictors
rf_model <- randomForest(Weight ~ ., data = train_data, mtry = mtry_value, importance = TRUE, ntree = 500)

num_predictors <- ncol(X_train)
mtry_value <- num_predictors
bagging_model <- randomForest(Weight ~ ., data = train_data, mtry = mtry_value, importance = TRUE, ntree = 500)

# Make predictions on the test data for both models
rf_pred <- predict(rf_model, newdata = test_data)
bagging_pred <- predict(bagging_model, newdata = test_data)

# Calculate MSE for both models
mse_rf <- mean((rf_pred - test_data$Weight)^2)
mse_bagging <- mean((bagging_pred - test_data$Weight)^2)

# Print the MSE for comparison
cat("MSE for Random Forest:", mse_rf, "\n")
cat("MSE for Bagging:", mse_bagging, "\n")
cat("MSE for Lasso on the test set:", mse_lasso, "\n")
cat("MSE for PCR on the test set:", mse_pcr_test, "\n")
cat("MSE for PLSR on the test set:", mse_plsr_test, "\n")

# Our random forest does not make better predictions than our PLSR model did for question number 1.

#d)

num_trees <- seq(1, 1000, by = 10)  

bagging_errors <- numeric(length(num_trees))
random_forest_errors <- numeric(length(num_trees))

for (i in seq_along(num_trees)) {
  # Bagging
  bagging_model <- randomForest(Weight ~ ., data = train_data, mtry = num_predictors, ntree = num_trees[i])
  yhat_bag <- predict(bagging_model, newdata = test_data)
  bagging_errors[i] <- mean((yhat_bag - test_data$Weight)^2)
  
  # Random Forest
  rf_model <- randomForest(Weight ~ ., data = train_data, mtry = num_predictors, ntree = num_trees[i])
  yhat_rf <- predict(rf_model, newdata = test_data)
  random_forest_errors[i] <- mean((yhat_rf - test_data$Weight)^2)
}

# Plot the test errors against the number of trees
plot(num_trees, bagging_errors, type = "l", col = "black", xlab = "Number of Trees",
     ylab = "Test MSE", main = "Bagging and Random Forests",
     ylim = c(0, max(bagging_errors, random_forest_errors)))

lines(num_trees, random_forest_errors, col = "red", lty = 2)

legend("topright", legend = c("Bagging", "Random Forest"), col = c("black", "orange"), lty = 1:2)

# I know this is only supposed to be for the RF model, but I included both just to see.
# Looking at the graph, I extended it out to 1000 trees and it still flat lined, which is why I dont think it would be valuable to include more trees.
# I really didn't need to do it out to 1000, because it clearly hits a flat line well before 500. 


#3----
#a)
set.seed(42)
library(ISLR2)
library(e1071)
data = OJ

train_indices <- sample(1:nrow(OJ), 800)
train_data <- OJ[train_indices, ]
test_data <- OJ[-train_indices, ]
#b)
svm_model_linear <- svm(Purchase ~ ., data = train_data, kernel = "linear", cost = 0.01)
summary(svm_model_linear)

# In this model, we have two classes we are predicting (CH, MM) and we are using 432 support vectors.

#c)
train_pred_linear <- predict(svm_model_linear, newdata = train_data)
test_pred_linear <- predict(svm_model_linear, newdata = test_data)
train_error_linear <- mean(train_pred_linear != train_data$Purchase)
test_error_linear <- mean(test_pred_linear != test_data$Purchase)
cat("Training Error Rate (Linear Kernel):", train_error_linear, "\n")
cat("Test Error Rate (Linear Kernel):", test_error_linear, "\n")


#d)
tune.out <- tune(svm, Purchase ~ ., data = train_data, kernel = "linear", 
                 ranges = list(cost = c( 0.01, 0.1, 1, 5, 10)))
svm_model_linear_optimal <- tune.out$best.model
summary(svm_model_linear_optimal)

#e)

train_pred_linear_optimal <- predict(svm_model_linear_optimal, newdata = train_data)
train_error_optimal_linear <- mean(train_pred_linear_optimal != train_data$Purchase)
test_pred_optimal <- predict(svm_model_linear_optimal, newdata = test_data)
test_error_optimal_linear <- mean(test_pred_optimal != test_data$Purchase)
cat("Training Error Rate (Optimal Linear Kernel):", train_error_optimal_linear, "\n")
cat("Test Error Rate (Optimal Linear Kernel):", test_error_optimal_linear, "\n")

#f)
svm_model_radial <- svm(Purchase ~ ., data = train_data, kernel = "radial", cost = 0.01)
summary(svm_model_radial)
train_pred_radial <- predict(svm_model_radial, newdata = train_data)
test_pred_radial <- predict(svm_model_radial, newdata = test_data)
train_error_radial <- mean(train_pred_radial != train_data$Purchase)
test_error_radial <- mean(test_pred_radial != test_data$Purchase)
cat("Training Error Rate (Radial Kernel):", train_error_radial, "\n")
cat("Test Error Rate (Radial Kernel):", test_error_radial, "\n")
tune.out <- tune(svm, Purchase ~ ., data = train_data, kernel = "radial", 
                 ranges = list(cost = c( 0.01, 0.1, 1, 5, 10)))
svm_model_radial_optimal <- tune.out$best.model
summary(svm_model_radial_optimal)
train_pred_radial_optimal <- predict(svm_model_radial_optimal, newdata = train_data)
train_error_optimal_radial <- mean(train_pred_radial_optimal != train_data$Purchase)
test_pred_optimal <- predict(svm_model_radial_optimal, newdata = test_data)
test_error_optimal_radial <- mean(test_pred_optimal != test_data$Purchase)
cat("Training Error Rate (Optimal Radial Kernel):", train_error_optimal_radial, "\n")
cat("Test Error Rate (Optimal Radial Kernel):", test_error_optimal_radial, "\n")

#g)

svm_model_poly <- svm(Purchase ~ ., data = train_data, kernel = "polynomial", degree = 2, cost = 0.01)
summary(svm_model_poly)
train_pred_poly <- predict(svm_model_poly, newdata = train_data)
train_error_poly <- mean(train_pred_poly != train_data$Purchase)
test_pred_poly <- predict(svm_model_poly, newdata = test_data)
test_error_poly <- mean(test_pred_poly != test_data$Purchase)
cat("Training Error Rate (Polynomial Kernel):", train_error_poly, "\n")
cat("Test Error Rate (Polynomial Kernel):", test_error_poly, "\n")
tune.out_poly <- tune(svm, Purchase ~ ., data = train_data, kernel = "polynomial", degree = 2,
                      ranges = list(cost = c(0.01, 0.1, 1, 5, 10)))
svm_model_poly_optimal <- tune.out_poly$best.model
summary(svm_model_poly_optimal)
train_pred_poly_optimal <- predict(svm_model_poly_optimal, newdata = train_data)
train_error_poly_optimal <- mean(train_pred_poly_optimal != train_data$Purchase)
test_pred_poly_optimal <- predict(svm_model_poly_optimal, newdata = test_data)
test_error_poly_optimal <- mean(test_pred_poly_optimal != test_data$Purchase)
cat("Training Error Rate (Optimal Polynomial Kernel):", train_error_poly_optimal, "\n")
cat("Test Error Rate (Optimal Polynomial Kernel):", test_error_poly_optimal, "\n")

#h)
cat("Training Error Rate (Optimal Linear Kernel):", train_error_optimal_linear, "\n")
cat("Test Error Rate (Optimal Linear Kernel):", test_error_optimal_linear, "\n")
cat("Training Error Rate (Optimal Radial Kernel):", train_error_optimal_radial, "\n")
cat("Test Error Rate (Optimal Radial Kernel):", test_error_optimal_radial, "\n")
cat("Training Error Rate (Optimal Polynomial Kernel):", train_error_poly_optimal, "\n")
cat("Test Error Rate (Optimal Polynomial Kernel):", test_error_poly_optimal, "\n")

# The Radial kernel give the best results on the testing data. The polynomial fit the training data the best, but overfit which led to worse test results. 




#4----
xdata <- data.matrix(NYSE[, c("DJ_return", "log_volume", "log_volatility", "day_of_week")])
istrain <- NYSE[, "train"]
xdata <- scale(xdata)


lagm <- function(x, k = 1) {
  n <- nrow(x)
  pad <- matrix(NA, k, ncol(x))
  rbind(pad, x[1:(n - k), ])
}

arframe <- data.frame(
  log_volume = xdata[, "log_volume"],
  L1 = lagm(xdata, 1), L2 = lagm(xdata, 2),
  L3 = lagm(xdata, 3), L4 = lagm(xdata, 4),
  L5 = lagm(xdata, 5),
  day_of_week = xdata[, "day_of_week"]
)
arframe <- arframe[-(1:5), ]
istrain <- istrain[-(1:5)]

arfit <- lm(log_volume ~ ., data = arframe[istrain, ])
arpred <- predict(arfit, arframe[!istrain, ])
V0 <- var(arframe[!istrain, "log_volume"])
1 - mean((arpred - arframe[!istrain, "log_volume"])^2) / V0

arframed <- data.frame(day_of_week = NYSE[-(1:5), "day_of_week"], arframe)
arfitd <- lm(log_volume ~ ., data = arframed[istrain, ])
arpredd <- predict(arfitd, arframed[!istrain, ])
1 - mean((arpredd - arframe[!istrain, "log_volume"])^2) / V0


n <- nrow(arframe)
xrnn <- data.matrix(arframe[, -1])
xrnn <- array(xrnn, c(n, 4, 5))  # Adjusted to match the correct input shape
xrnn <- xrnn[, , 5:1]
xrnn <- aperm(xrnn, c(1, 3, 2))


model <- keras_model_sequential() %>%
  layer_simple_rnn(units = 12,
                   input_shape = list(5, 4),  # Adjusted to match the correct input shape
                   dropout = 0.1, recurrent_dropout = 0.1) %>%
  layer_dense(units = 1)
model %>% compile(optimizer = optimizer_rmsprop(),
                  loss = "mse")


history <- model %>% fit(
  xrnn[istrain, , ], arframe[istrain, "log_volume"],
  batch_size = 64, epochs = 75,
  validation_data = list(xrnn[!istrain, , ], arframe[!istrain, "log_volume"])
)

kpred <- predict(model, xrnn[!istrain, , ])
r2 = 1 - mean((kpred - arframe[!istrain, "log_volume"])^2) / V0


model <- keras_model_sequential() %>%
  layer_flatten(input_shape = c(5, 4)) %>%
  layer_dense(units = 1)
x <- model.matrix(log_volume ~ . - 1, data = arframed)
colnames(x)

arnnd <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = 'relu',
              input_shape = ncol(x)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)
arnnd %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop())


history <- arnnd %>% fit(
  x[istrain, ], arframe[istrain, "log_volume"], epochs = 30,
  batch_size = 32, validation_data = list(x[!istrain, ], arframe[!istrain, "log_volume"])
)


plot(history)


npred <- predict(arnnd, x[!istrain, ])
r2_second = 1 - mean((arframe[!istrain, "log_volume"] - npred)^2) / V0

print(r2)
print(r2_second)

