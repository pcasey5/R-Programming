#1----
#a)
library(caret)
library(ISLR2)
library(caTools)
data = College

#split data
set.seed(42)
sample = sample.split(data$Private, SplitRatio = .8)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)

#b)
lm_app = lm(Apps~., data = train)
prediction = predict(lm_app, newdata = test)

mse = mean((prediction - test$Apps)^2)
print(mse)

#c)

library(glmnet)
x <- model.matrix(Apps ~ ., train)[, -1]
y <- train$Apps
grid <- 10^seq(10, -2, length = 100)
ridge.mod = cv.glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge.mod)) #SEE THE NUMBER OF PREDICTORS AND LAMBDA

#TEST NOW
x_test = model.matrix(Apps ~ ., test)[, -1]
y_test = test$Apps
ridge_pred = predict(ridge.mod, newx = x_test, s = "lambda.min")
ridge_test_error <- mean((ridge_pred - y_test)^2)
print(ridge_test_error)

#d)
x <- model.matrix(Apps ~ ., train)[, -1]
y <- train$Apps
grid <- 10^seq(10, -2, length = 100)
lasso.mod = cv.glmnet(x, y, alpha = 1, lambda = grid)
lasso_pred = predict(lasso.mod, newx = model.matrix(Apps ~ ., test)[, -1], s = "lambda.min")
lasso_test_error = mean((lasso_pred - test$Apps)^2)
print(lasso_test_error)
num_nonzero = sum(coef(lasso.mod, s = "lambda.min") != 0)
print(num_nonzero)
plot(lasso.mod)


#e)
library(pls)
pcr_model = pcr(Apps ~ ., data = train, scale = TRUE, validation = "CV")
validationplot(pcr_model, val.type = "MSEP")
summary(pcr_model)
x_test = model.matrix(Apps ~ ., test)[, -1]
y_test = test$Apps
selected_components = pcr_model$ncomp
print(selected_components)
pcr_pred = predict(pcr_model, newdata = x_test, ncomp = selected_components)
pcr_test_error = mean((pcr_pred - y_test)^2)
print(pcr_test_error)


#f)
pls_model = plsr(Apps ~ ., data = train, scale = TRUE, validation = "CV")
summary(pls_model)
validationplot(pls_model, val.type = "MSEP")
selected_components = pls_model$ncomp
print(selected_components)
pls_pred = predict(pls_model, newdata = x_test, ncomp = selected_components)
pls_test_error = mean((pls_pred - y_test)^2)
print(pls_test_error)

#g)
print(paste('Linear Model:', mse))
print(paste('Ridge MSE:', ridge_test_error))
print(paste('Lasso MSE:', lasso_test_error))
print(paste('PCR MSE:', pcr_test_error))
print(paste('PLS MSE:', pls_test_error))

lasso_rsquared = cor(lasso_pred, test$Apps)^2
lasso_rmse = sqrt(mean((lasso_pred - test$Apps)^2))
ape = abs((lasso_pred - test$Apps) / test$Apps)
mape = mean(ape) * 100

print(paste("LASSO RMSE:", lasso_rmse))
print(paste("LASSO R-squared:", lasso_rsquared))
print(paste("LASSO MAPE:", mape))

# When we compare our models, Lasso has a significant decrease by almost 7000 MSE. 
# After finding the R-square, RMSE, and MAPE, I think it is safe to say that we can predict the number of college applications recieved with reasonable accuracy.
# The r squared is high which means that our Lasso model does a good job of explaining the variance in the target variable. 
# I do wish RMSE and MAPE were a bit lower, but overall I think the model is decent. 

#2----
#a)
library(ISLR2)
data = Boston
set.seed(42)
# It didn't ask to split, but I am going to. 
train_indices = sample(1:nrow(Boston), 0.8 * nrow(Boston))
train = Boston[train_indices, ]
test = Boston[-train_indices, ]

poly3 = lm(nox ~ poly(dis, degree = 3, raw = TRUE), data = train)
summary(poly3)
coef(summary(poly3)) #simplier output for coef

# Create a grid of values for dis at which we want predictions
dis.lims <- range(train$dis)
dis.grid <- seq(from = dis.lims[1], to = dis.lims[2], length.out = 100)

# Make predictions on the grid with standard errors
preds <- predict(poly3, newdata = data.frame(dis = dis.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

# Plot the data and add the cubic polynomial fit
plot(train$dis, train$nox, main = "Cubic Polynomial Fit", xlab = "dis", ylab = "nox")
lines(sort(train$dis), predict(poly3, data.frame(dis = sort(train$dis))), col = "red")

# Add the fit from the cubic polynomial along with confidence bands
lines(dis.grid, preds$fit, lwd = 2, col = "blue")
matlines(dis.grid, se.bands, lwd = 1, col = "blue", lty = 3)

#b)
# Initialize a vector to store RSS values
rss_values = numeric(10)

# Loop through polynomial degrees from 1 to 10
for (degree in 1:10) {
  # Fit a polynomial regression
  poly_model = lm(nox ~ poly(dis, degree = degree, raw = TRUE), data = train)
  
  # Make predictions on the test set
  predictions = predict(poly_model, newdata = data.frame(dis = test$dis))
  
  # Calculate RSS
  rss_values[degree] = sum((test$nox - predictions)^2)
  
}

# To plot all the different poly models, change the degre_to_plot
degree_to_plot = 5

# Plot the polynomial fit for the chosen degree
plot(test$dis, test$nox, main = paste("Polynomial Fit (Degree =", degree_to_plot, ")"), 
     xlab = "dis", ylab = "nox")
poly_model = lm(nox ~ poly(dis, degree = degree_to_plot, raw = TRUE), data = train)
lines(sort(test$dis), predict(poly_model, data.frame(dis = sort(test$dis))), col = "red")

# Report RSS values
cat("Degree\tRSS\n")
for (degree in 1:10) {
  cat(degree, "\t", rss_values[degree], "\n")
}

#c)
set.seed(42)

library(boot)

# Initialize a vector to store cross-validated mean squared errors (MSE)
cv_mse_values = numeric(10)

# Loop through polynomial degrees from 1 to 10
for (degree in 1:10) {
  # Fit a polynomial regression
  poly_model = glm(nox ~ poly(dis, degree = degree, raw = TRUE), data = train)
  
  # Perform cross-validation
  cv_results = cv.glm(train, poly_model, K = 10)
  
  # Average MSE over the folds
  cv_mse_values[degree] = mean(cv_results$delta^2)
}

# Find the degree that minimizes cross-validated MSE
best_degree_cv = which.min(cv_mse_values)
cat("Best Degree (Minimizing Cross-Validated MSE):", best_degree_cv, "\n")

# Plot cross-validated MSE values for each degree
plot(1:10, cv_mse_values, type = "b", main = "Cross-Validated MSE vs. Degree", 
     xlab = "Degree", ylab = "Cross-Validated Mean Squared Error")
# Poly7 is the best model according to cv. It has the lowest MSE when folded 10 times.
# This can be seen visually in the polt created. 

#d)


library(splines)

# Find the quantiles so they can be the knots
quantiles <- quantile(train$nox, probs = c(0.25, 0.5, 0.75))
spline_model <- lm(nox ~ bs(dis, df = 4, knots = quantiles), data = train)

# Predict on nox
dis.grid <- seq(min(test$dis), max(test$dis), length.out = 100)
pred <- predict(spline_model, newdata = data.frame(dis = dis.grid), se = TRUE)

# Plot the results
plot(test$dis, test$nox, col = "gray", main = "Regression Spline (4 DF) with Quantiles as Knots", xlab = "dis", ylab = "nox")
lines(dis.grid, pred$fit, lwd = 2)
lines(dis.grid, pred$fit + 2 * pred$se.fit, lty = "dashed")
lines(dis.grid, pred$fit - 2 * pred$se.fit, lty = "dashed")
summary(spline_model)
# I chose my knots based upon the quantiles. There seems to be a lot of multicolinearity in this model.


#e)

# Initialize vectors to store results
rss_values_spline = numeric(10)

# Cross-validation for different degrees of freedom for spline
for (df in 1:10) {
  errors = numeric(10)
  
  for (i in 1:10) {
    val_indices <- ((i - 1) * nrow(train)/10 + 1):(i * nrow(train)/10)
    train_indices <- setdiff(1:nrow(train), val_indices)
    
    # Fit the spline model on training data
    spline_model = lm(nox ~ bs(dis, df = df), data = train[train_indices, ])
    
    # Predict on the validation set
    pred = predict(spline_model, newdata = data.frame(dis = train[val_indices, ]$dis), se = TRUE)
    
    # Compute mean squared error
    errors[i] = mean((train[val_indices, ]$nox - pred$fit)^2)
  }
  
  # Print out some information to debug
  cat("Degree of Freedom:", df, " | RSS:", errors, "\n")
  
  rss_values_spline[df] = sum(errors)
}



# Plot spline fits for different degrees of freedom with legend
plot(test$dis, test$nox, col = "gray", main = "Regression Spline with Altering Degrees of Freedom", xlab = "dis", ylab = "nox")
legend("topright", legend = paste("DF =", 1:10), col = 2:11, lty = 1:10, cex = 0.8)

for (df in 1:10) {
  spline_model = lm(nox ~ bs(dis, df = df), data = train)
  lines(sort(test$dis), predict(spline_model, newdata = data.frame(dis = sort(test$dis))), col = df + 1, lwd = 2)
}

# Find the optimal degree of freedom with the minimum mean RSS
optimal_df = which.min(rss_values_spline)
cat("Optimal Degree of Freedom:", optimal_df, "\n")
# Report the resulting RSS values
rss_values_spline

# While cycling through the dfs from 1 to 10, we see that ten df is the optimal model for minimizing rss. 

#f)
# Initialize vectors to store results
mse_values_spline = numeric(10)

# Number of folds for cross-validation
k = 10

# Cross-validation for different degrees of freedom for spline
for (df in 1:10) {
  errors = numeric(k)
  
  for (i in 1:k) {
    val_indices = ((i - 1) * nrow(train)/k + 1):(i * nrow(train)/k)
    train_indices = setdiff(1:nrow(train), val_indices)
    
    # Fit the spline model on training data
    spline_model = lm(nox ~ bs(dis, df = df), data = train[train_indices, ])
    
    # Predict on the validation set
    pred = predict(spline_model, newdata = data.frame(dis = train[val_indices, ]$dis), se = TRUE)
    
    # Compute mean squared error
    errors[i] = mean((train[val_indices, ]$nox - pred$fit)^2)
  }
  
  # Print out some information to debug
  cat("Degree of Freedom:", df, " | Mean MSE:", mean(errors), "\n")
  
  mse_values_spline[df] = mean(errors)
}

# Plot spline fits for different degrees of freedom with legend
plot(test$dis, test$nox, col = "gray", main = "Regression Spline with Altering Degrees of Freedom", xlab = "dis", ylab = "nox")
legend("topright", legend = paste("DF =", 1:10), col = 2:11, lty = 1:10, cex = 0.8)

for (df in 1:10) {
  spline_model = lm(nox ~ bs(dis, df = df), data = train)
  lines(sort(test$dis), predict(spline_model, newdata = data.frame(dis = sort(test$dis))), col = df + 1, lwd = 2)
}

# Find the optimal degree of freedom with the minimum mean MSE
optimal_df = which.min(mse_values_spline)
cat("Optimal Degree of Freedom:", optimal_df, "\n")
# Report the resulting MSE values
mse_values_spline

# With the cv of 10 folds, we are still seeing that ten df is providing the lowest mse. 

#3----
#a)
set.seed(42)
library(tree)
train_indices = sample(1:nrow(OJ), 800)
train = OJ[train_indices, ]
test = OJ[-train_indices, ]

#b)
tree_model = tree(Purchase ~ ., data = train)
summary(tree_model)

# Our tree has 7 terminal nodes. The training error rate is 17.75%. The interesting part to me is only two variable were chosen to be in the model; LoyalCH and PriceDiff. 

#c)
tree_model


# If we look at the terminal node: 4) LoyalCH < 0.064156 64    0.00 MM ( 0.00000 1.00000 ), we see that if LoyalCH is less than .064, the decesion is to go left in the predicted class of MM.
# We also see that this node contains 64 observations with the predicted probability of being CH equal to zero if this condition is true aka goes left. 

#d)
plot(tree_model)
text(tree_model, pretty = 0)
# We see that the most important decision is LoyalCH<.48285. If you go to the right, you will most likely find a CH fan unless the price difference is dramatic. If you go to the left, you will find a MM fan most of the time. 

#e)
# Predict on the test data
tree_pred = predict(tree_model, newdata = test, type = "class")

# Create a confusion matrix
conf_matrix_tree = table(tree_pred, test$Purchase)
conf_matrix_tree
# Calculate the test error rate
test_error_rate = 1 - sum(diag(conf_matrix_tree)) / sum(conf_matrix_tree)
test_error_rate

#f
cv_result = cv.tree(tree_model)

# Optimal tree size
optimal_tree_size = cv_result$size[which.min(cv_result$dev)]
optimal_tree_size

#g)
plot(cv_result$size, cv_result$dev, type = "b", xlab = "Tree Size", ylab = "Cross-validated Error Rate")

#h)
# A tree size of five has the lowest cross-validated classification error rate.

#i)

pruned_tree = prune.tree(tree_model, best = optimal_tree_size)

plot(pruned_tree)
text(pruned_tree, pretty = 0)

#j)
summary(tree_model)
summary(pruned_tree)

# The pruned tree has a higher error rate while the unpruned tree has a lower error rate. 
#k)

# Prediction for the unpruned tree
tree_pred = predict(tree_model, newdata = test, type = "class")

# Create a confusion matrix for the unpruned tree
conf_matrix_tree = table(tree_pred, test$Purchase)

# Calculate the test error rate for the unpruned tree
test_error_rate_tree = 1 - sum(diag(conf_matrix_tree)) / sum(conf_matrix_tree)

# Prediction for the pruned tree
prune_pred = predict(pruned_tree, newdata = test, type = "class")

# Create a confusion matrix for the pruned tree
conf_matrix_prune = table(prune_pred, test$Purchase)

# Calculate the test error rate for the pruned tree
test_error_rate_prune = 1 - sum(diag(conf_matrix_prune)) / sum(conf_matrix_prune)

# Print out the results
cat("Confusion Matrix - Unpruned Tree:\n")
print(conf_matrix_tree)

cat("\nTest Error Rate - Unpruned Tree: ", test_error_rate_tree, "\n\n")

cat("Confusion Matrix - Pruned Tree:\n")
print(conf_matrix_prune)

cat("\nTest Error Rate - Pruned Tree: ", test_error_rate_prune, "\n")
# As we can see, the unpruned tree has a lower error rate for the testing data while the pruned tree has a higher error rate. 

