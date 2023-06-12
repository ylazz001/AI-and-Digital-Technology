#Load Packages
library(caret)
library(randomForest)
#Load data
red_data <- read.csv("winequality-red.csv", sep=';')

# Set seed 
set.seed(123)

# Split the data into training and test set using a 70/30 split
train_partition <- createDataPartition(red_data$quality, p=0.7, list=FALSE)
train_data <- red_data[train_partition,]
test_data <- red_data[-train_partition,]

# Train the model
model <- randomForest(quality ~ ., data = train_data, ntree=200)

# Print the model
print(model)

# Make predictions based on test data
predictions <- predict(model, test_data)

# Calculate the absolute differences between predictions and actual scores
differences <- abs(predictions - test_data$quality)

# Define a tolerance level
tolerance <- 1

# Calculate the percentage of predictions within the quality_tolerance level
# differences <= quality_tolerance creates a vector where each element is TRUE if the 
# corresponding difference is within quality_tolerance level, and FALSE if it is not
# mean calculates the mean between TRUE (treated as 1) and FALSE (treated as 0)
accuracy <- mean(differences <= tolerance) * 100

print(paste("Accuracy: ", round(accuracy, 2), "%", sep=""))

## EXTRA details

# Correlation Coefficient
correlation_coefficient <- cor(predictions, test_data$quality)
print(paste("Correlation Coefficient: ", round(correlation_coefficient, 2), sep=""))

# Calculate Residuals
residuals = test_data$quality - predictions
# Print Mean Squared Error (MSE)
mse = mean(residuals^2)
print(paste('Mean Squared Error (MSE):', mse))

# Print Root Mean Squared Error (RMSE)
rmse = sqrt(mse)
print(paste('Root Mean Squared Error (RMSE):', rmse))

# Print Mean Absolute Error (MAE)
mae = mean(abs(residuals))
print(paste('Mean Absolute Error (MAE):', mae))

# Calculating the R-Squared
ss_total = sum((test_data$quality - mean(test_data$quality))^2)
ss_residual = sum((test_data$quality - predictions)^2)
r_squared = 1 - (ss_residual/ss_total)
print(paste('R-Squared:', r_squared))


