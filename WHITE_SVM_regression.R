#Load Packages
library(caret)
library(e1071)

#Load data
data <- read.csv("winequality-white.csv", sep=';')

# Split the data into training and testing sets. Use an 80/20 split
set.seed(123)
train_partition <- createDataPartition(data$quality, p = .8, list = FALSE, times = 1)
train_data <- data[train_partition,]
test_data  <- data[-train_partition,]

# Set seed and Train the model using Gaussian kernel
set.seed(123)
svm_model <- svm(quality ~ ., data=train_data, kernel="radial")

# Print model summary
print(svm_model)

# Make predictions based on test data
predictions <- predict(svm_model, test_data)

# Evaluate model performance by finding RMSE, R^2 and MAE
postResample(pred = predictions, obs = test_data$quality)


# Calculate the absolute differences between predictions and actual quality scores
differences <- abs(predictions - test_data$quality)

# Define a quality_tolerance level
quality_tolerance <- 1

# Calculate the percentage of predictions within the quality_tolerance level
# differences <= quality_tolerance creates a vector where each element is TRUE if the 
# corresponding difference is within quality_tolerance level, and FALSE if it is not
# mean calculates the mean between TRUE (treated as 1) and FALSE (treated as 0)
accuracy <- mean(differences <= quality_tolerance) * 100

print(paste("Accuracy: ", round(accuracy, 2), "%", sep=""))

## EXTRA details
# Correlation Coefficient
correlation_coefficient <- cor(predictions, test_data$quality)
print(paste("Correlation Coefficient: ", round(correlation_coefficient, 2), sep=""))

# Mean Squared Error
mean_squared_error <- mean((predictions - test_data$quality)^2)
print(paste("Mean Squared Error: ", round(mean_squared_error, 2), sep=""))

# Mean Absolute Percentage Error
mean_abs_perc_error <- mean(abs((predictions - test_data$quality) / test_data$quality))
print(paste("Mean Absolute Percentage Error: ", round(mean_abs_perc_error, 2),  sep=""))



