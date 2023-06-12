#Load packages
library(caret)
library(e1071)

# Load the data
data <- read.csv("winequality-white.csv", sep=';')

# Transform the 'quality' into 10 distinct classes to be able to classify 
#each one grade level# Transform the 'quality' into 10 distinct classes
data$quality <- as.factor(data$quality)

# Set seed 
set.seed(123)

# Split the data into training and test set with an 80/20 split
train_partition <- createDataPartition(data$quality, p=0.8, list=FALSE)
train_data <- data[train_partition,]
test_data  <- data[-train_partition,]

# Train the model using Gaussian kernel
svm_model <- svm(quality ~ ., data=train_data, kernel = "radial")

# Print the model
print(svm_model)

# Make predictions based on test data
predictions <- predict(svm_model, test_data)

# Evaluate the model
cm <- confusionMatrix(as.factor(predictions), as.factor(test_data$quality))
print(cm)

# EXTRA details: Precision, recall, and F1-measure for each class from 
# quality score 3 (the lowest in the data) to quality score 8 (the highest in 
# the data)

# for(i in 1:length(levels(test_data$quality))){
#   precision <- cm$byClass[i, "Pos Pred Value"]
#   recall <- cm$byClass[i, "Sensitivity"]
#   f1 <- 2 * (precision * recall) / (precision + recall)
#   
#   cat("\nFor class", levels(test_data$quality)[i], ":")
#   print(paste("Precision: ", precision))
#   print(paste("Recall: ", recall))
#   print(paste("F1-measure: ", f1))
# }

