#Load Packages
library(caret)
library(randomForest)
library(ggplot2)
#Load Data
red_data <- read.csv("winequality-red.csv", sep=';')

# Transform the 'quality' into 10 distinct classes to be able to classify 
#each one grade level
red_data$quality <- as.factor(red_data$quality)

# Set seed 
set.seed(123)

# Split the data into training and test set using a 70/30 split
train_partition <- createDataPartition(red_data$quality, p=0.7, list=FALSE)
train_data <- red_data[train_partition,]
test_data <- red_data[-train_partition,]

# Train the model based on the training data
model <- randomForest(quality ~ ., data = train_data, ntree=150)

# Print the model
print(model)

# Make predictions based on the test data
predictions <- predict(model, test_data)

# Evaluate the model
confusion_matrix <- confusionMatrix(predictions, test_data$quality)
print(confusion_matrix)

#Plot the variable importance
varImpPlot(model, main= "Red Wine Feature Importance", pch=16)
###########
