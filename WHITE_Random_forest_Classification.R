#Load Packages
library(caret)
library(randomForest)
library(ggplot2)
#Load Data
white_data <- read.csv("winequality-white.csv", sep=';')
# Transform the 'quality' into 10 distinct classes to be able to classify 
#each one grade level
white_data$quality <- as.factor(white_data$quality)

# Set seed 
set.seed(123)

# Split the data into training and test set using a 70/30 split
train_partition <- createDataPartition(white_data$quality, p=0.7, list=FALSE)
train_data <- white_data[train_partition,]
test_data <- white_data[-train_partition,]

# Train the model based on the training data
model <- randomForest(quality ~ ., data = train_data, ntree=160)

# Print the model
print(model)

# Make predictions based on the test data
predictions <- predict(model, test_data)

# Evaluate the model
confusion_matrix <- confusionMatrix(predictions, test_data$quality)
print(confusion_matrix)

#Plot the variable importance
varImpPlot(model, main= "White Wine Feature Importance", pch=16)
###########