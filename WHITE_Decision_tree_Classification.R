#Load Packages
library(caret)
library(rpart)
library(ggplot2)
#Load data

data <- read.csv("winequality-white.csv", sep=';')

# OPTIONAL to treat model as having threshold of quality instead of using FACTOR below
#Transform the 'quality' column into a binary variable indicating whether the quality is above 7
#data$quality <- as.factor(ifelse(data$quality >= 7, "high", "low"))

# Transform the 'quality' into 10 distinct classes to be able to classify 
#each one grade level
data$quality <- as.factor(data$quality)


# Split the data into training and testing sets with an 80/20 split
set.seed(123)
train_partition <- createDataPartition(data$quality, p = .8, list = FALSE, times = 1)
train_data <- data[train_partition,]
test_data  <- data[-train_partition,]

# Set seed and train the model
set.seed(123)
dt_model <- rpart(quality ~ ., data=train_data, method="class")

# Print model summary
#print(summary(dt_model))


# Predict on the test data
predictions <- predict(dt_model, test_data, type="class")

# Evaluate model performance
cm<-confusionMatrix(predictions, test_data$quality)
print(cm)


#OPTIONAL
#Calculating feature importance
importance<-varImp(dt_model)
print(importance)

# Create a data frame with the importance values of each feature
df <- data.frame(
  Variable = c("alcohol", "citric.acid", "density", "fixed.acidity", "pH", "residual.sugar", "sulphates", "total.sulfur.dioxide", "volatile.acidity", "chlorides", "free.sulfur.dioxide"),
  Values = c(126.847974, 18.030310, 28.202229, 38.378305, 4.446163, 3.811905, 83.852583, 52.058754, 87.954016, 0.000000, 0.000000)
)

# Create the bar plot
ggplot(df, aes(x = reorder(Variable, -Values), y = Values)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Variable", y = "Values", title = "Variable Importance") +
  theme_minimal()


#EXTRA OPTIONAL DETAILS: Precision, recall, and F1-measure for each class
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
