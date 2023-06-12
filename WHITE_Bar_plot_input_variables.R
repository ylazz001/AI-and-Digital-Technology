
# Read the data
data <- read.csv("winequality-white.csv", sep=';')

# Get the summary statistics
summary_stats <- data.frame(
  Min = apply(data, 2, min),
  Mean = apply(data, 2, mean),
  Max = apply(data, 2, max)
)

print(summary_stats)

# Create a data frame with the counts of each quality
quality_counts <- as.data.frame(table(data$quality))

# Rename the columns
colnames(quality_counts) <- c("Quality", "Count")

# Convert Quality to numeric
quality_counts$Quality <- as.numeric(as.character(quality_counts$Quality))

ggplot(quality_counts, aes(x = Quality, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Quality Grade", y = "Count", title = "Count of Wines by Quality Grade") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20), 
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16), 
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14)
  )







