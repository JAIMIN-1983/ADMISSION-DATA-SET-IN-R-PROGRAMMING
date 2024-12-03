# Multiple regression in machine learning

install.packages("tidyverse")  # Install tidyverse if you haven't already
library(tidyverse)

# Load the data set
data <- read.csv("D:\\JAIMIN\\Data Science\\R Programming\\admission_data.csv")

# Check for missing values
sum(is.na(data))

# Remove rows with missing values (if necessary)
data <- na.omit(data)

# View the structure of the data
str(data)

install.packages("caret")
library(caret)

# Set seed for reproducibility
set.seed(42)

# Create a train-test split
trainIndex <- createDataPartition(data$Chance.of.Admit, p = .7, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- data[trainIndex, ]
dataTest <- data[-trainIndex, ]

# Fit the multiple regression model
model <- lm(formula = Chance.of.Admit ~ ., data = dataTrain)

# View the summary of the model
summary(model)

# Make predictions
predictions <- predict(model, newdata = dataTest)

# View the predictions
head(predictions)

# Calculate Mean Squared Error (MSE)
mse <- mean((predictions - dataTest$Chance.of.Admit)^2)
print(paste("Mean Squared Error:", mse))

# Calculate R-squared
rsq <- 1 - (sum((dataTest$Chance.of.Admit - predictions)^2) / sum((dataTest$Chance.of.Admit - mean(dataTest$Chance.of.Admit))^2))
print(paste("R-squared:", rsq))

# Create a data frame for plotting
results <- data.frame(Actual = dataTest$Chance.of.Admit, Predicted = predictions)

# Plot actual vs predicted values
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

