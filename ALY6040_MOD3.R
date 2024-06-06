library(rmarkdown)
library(readxl)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(magrittr)
library(RColorBrewer)
library(tables)
library(ggpubr)
library(pander)
library(corrgram)
library(corrplot)
library(zoo)
library(car)
library(leaps)
library(ggplot2)

carinsurancedataset <- read_csv("carinsurance.csv")
# Step 1: Check for missing values
missing_values <- sapply(carinsurancedataset, function(x) sum(is.na(x)))
print("Missing Values:")
print(missing_values)
attach(carinsurance)
#deleting rows with zero risk factor 
carinsurance <- carinsurancedataset %>%
  filter(!is.na(risk_factor))  
# Remove rows with missing values in car_value
carinsurance <- carinsurance%>%
  na.omit(car_value)
# Step 2: Check the structure of the cleaned dataset
str(carinsurance)
# Step 1: Remove the "time" variable
carinsurance <- carinsurance %>%
  select(-time) 

# Step 1: Rename "day" as a categorical variable
carinsurance <- carinsurance %>%
  mutate(day = factor(day, levels = 0:6, labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
# Load the corrplot package
library(corrplot)

# Subset the dataset to include only numeric variables
numeric_data <- carinsurance[, sapply(carinsurance, is.numeric)]

# Compute the correlation matrix for the numeric variables
correlation_matrix <- cor(numeric_data, method = "pearson")

# Create the correlation plot
corrplot(correlation_matrix, method = "color")
# Again Checking for missing values
missing_values <- sapply(carinsurance, function(x) sum(is.na(x)))
print("Missing Values:")
print(missing_values)

#EDA 
summary(carinsurance)
#HISTOGRAM OF CAR AGE 
ggplot(carinsurance, aes(x = car_age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Car Age")

# Boxplot of 'cost' by 'homeowner'
ggplot(carinsurance, aes(x = factor(homeowner), y = cost)) +
  geom_boxplot(fill = "green", color = "black") +
  labs(title = "Cost by Homeowner Status") +
  xlab("Homeowner") +
  ylab("Cost")

# Scatter plot of 'car_age' vs. 'cost'
ggplot(carinsurance, aes(x = car_age, y = cost)) +
  geom_point(aes(color = factor(homeowner), shape = factor(married_couple))) +
  labs(title = "Car Age vs. Cost") +
  xlab("Car Age") +
  ylab("Cost")

#BAR CHART FOR HOMEOWNER DISTRIBUTION
ggplot(carinsurance, aes(x = factor(homeowner))) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Distribution of Homeowner Status") +
  xlab("Homeowner") +
  ylab("Count")

#MULTIPLE LINEAR REGRESSION
# Load necessary libraries
library(caret)

# Create a data frame with the "cost" variable and all other variables as predictors
data_for_mlr <- carinsurance

# Build the MLR model
mlr_model <- lm(cost ~ ., data = data_for_mlr)

# Summarize the MLR model
summary(mlr_model)


#PRINCIPAL COMPONENT ANALYSIS
numerical_variables <- carinsurance[sapply(carinsurance, is.numeric)]
#checking for missing values
missing_values <- sapply(numerical_variables, function(x) sum(is.na(x)))
print("Missing Values:")
print(missing_values)
#non numeric variables 
non_numerical_variables <- carinsurance[!sapply(carinsurance, is.numeric)]
head(numerical_variables)

# Define a vector of column names you want to retrieve
selected_columns <- c("car_age", "age_oldest", "risk_factor")

# Subset the numerical_variables data frame using the selected columns
selected_numerical_variables <- numerical_variables[selected_columns]

# Convert the selected columns to numerical
for (col in selected_columns) {
  selected_numerical_variables[[col]] <- as.numeric(selected_numerical_variables[[col]])
}
#normalizing the data
data_normalized <- scale(selected_numerical_variables)

#corrplot 
library(ggcorrplot)
corr_nummatrix <- cor(data_normalized)
ggcorrplot(corr_nummatrix)
#applying pca function 
data.pca <- princomp(corr_nummatrix)
summary(data.pca)

data.pca$loadings[, 1:3]
#SCREE PLOT 
library(factoextra)
fviz_eig(data.pca, addlabels = TRUE)

# the first two This plot shows the eigenvalues in a downward curve, 
#from highest to lowest. 
#The first two components can be considered to be the most significant 
#since they contain almost 90% of the total information of the data.


# Assuming you have already performed PCA and stored the principal components in data.pca
# and you have a data frame with "cost" and the principal components

#MLR OF PCA
# Create a data frame with cost and principal components
data_for_regression <- data.frame(cost = carinsurance$cost, PC1 = data.pca$scores[, 1], PC2 = data.pca$scores[, 2], PC3 = data.pca$scores[, 3])

# Fit a Multiple Linear Regression model
mlr_model <- lm(cost ~ PC1 + PC2 + PC3 , data = data_for_regression)

# Print a summary of the regression model
summary(mlr_model)

#KNN

# Load the required package
library(class)



# Select the variables for KNN and create a data frame
knn_data1 <- carinsurance[, c("group_size", "car_age", "risk_factor", "cost")]

# Split the data into training and testing sets (e.g., 70% training and 30% testing)
set.seed(123)  # for reproducibility
sample_size1 <- floor(0.7 * nrow(knn_data1))
train_indices1 <- sample(1:nrow(knn_data1), sample_size1)
train_data1 <- knn_data1[train_indices1, ]
test_data1 <- knn_data1[-train_indices1, ]

# Train the KNN model
k <- 5  # Set the number of neighbors (you can experiment with different values)
knn_model1 <- knn(train_data1 , test_data1, train_data1 , k)

# Evaluate the model
predictions1 <- as.numeric(knn_model1)
actual1 <- test_data1[, 4]

# Calculate mean absolute error
mae1 <- mean(abs(predictions1 - actual1))
cat("Mean Absolute Error:", mae1, "\n")

# You can also calculate other evaluation metrics as needed
#####################
library(caTools)
set.seed(255)

split = sample.split(carinsurance$cost, 
                     SplitRatio = 0.75)
train = subset(carinsurance, 
               split == TRUE)
test = subset(carinsurance, 
              split == FALSE)
train_scaled = scale(train[-24])
test_scaled = scale(test[-24])

library(class)
test_pred <- knn(
  train = train_scaled, 
  test = test_scaled,
  cl = carinsurance$cost, 
  k=10
)
actual <- test$cost

cm <- table(actual,test_pred)
cm
###################3
# Load the necessary libraries
library(knn)
install.packages("knn")

# Load the training data

# Split the data into training and test sets
train_index <- sample(1:nrow(carinsurance), nrow(carinsurance) * 0.7)
train_data <- carinsurance[train_index, ]
test_data <- carinsurance[-train_index, ]

# Create the KNN model
knn(train_data$cost, train_data[, c("group_size", "car_age", "risk_factor", "age_oldest")], k = 5, cl = 4)

# Predict the cost for the test data
predicted_cost <- predict(knn_model, test_data[, c("group_size", "car_age", "risk_factor", "age_oldest")])

# Calculate the accuracy of the model
accuracy <- mean(predicted_cost == test_data$cost)

# Print the accuracy
print(accuracy)

# Check the number of rows in the training data
nrow(train_data$cost)

# Check the number of rows in the class labels
nrow(train_data[, c("group_size", "car_age", "risk_factor")])




library(caret)
library(class)
library(ggplot2)
library(ggcorrplot)
library(factoextra)

# Data Preparation
# Selecting variables for KNN
selected_variables <- c("car_age", "group_size", "risk_factor")
knn_data <- carinsurance[selected_variables]

# Handle missing values - using mean imputation
for (col in colnames(knn_data)) {
  if (sum(is.na(knn_data[[col]])) > 0) {
    knn_data[[col]][is.na(knn_data[[col]])] <- mean(knn_data[[col]], na.rm = TRUE)
  }
}

# Ensure the data is numeric
knn_data <- sapply(knn_data, as.numeric)

# Combine the selected variables and the outcome variable
knn_data <- data.frame(knn_data, cost = carinsurance$cost)

# Define the outcome variable (cost)
outcome_var <- knn_data$cost

# Set up the control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Train the KNN model
knn_model <- train(x = knn_data[, -which(names(knn_data) == "cost")], y = outcome_var, method = "knn", trControl = ctrl)
summary(knn_model)