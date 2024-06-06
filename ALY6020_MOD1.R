library(dplyr)
library(pander)
library(knitr)
library(corrplot)
library(dplyr)
library(ggplot2)
library(tidyr)
library(kableExtra)





census1 = read.csv("C:\\Users\\sreej\\OneDrive\\Documents\\Northeastern - Analytics\\ALY 6020 Predictive Analysis\\census.csv", header=TRUE, stringsAsFactors=FALSE)
census1
#DATA CLEANING 

# Variable names as per the description
variable_names <- c("age", "workclass", "fnlwgt", "education", "education-num",
                    "marital-status", "occupation", "relationship", "race", "sex",
                    "capital-gain", "capital-loss", "hours-per-week", "native-country", "salary")

# Assign variable names to the dataset
colnames(census1) <- variable_names

# Check the updated dataset
head(census1)

# Display summary statistics for each variable
summary(census1)

# Check for missing values
missing_values <- colSums(is.na(census1))
print("Missing Values:")
print(missing_values)

# Check for duplicated rows
duplicated_rows <- census1[duplicated(census1), ]
print("Duplicated Rows:")
print(duplicated_rows)

#deleting the unecessary variable 
census1 <- census1[, !colnames(census1) %in% c("fnlwgt")]

# Check the updated dataset
head(census1)

# Assuming 'census1' is your dataset
census1 <- census1[!apply(census1 == "?", 1, any), ]

# Check the updated dataset
head(census1)

# Install and load the summarytools package if not already installed
if (!requireNamespace("summarytools", quietly = TRUE)) {
  install.packages("summarytools")
}
library(summarytools)


# Generate summary statistics table
summary_table <- descr(census1)

# Print the summary table
print(summary_table)


# Plot for 'age' (Numerical Variable)
plot_age <- census1 %>%
  ggplot(aes(x = age)) +
  theme_minimal() +
  geom_histogram(bins = 35, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Age",
       x = "Age", 
       y = "Frequency")

# Print the plot
print(plot_age)

# Plot for 'age' by Gender
plot_age_gender <- census1 %>%
  ggplot(aes(x = age, fill = sex)) +
  geom_histogram(bins = 35, position = "identity", alpha = 0.7) +
  labs(title = "Distribution of Age by Gender",
       x = "Age",
       y = "Frequency",
       fill = "Gender") +
  theme_minimal()

# Print the plot
print(plot_age_gender)

# Bar plot for 'education' levels
plot_education <- census1 %>%
  ggplot(aes(x = education, fill = education)) +
  geom_bar() +
  labs(title = "Distribution of Education Levels",
       x = "Education Level",
       y = "Frequency",
       fill = "Education Level") +
  theme_minimal()
print(plot_education)

# Boxplot for 'hours-per-week' by 'salary'
plot_boxplot <- census1 %>%
  ggplot(aes(x = salary, y = `hours-per-week`, fill = salary)) +
  geom_boxplot() +
  labs(title = "Boxplot of Hours per Week by Salary",
       x = "Salary",
       y = "Hours per Week",
       fill = "Salary") +
  theme_minimal()

# Print the plot
print(plot_boxplot)

# THREE VARIABLES 
library(ggplot2)

# Boxplot for Age vs. Income
plot_age_income <- ggplot(census1, aes(x = salary, y = age, fill = salary)) +
  geom_boxplot() +
  labs(title = "Age vs. Salary",
       x = "Salary",
       y = "Age",
       fill = "Salary") +
  theme_minimal()

# Print the plot
print(plot_age_income)

# Boxplot for Education-Num vs. Income
plot_education_income <- ggplot(census1, aes(x = salary, y = `education-num`, fill = salary)) +
  geom_boxplot() +
  labs(title = "Years of Education vs. Income",
       x = "Salary",
       y = "Years",
       fill = "Salary") +
  theme_minimal()

# Print the plot
print(plot_education_income)

# Boxplot for Hours-per-Week vs. Income
plot_hours_income <- ggplot(census1, aes(x = salary, y = `hours-per-week`, fill = salary)) +
  geom_boxplot() +
  labs(title = "Hours-per-Week vs. Income",
       x = "Salary",
       y = "Hours per Week",
       fill = "Salary") +
  theme_minimal()

# Print the plot
print(plot_hours_income)


############
# K Value Selection for KNN Models
k_values <- c(5, 15, 30)

features <- census1[, c("age", "education-num", "hours-per-week")]  
target <- as.factor(census1$salary)  

# Function to build KNN model and generate confusion matrix
build_knn_model <- function(k) {
  knn_model <- knn(train = features, test = features, cl = target, k = k)
  confusion_matrix <- table(knn_model, target)
  return(confusion_matrix)
}

# Build KNN models for different K values
k_values <- c(5, 15, 30)
confusion_matrices <- lapply(k_values, build_knn_model)

# Display confusion matrices
for (i in seq_along(k_values)) {
  cat(paste("Confusion Matrix for K =", k_values[i], ":\n"))
  print(confusion_matrices[[i]])
  cat("\n")
}

#COMPARING THE MATRICES
confusion_matrix_metrics <- function(conf_matrix) {
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  f1_score <- 2 * (precision * recall) / (precision + recall)
  return(c(Accuracy = accuracy, Precision = precision, Recall = recall, F1_Score = f1_score))
}

# Evaluate metrics for each model
metrics <- lapply(confusion_matrices, confusion_matrix_metrics)

# Display metrics
for (i in seq_along(k_values)) {
  cat(paste("Metrics for K =", k_values[i], ":\n"))
  print(metrics[[i]])
  cat("\n")
}



