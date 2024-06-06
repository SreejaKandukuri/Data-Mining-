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

Titanic1 = read.csv("C:\\Users\\sreej\\OneDrive\\Documents\\Northeastern - Analytics\\ALY 6040 Data Mining Applications\\titanic.csv", header=TRUE, stringsAsFactors=FALSE)

head(Titanic1)       
str(Titanic1)        
summary(Titanic1)    
sum(is.na(Titanic1))

#filling missing age values with mean value 
Titanic1$Age[is.na(Titanic1$Age)] <- mean(Titanic1$Age, na.rm = TRUE)
#deleting missing values in embarked variable 
Titanic1 <- Titanic1[complete.cases(Titanic1[c("Embarked")]), ]

#replacing na values specific to their gender 
# Calculate mean age by gender
meanagebygender <- aggregate(Age ~ Sex, data = Titanic1, FUN = mean, na.rm = TRUE)

# Fill missing 'Age' values based on gender-specific means
Titanic1$Age[is.na(Titanic1$Age) & Titanic1$Sex == "male"] <- meanagebygender[meanagebygender$Sex == "male", "Age"]
Titanic1$Age[is.na(Titanic1$Age) & Titanic1$Sex == "female"] <- meanagebygender[meanagebygender$Sex == "female", "Age"]

# Remove PassengerId and Ticket, Cabin columns
Titanic1 <- Titanic1[, !(names(Titanic1) %in% c("PassengerId", "Ticket" , "Cabin"))]

# Create a new variable 'FamilySize'
Titanic1$TotalFamily <- Titanic1$SibSp + Titanic1$Parch

#round the fare values 
Titanic1$Fare <- round(Titanic1$Fare, 2)
#round the age values 
Titanic1$Age <- round(Titanic1$Age)

# renaming the values of embarked
Titanic1 <- Titanic1 %>%
  mutate(Embarked = factor(Embarked, levels = c("C", "Q", "S"),
                         labels = c("Cherbourg", "Queenstown", "Southampton")))

# Identifying any duplicate rows
Titanic1 <- distinct(Titanic1)


#survived distribution
ggplot(Titanic1, aes(x = factor(Survived))) +
  geom_bar()
#age distribution
ggplot(Titanic1, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Age Distribution", x = "Age")
# class distribution
ggplot(Titanic1, aes(x = factor(Pclass))) +
  geom_bar()
#sex
ggplot(Titanic1, aes(x = Sex)) +
  geom_bar()

# Visualize survival by class
#Question 1: Did people survive based on the class they were traveling in?
library(ggplot2)
ggplot(Titanic1, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(title = "Survival by Class", x = " Passenger Class", y = "Count") +
  scale_fill_discrete(name = "Survived", labels = c("No", "Yes"))
# Visualize survival by gender
#Question 2: Who was given more importance?

# Summary table
table(Titanic1$Sex, Titanic1$Survived)

# Visualize the count of survivors by gender
ggplot(Titanic1, aes(x = Sex, fill = factor(Survived))) +
  geom_bar() +
  labs(title = "Survival Count by Gender", x = "Gender", y = "Count") +
  scale_fill_discrete(name = "Survived", labels = c("No", "Yes"))

#whats the highest and lowest fare ?  
# Highest fare
max_fare <- max(Titanic1$Fare)

# Lowest fare
min_fare <- min(Titanic1$Fare)

cat("Highest Fare: $", max_fare, "\n")
cat("Lowest Fare: $", min_fare, "\n")
#Are people with many family members saved ?
# Visualize survival by family size
ggplot(Titanic1, aes(x = TotalFamily, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  labs(title = "Survival by Family Size", x = "Total Family", y = "Proportion") +
  scale_fill_discrete(name = "Survived", labels = c("No", "Yes"))

# Load the corrplot package
library(corrplot)

# Subset the dataset to include only numeric variables
numeric_data <- Titanic1[, sapply(Titanic1, is.numeric)]

# Compute the correlation matrix for the numeric variables
correlation_matrix <- cor(numeric_data, method = "pearson")

# Create the correlation plot
corrplot(correlation_matrix, method = "color")



#LOGISTIC REGRESSION 

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(Titanic1), 0.7 * nrow(Titanic1))
train_data <- Titanic1[train_indices, ]
test_data <- Titanic1[-train_indices, ]

# Fit a logistic regression model
logistic_model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare , data = train_data, family = "binomial")

# Make predictions on the test set
predictions_logistic <- predict(logistic_model, newdata = test_data, type = "response")

# Evaluate the model
library(pROC)
roc_obj <- roc(test_data$Survived, predictions_logistic)
roc_auc_logistic <- auc(roc_obj) 

####

# Fit a Decision Tree model
library(rpart)
tree_model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train_data, method = "class")

# Make predictions on the test set
predictions_tree <- predict(tree_model, test_data, type = "class")

# Evaluate the model
tree_accuracy <- sum(predictions_tree == test_data$Survived) / length(predictions_tree)





