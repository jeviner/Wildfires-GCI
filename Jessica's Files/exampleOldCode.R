
#Delete Environment-------------------------------------------------------------------------------------
rm(list = ls())
par(mfrow = c(1,1))


#Libraries----------------------------------------------------------------------------------------------
#Data Manipulation
library(dplyr)
library(readxl)
library(MASS)
#Data Visualization 
library(ggplot2)
library(corrplot)
#Other
library(caret)
library(leaps)



#Functions----------------------------------------------------------------------------------------------
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))}

#Importing Data-----------------------------------------------------------------------------------------
movie_data <- read_excel("MGSC310_Midterm1_take-home_datasets.xlsx")
vote_data <- read_excel("MGSC310_Midterm1_take-home_datasets.xlsx", 
                        sheet = "dataset_2")
test_data <-read_excel("MGSC310_Midterm1_data.xlsx",
                       sheet = "testing")


#Data Cleaning------------------------------------------------------------------------------------------
#Assumption: All non specified budgets are in USD,

#turn budget into numeric and remove foreign currency budgets
movie_data$budget_clean <- as.numeric(movie_data$budget)

data_full <- cbind(movie_data, vote_data)
data_types <- sapply(data_full, class)

#Split Datasets
data_full_num <- data_full[, data_types %in% c('numeric','interger')]
data_full_char <- data_full[, !data_types %in% c('numeric', 'interger')]

#Omit NA
data_clean_num <- na.omit(data_full_num)

#Remove Outliers from Budget and Gross USD Income
data_clean_num <- data_clean_num %>% 
  mutate(usa_gross_income_zscore = abs(usa_gross_income - mean(usa_gross_income))/sd(usa_gross_income)) %>%
  mutate(budget_clean_zscore = abs(budget_clean - mean(budget_clean))/sd(budget_clean)) %>%
  filter(usa_gross_income_zscore < 6) %>%
  filter(budget_clean_zscore < 6)

#Create Training and Testing Datasets
#If using normalized data, change data_clean_num to data_clean_norm
split <- 0.8
split <- round(nrow(data_clean_num) * split, 0)
training <- data_clean_num[1:split,]
testing <- data_clean_num[-(1:split),]


#Final Model-------------------------------------------------------------------------------------------
all_in_reg <- lm(data_clean_num$usa_gross_income ~ ., data = data_clean_num)
summary(all_in_reg)

#Variable Selection and Performance Metrics-----------------------------------------------------

#Testing removing 19-56 from training in order to run regsub
#training <- training[,-c(13:50)]

#Regsub to visualize the importance of different variables, can't be too many variables
#regsub <- regsubsets(usa_gross_income ~ ., data = training)
#summary(regsub)
#plot(regsub, scale = "adjr2")


#Performance Metric For the Model

#1000 Simulation Bootstrap Test
set.seed(123)
train.control <- trainControl(method = "boot", number = 1000)
model1 <- train(usa_gross_income ~ ., data = training, method = "lm",
                trControl = train.control)
print(model1)

#K Fold Cross Validation
set.seed(123)
train.control <- trainControl(method = "cv", number = 10)
#train the model
model2 <- train(usa_gross_income ~ ., data = training, method = "lm",
                trControl = train.control)
print(model2)

# Make predictions and compute the R2, RMSE and MAE
predictions <- all_in_reg %>% predict(testing)
data.frame( R2 = R2(predictions, testing$usa_gross_income),
            RMSE = RMSE(predictions, testing$usa_gross_income),
            MAE = MAE(predictions, testing$usa_gross_income))


#Z Score Data Cleaning---------------------------------------------------------------------
#data_clean_num_zscore <- data_clean_num %>% 
#mutate(usa_gross_income_zscore = abs(usa_gross_income - mean(usa_gross_income))/sd(usa_gross_income)) %>%
#mutate(budget_clean_zscore = abs(budget_clean - mean(budget_clean))/sd(budget_clean)) %>%
#filter(usa_gross_income_zscore < 6) %>%
#filter(budget_clean_zscore < 6)

hist(data_clean_num$budget_clean)
