# Name : Tashi Phuntsho
# Registration Number : 19BDS0169.
# Exercise 3: Handling Missing Data
# Date : 29/09/2021
 

# importing the necessary library
library(base)
library(utils)
library(stats)
library(readr)
library(graphics)
library(naniar)
library(dplyr)


# loading the dataset.
df <- read.csv('https://raw.githubusercontent.com/tashiphuntsho-thetashi/Programming-for-DataScience.-/main/missing_value%20dataset%20.csv')
# dataset source https://www.kaggle.com/sonukumari47/disney-movie-dataset and preprocessed in python with reduced number of rows.
head(df)
summary(df)

# visualizing the missing values using 
vis_miss(df)
# from the plot we can see that the last three columns contain 100% missing value,
# hence removing them.
df1 <- df %>% select(1:5)

##renaming the column names of the dataframe 
colnames(df1) <- c("x","title","running_time","budget","box_office")



# imputing the missing values of running_time and budget using mean and median imputation
mean_running_time <- mean(df1$running_time,na.rm = TRUE)
df1$running_time[is.na(df1$running_time)] = mean_running_time
# filling the missing value of running_time columns using the mean of the column
mean_budget <- mean(df1$budget,na.rm = TRUE)
df1$budget[is.na(df1$budget)] <- mean_budget # filling the missing value of budget with the median of
#budget column
vis_miss(df1)




# Predicting the missing values of box_office column using any multiple linear regression.
# In here dependent variable is the box_office and independent variables are running
# time and budget , multiple linear regression is chosen because the value we are predicting 
# is a continuous varaible
box_office_temp <- df1 %>% filter(!is.na(df1$box_office))
print("Training data: ")
print(head(box_office_temp))
# building a regression model for prediction.
reg_model <- lm(box_office ~ budget + running_time,data = box_office_temp)


# The model will learn from the records without missing values(training data), once the parameters are 
# found , the model can be used to predict the value of the missing data of box_office(testing data)
# using running_time and  budget variables.
box_office_missing <- df1 %>% filter(is.na(df1$box_office))
print("Testing data: ")
print(head(box_office_missing))

predicted <- predict(reg_model ,box_office_missing)# predicting the missing values using our regression 
#model.

print("The first 10 predicted values of missing box_office column entries : ")
print(predicted[1:10])
#replacing all the null values of the box_office with predicted.
df1$box_office[is.na(df1$box_office)] <- predicted
print("All the missing values are handled and imputed:")
print(paste("total number of missing values after imputation: ",sum(is.na(df1))))

head(df1)





