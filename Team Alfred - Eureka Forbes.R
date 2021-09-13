library(ggplot2)
library(tidyverse)
library(dplyr)
library(caret)
library(reshape2)
library(ROSE)
library(randomForest)
library(rpart)
library(rpart.plot)
# Load data
df_eureka <-
  read.csv(
    "Documents/Study/MMA 2022S/831 - Marketing Analytics/Eureka_Forbes_Analytical_Model/eureka_data_final_2019-01-01_2019-03-01.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )

############################################################################

#Exploratory Data Analysis

############################################################################

head(df_eureka)
str(df_eureka)
summary(df_eureka)

correlation = cor(df_eureka[, unlist(lapply(df_eureka, is.numeric))])
c = correlation[correlation>0.5]

#There are columns that have NAs as the correlation - this is because of the missing values
#This data is MNAR and would need to be handled in a way that there is no bias created for the data that is missing

#Count missing values per feature
lapply(df_eureka, function(x)
  sum(is.na(x)))
#Missingness has a pattern:
#Missing columns are all hist: $bounces_hist, $help_me_buy_evt_count_hist, $pageviews_hist, $paid_hist, $phone_clicks_evt_count_hist, $sessionDuration_hist, $sessions_hist, $visited_air_purifier_page_hist, $visited_checkout_page_hist, $visited_contactus_hist, $visited_customer_service_amc_login_hist, $visited_customer_service_request_login_hist, $visited_demo_page_hist, $visited_offer_page_hist, $visited_security_solutions_page_hist, $visited_storelocator_hist, $visited_vacuum_cleaner_page_hist, $visited_water_purifier_page_hist
#Number of missing values is the same for each column

############################################################################

#Data Wrangling

############################################################################
df_eureka_clean <- data.frame(df_eureka)
cols <-  as.data.frame(sapply(df_eureka_clean, class))
cols
#Remove unwanted columns
df_eureka_clean <- df_eureka_clean[,-c(6,12,30)]

#Change the data into binomial classficiation by changing the values >1 to 1
df_eureka_clean$converted_in_7days <-
  as.numeric(ifelse(df_eureka_clean$converted_in_7days >= 1, 1, 0))

#Splitting the source medium column on '/' and then trimming the left and right spaces
df_eureka_clean$sourceMedium<-sub(".*/", "", df_eureka_clean$sourceMedium)
trimws(df_eureka_clean$sourceMedium,which = "left")

#Validating the column for possible duplicates
table(df_eureka_clean$sourceMedium)

#Remove discrepancies in source medium column
df_eureka_clean$sourceMedium<-ifelse(df_eureka_clean$sourceMedium==" Social"," social",df_eureka_clean$sourceMedium)
df_eureka_clean$sourceMedium<-ifelse(df_eureka_clean$sourceMedium==" (none)","None",df_eureka_clean$sourceMedium)
df_eureka_clean$sourceMedium<-ifelse(df_eureka_clean$sourceMedium==" (not set)","None",df_eureka_clean$sourceMedium)
df_eureka_clean$sourceMedium<-trimws(df_eureka_clean$sourceMedium)

table(df_eureka_clean$sourceMedium)
#We will fill the NAs in the columns with missing data with default values based on their data type and create a copy of the column with 0s and 1s as dummy variables to sustain the model NA effect and not cause bias
handleMissingValues <- function(df_Na) {
  integer_default <- 0
  factor_default <- "HAN_NA"
  character_default <- "HAN_NA"
  date_default <- as.Date("1900-01-01")
  
  for (i in 1:ncol(df_Na)) {
    if (class(df_Na[, i]) %in% c("numeric", "integer")) {
      if (any(is.na(df_Na[, i]))) {
        df_Na[, paste0(colnames(df_Na)[i], "_retention")] <-
          as.factor(ifelse(is.na(df_Na[, i]), "1", "0"))
        df_Na[is.na(df_Na[, i]), i] <- integer_default
      }
    } else
      if (class(df_Na[, i]) %in% c("factor")) {
        if (any(is.na(df_Na[, i]))) {
          df_Na[, i] <- as.character(df_Na[, i])
          df_Na[, paste0(colnames(df_Na)[i], "_retention")] <-
            as.factor(ifelse(is.na(df_Na[, i]), "1", "0"))
          df_Na[is.na(df_Na[, i]), i] <- factor_default
          df_Na[, i] <- as.factor(df_Na[, i])
          
        }
      } else {
        if (class(df_Na[, i]) %in% c("character")) {
          if (any(is.na(df_Na[, i]))) {
            df_Na[, paste0(colnames(df_Na)[i], "_retention")] <-
              as.factor(ifelse(is.na(df_Na[, i]), "1", "0"))
            df_Na[is.na(df_Na[, i]), i] <- character_default
          }
        } else {
          if (class(df_Na[, i]) %in% c("Date")) {
            if (any(is.na(df_Na[, i]))) {
              df_Na[, paste0(colnames(df_Na)[i], "_retention")] <-
                as.factor(ifelse(is.na(df_Na[, i]), "1", "0"))
              df_Na[is.na(df_Na[, i]), i] <- date_default
            }
          }
        }
      }
  }
  return(df_Na)
}

#Clean the NAs using the above funtion
df_eureka_clean <- handleMissingValues(df_eureka_clean)
str(df_eureka_clean)
dim(df_eureka_clean)


############################################################################

#Balancing

############################################################################

#Check the dimentions of the dataframe
dim(df_eureka_clean)
table(df_eureka_clean$converted_in_7days)

#Balance the data using the ROSE library using oversampling. We have kept the N as 1412994 as it is the double of 0s in our dataset and after oversampling, the values of 1s and 0s would become the same
df_clean_eureka_os<-ovun.sample(converted_in_7days~.,data=df_eureka_clean,method="over",N=1000000)$data
table(df_clean_eureka_os$converted_in_7days)
summary(df_clean_eureka_os)

############################################################################

#Modelling - Random Forest and Decision Trees

############################################################################

#Split the dataset into testing and training sets
set.seed(2) #set a random number generation seed to ensure that the split is the same everytime
inTrain <- createDataPartition(y = df_clean_eureka_os$converted_in_7days,
                               p = 0.8, list = FALSE)
training <- df_clean_eureka_os[ inTrain,]
testing <- df_clean_eureka_os[ -inTrain,]

#Run Decision tree model on training dataset
fit <- rpart(converted_in_7days~., data = training, method = 'class')

#Predict using testing dataset & measure
predict_unseen <- predict(fit, testing, type = 'class')
table_mat <- table(testing$converted_in_7days, predict_unseen)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
accuracy_tune(fit)

#Run HyperParameter Tuning - These configs did not work and reduced the accuracy from 0.74367 to 0.73235 - need to change them
control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(converted_in_7days~., data = training, method = 'class', control = control)
predict_unseen_tune <- predict(tune_fit, testing, type = 'class')
table_mat_tune <- table(testing$converted_in_7days, predict_unseen_tune)
accuracy_Test_tune <- sum(diag(table_mat_tune)) / sum(table_mat_tune)
accuracy_Test_tune

#Run Random model on training dataset
fit_random_forest <- randomForest(converted_in_7days~., data = training,ntree=3,nodesize=10)

#Run prediction on the testing dataset
predict_rf= predict(fit_random_forest, newdata=testing)

predict_rf = as.data.frame(predict_rf)
colnames(predict_rf) <- c("converted_in_7day")

#Run measures and create a confusion matrix for the predictions
predict_rf  <- as.matrix(predict_rf )
table_mat_rf <- table(testing$converted_in_7days,predict_rf) 
accuracy_mat_rf <- sum(diag(table_mat_rf))/sum(table_mat_rf)

accuracy_mat_rf

