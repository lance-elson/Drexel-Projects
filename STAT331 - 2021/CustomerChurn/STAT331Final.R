#------------------------------------------
## STAT 331 Final Project
## Group 13 - Lance Elson
## Case #3 - Predicting customer churn
#------------------------------------------
#------------------------------------------
######### Preliminary Code #########
#------------------------------------------
## Clear your workspace
# If your workspace is NOT empty, run:
rm(list=ls())
# to clear it
#------------------------------------------
## Set wd
setwd("~/Desktop/STAT331/Final")
#------------------------------------------
## Load libraries
library(DescTools)
library(caret)
library(e1071)
library(randomForest)

## Load Data
CCdata <- read.csv(file = "CustomerChurn.csv",
                   stringsAsFactors = FALSE)

#------------------------------------------
## Data Overview

# The CustomerChurn.sv file contains data
# for 2114 customers representing metrics
# that have hypothetical correlations to
# whether or not a telecommunications
# company's customer churns. Using
# demographic, historical, and contract
# data, we can observe relationships
# between metrics and customer churns,
# and predict customer churn. Considering
# the cost of losing loyal customers, we
# want  to prioritize minimizing the number
# of cases  where we preduct a customer
# stays (Churn = No) but actually leaves
# (Churn = Yes)

# Variables include:
# customerID: unique customer identifier
# gender: customer gender (Male, Female)
# SeniorCitizen: indicates if a customer
#   is a senior citizen (1) or not (0)
# Partner: Indicates if the customer has
#   a partner (Yes) or not (No)
# Dependents: Indicates if the customer
#   has dependents (Yes) or not (No)
# tenure: the number of months that the
#   customer has been a customer
# PhoneService: Indicates if the customer
#   has phone service with the company (Yes)
#   or not (No)
# InternetService: Indicates the type of
#   internet service the customer has with
#   the company (Fiber optic, DSL, No) - No
#   for none
# Contract: The type of contract that the
#   customer has with the company
#   (Month-to-month, One year, Two year)
# PaperlessBilling: If the customer is
#   enrolled in paperless billing (Yes) or
#   not (No)
# PaymentMethod: The most recent payment
#   method used by the customer to pay the
#   company (Electronic check, Mailed check,
#   Bank transfer (automatic), or
#   Credit card (automatic)
# MonthlyCharges: The most recent amount that
#   the customer is charged per month
# TotalCharges: The total amount that the
#   customer has been charged
# Churn: Whether the customer has left the
#   company (Yes) or not (No)

#------------------------------------------
######### Data Preparation #########
#------------------------------------------
## I. Data Exploration & Preparation

# High-level data view
Abstract(CCdata)

# 2 NAs in TotalCharges for new customers
# (tenure = 0) Filling NA with
# MonthlyCharges to match TotalCharges for
# other new customers
CCdata$TotalCharges <- ifelse(is.na(CCdata$TotalCharges),
                              CCdata$MonthlyCharges,
                              CCdata$TotalCharges)

# Descriptive statistic information
Desc(CCdata, plotit = FALSE)


## II. Preparing Variables

## Prepare Target (Y) Variable
# Churn = unordered factor, 2 levels (Yes / No)
CCdata$Churn <- factor(x = CCdata$Churn)

# Nominal Variables
# Convenience vector of nominal variable names
nomVars <- c("gender", "SeniorCitizen", "Partner",
             "Dependents", "PhoneService",
             "InternetService", "PaperlessBilling",
             "PaymentMethod")

# Convert variables to unordered factors
CCdata[ ,nomVars] <- lapply(X = CCdata[ ,nomVars],
                            FUN = factor)

# Ordinal Variables
# Convenience vector of ordinal variable names
ordVars <- c("Contract")
# Convert variables to ordered factors
CCdata$Contract <- factor(x = CCdata$Contract, levels =
                      c("Month-to-month", "One year",
                      "Two year"), ordered = TRUE)

# Numeric Variables
numVars <- c("MonthlyCharges","TotalCharges","tenure")

# Predictor Variables
indepVars <- c(nomVars, ordVars, numVars)
indepVars

# Checking high-level information
# for our prepared data
Abstract(CCdata)

# Summary statistic information
# for our prepared data
summary(CCdata)

#------------------------------------------
######### Data Partitioning #########
#------------------------------------------
# Initialize the random seed
set.seed(1)

# Generate the list of observations for the
# training dataframe
dataSub <- createDataPartition(y = CCdata$Churn,
                                  p = 0.80,
                                  list = FALSE)

# Create training dataframe
dataTrain <- CCdata[dataSub, -1]
#   -1 in column section to remove customerID

# Create testing dataframe
dataTest<- CCdata[-dataSub, -1]
#   -1 in column section to remove customerID

#------------------------------------------
#------------------------------------------
######### Naive Bayes Modeling #########
#------------------------------------------
## I. Checking Naive Bayes Assumptions

# Checking for missing values
PlotMiss(x = CCdata)
#   No missing values

# Checking for redundant variables
#   We need to identify highly correlated 
#   numeric input (X) variables and exclude 
#   them from our predictive model.
NB_corrVars <- cor(x = CCdata[ ,numVars])
NB_corrVars

symnum(x = NB_corrVars, corr = TRUE)

NB_highCorrs <- findCorrelation(x = NB_corrVars,
                              cutoff = .75,
                              names = TRUE)

NB_highCorrs

NB_indepVars <- indepVars[!indepVars %in% NB_highCorrs]

NB_indepVars

# Checking for normally distributed
# numercial variables
# Boxcox transformation
NB_numsProcessed <- preProcess(x = CCdata[ ,NB_indepVars],
                      method = c("BoxCox", "center", "scale"))
NB_numsProcessed <- predict(object = NB_numsProcessed,
                  newdata = CCdata)

# Checking new distribution
par(mfrow=c(1,2))
hist(CCdata$MonthlyCharges, main="Histogram of Monthly Charges", xlab= "Monthly Charges in $")
hist(NB_numsProcessed$MonthlyCharges, main="Histogram of Box-Cox Monthly Charges", xlab= "Monthly Charges Standardized")
hist(CCdata$tenure, main="Histogram of tenure", xlab= "Tenure in months")
hist(NB_numsProcessed$tenure, main="Histogram of Box-Cox tenure", xlab= "Tenure Standardized")
# Still not very normal, but we will try...
par(mfrow=c(1,1))

#------------------------------------------
## II. Training & Testing
# Initialize the random seed
set.seed(1)

# Generate the list of observations for the
# training dataframe
NB_dataSub <- createDataPartition(y = NB_numsProcessed$Churn,
                                      p = 0.80,
                                      list = FALSE)

# Create training dataframe
NB_dataTrain <- NB_numsProcessed[NB_dataSub, -1]
#   -1 in column section to remove customerID

# Create testing dataframe
NB_dataTest<- NB_numsProcessed[-NB_dataSub, -1]
#   -1 in column section to remove customerID

#------------------------------------------
## III. Naive Bayes Model
# Looking for zero probability categories in
# case we need laplace smoothing
aggregate(NB_dataTrain[ ,c(nomVars,ordVars)],
          by = list(NB_dataTrain$Churn),
          FUN = table)
# Since we have categories with 0 frequency
# we will set laplace = 0

# Train the model
NB_model <- naiveBayes(x = NB_dataTrain[ ,NB_indepVars],
                     y = NB_dataTrain$Churn,
                     laplace = 0)


## Model Performance & Fit 

# View the model output
NB_model


# obtain the class predictions for the
# training data based on our NB model. 
NB_classPred <- predict(object = NB_model, # NB model
                    newdata = NB_dataTrain[ ,NB_indepVars], # predictors
                    type = "class")

# Obtain performance measures for our model
# applied to the training dataset (NB_dataTrain)
# via confusion matrix
NB_trainConf <- confusionMatrix(data = NB_classPred, # predictions
                              reference = NB_dataTrain$Churn, # actual
                              positive = "Yes",
                              mode = "everything")
NB_trainConf

## Testing Performance

# Obtain the class predictions for the
# testing data
NB_testPred <- predict(object = NB_model, # NB model
                   newdata = NB_dataTest[ ,NB_indepVars], # predictors
                   type = "class")

# Obtain performance measures for our test
# model 
NB_testConf <- confusionMatrix(data = NB_testPred, 
                             reference = NB_dataTest$Churn, 
                             positive = "Yes",
                             mode = "everything")
NB_testConf


# Class-level performance
NB_testConf$byClass


## Model Goodness of Fit
# Overall
cbind(Training = NB_trainConf$overall,
      Testing = NB_testConf$overall)

# Class-Level
cbind(Training = NB_trainConf$byClass,
      Testing = NB_testConf$byClass)

#------------------------------------------
######### Logistic Regression  #########
#------------------------------------------
## I. Checking Logistic Regression
##    Assumptions

# Ia. Checking for missing values
PlotMiss(x = CCdata)
#   No missing values

#------------------------------------------

## II. Logistic Regression Model

LogReg_model <- glm(formula = Churn ~ ., 
              family = binomial,
              data = dataTrain)

summary(LogReg_model)

# Log Odds
coef(LogReg_model)
#   For positive values, higher value =
#   increased churn chance for that variable
#   ie. holding all other vars constant, +1
#   var x = decrease in log odds

# Odds
exp(coef(LogReg_model))
# For a one-unit increase in var x, the odds
# of Churn = Yes increase by a factor of odds

#------------------------------------------
## IV. Model Performance & Fit

# IVa. Training Performance

# Vector of fitted values from
# the model, which are the predicted
# probabilities that Churn = Yes
head(fitted(LogReg_model))

# Cutoff point is 0.5. Assigning the
# probabilities to a class (Yes or No)
# for the target variable (Churn)
LogReg_trainPred <- ifelse (
        test = fitted(LogReg_model) >= 0.5, # is the prediction >= .5?
        yes = "Yes", # if yes, predict "Yes"
        no = "No") # if no, predict "No"

# Converting the vector of class predictions
# to a factor with the same class levels as Churn
LogReg_trainPred <- factor(LogReg_trainPred,
                     levels = c("No", "Yes"))

# Confusion matrix and  applied to the
# training dataset
LogReg_trainConf <- confusionMatrix(data = LogReg_trainPred, # predictions
                              reference = dataTrain$Churn, # actual
                              positive = "Yes", # class we are interested in predicting
                              mode = "everything")
LogReg_trainConf

# IVb. Testing Performance

# Predicted probabilities
# (type = "response") for the testing
# data based on our regression model. 
LogReg_modelTest <- predict(object = LogReg_model, # LR model
                   newdata = dataTest, # testing data
                   type = "response")

# Cutoff point of 0.5 to assign the
# probabilities to a class  (Yes or No)
# for our target variable (Churn)
LogReg_testPred <- factor(ifelse(test = LogReg_modelTest >= 0.5,
                           yes = "Yes",
                           no = "No"),
                           levels = c("No", "Yes"))

# Confusion matrix applied to the
# testing dataset
LogReg_testConf <- confusionMatrix(data = LogReg_testPred, # predictions
                             reference = dataTest$Churn, #actual
                             positive = "Yes",
                             mode = "everything")
LogReg_testConf


# Class-level performance

## Goodness of Fit

# Overall
cbind(Training = LogReg_trainConf$overall,
      Testing = LogReg_testConf$overall)
# balanced model, good corr b/w training and testing

# Class-Level
cbind(Training = LogReg_trainConf$byClass,
      Testing = LogReg_testConf$byClass)
# model performance not degraded by including non-significant predictors
# when purpose is inference, it would be more important

LogReg_testConf$byClass

#------------------------------------------
######### Random Forest Modeling  #########
#------------------------------------------
# Is more interpretable than bagging


# I. Random Forest Model
# By default m (mtry) is equal to the square 
# root of our number of predictor (X) variables. 
floor(sqrt(ncol(dataTrain)-1))
#   3

# Build default model
RF_model <- randomForest(formula = Churn ~. , # use all other variables to predict Amount
                       data = dataTrain, # training data
                       importance = TRUE, # obtain variable importance 
                       ntree = 500) # number of trees in forest

# We can view basic output from the model
RF_model


# Variable Importance Plot
# We can view the most important variables 
# in the Random Forest model using the
# varImpPlot() function
varImpPlot(x = RF_model, # randomForest object
           main = "Variable Importance Plot") # title

## Training Performance
# We use the predict() function to generate 
# class predictions for our training set
RF_base_trainPreds <- predict(object = RF_model, # RF model
                        type = "class") # class predictions

# We can use the confusionMatrix() function
# from the caret package to obtain a 
# confusion matrix and obtain performance
# measures for our model applied to the
# training dataset (train).
RF_base_trainConf <- confusionMatrix(data = RF_base_trainPreds, # predictions
                                  reference = dataTrain$Churn, # actual
                                  positive = "Yes",
                                  mode = "everything")
RF_base_trainConf


## Testing Performance
# We use the predict() function to generate 
# class predictions for our testing set
RF_base_testPreds <- predict(object = RF_model, # RF model
                          newdata = dataTest, # testing data
                          type = "class")

# We can use the confusionMatrix() function
# from the caret package to obtain a 
# confusion matrix and obtain performance
# measures for our model applied to the
# testing dataset (test).
RF_base_testConf <- confusionMatrix(data = RF_base_testPreds, # predictions
                                 reference = dataTest$Churn, # actual
                                 positive = "Yes",
                                 mode = "everything")
RF_base_testConf


## Goodness of Fit

# To assess if the model is balanced,
# underfitting or overfitting, we compare
# the performance on the training and
# testing. We can use the cbind() function
# to compare side-by-side.

# Overall
cbind(Training = RF_base_trainConf$overall,
      Testing = RF_base_testConf$overall)

# Class-Level
cbind(Training = RF_base_trainConf$byClass,
      Testing = RF_base_testConf$byClass)


## Hyperparameter Tuning

# We will tune the number of variables to 
# randomly sample as potential variables to split on 
# (m, the mtry argument).

# We use the tuneRF() function in the 
# randomForest package. The output will
# be a plot, where we choose the mtry with
# the smallest OOB Error. By setting
# doBest = TRUE, the best mtry will
# be used to automatically create
# an RF model

set.seed(1) # initialize random seed

RF_modelTuned <- tuneRF(x = dataTrain[,-13],
                y = dataTrain$Churn, # use Amount as the target variable
                ntreeTry = 500, # 500 trees in the forest
                doBest = TRUE) # use the best m (mtry) value to create an RF model

# View basic model information
RF_modelTuned

# View variable importance for the tuned 
# model
varImpPlot(RF_modelTuned)

## Training Performance
# We use the predict() function to generate 
# class predictions for our training set
RF_tuned_trainPred <- predict(object = RF_modelTuned, # tuned RF model
                          type = "class") # class predictions

# We can use the confusionMatrix() function
# from the caret package to obtain a 
# confusion matrix and obtain performance
# measures for our model applied to the
# training dataset (train).
RF_tuned_trainConf <- confusionMatrix(data = RF_tuned_trainPred, # predictions
                                  reference = dataTrain$Churn, # actual
                                  positive = "Yes",
                                  mode = "everything")
RF_tuned_trainConf


## Testing Performance
# We use the predict() function to generate 
# class predictions for our testing set
RF_tuned_testPred <- predict(object = RF_modelTuned, # tuned RF model
                          newdata = dataTest, # testing data
                          type = "class")

# We can use the confusionMatrix() function
# from the caret package to obtain a 
# confusion matrix and obtain performance
# measures for our model applied to the
# testing dataset (test).
RF_tuned_testConf <- confusionMatrix(data = RF_tuned_testPred, # predictions
                                 reference = dataTest$Churn, # actual
                                 positive = "Yes",
                                 mode = "everything")
RF_tuned_testConf


## Goodness of Fit

# To assess if the model is balanced,
# underfitting or overfitting, we compare
# the performance on the training and
# testing. We can use the cbind() function
# to compare side-by-side.

# Overall
cbind(Training = RF_tuned_trainConf$overall,
      Testing = RF_tuned_testConf$overall)

# Class-Level
cbind(Training = RF_tuned_trainConf$byClass,
      Testing = RF_tuned_testConf$byClass)


#----------------------------------------