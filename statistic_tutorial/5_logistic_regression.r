# logistic regression
#   -> predict categorical data
#   -> answer question with "yes/no" and "case/control"

# the purpost of this file -> we will predict the presence or absence of diabetes in Pima Indian women from Pima.tr from the MASS package

# download the package
pkgs <- c("MASS", "dplyr", "pROC", "caret", "PerformanceAnalytics")
install.packages(pkgs, dependencies = TRUE)
lapply(pkgs, library, character.only = TRUE)
# Load the libraries we will need for today's session
library(MASS) # For the Pima.tr dataset
library(dplyr) # For data manipulation
library(pROC) # For creating ROC curves
library(caret) # For data splitting and confusion matrix
library(PerformanceAnalytics) # For correlation matrix

# 1. association analysis: Chi-squared Test
# use for categorical data, find association btw 2 variables
# we want to know that Is there a statistically significant association between being overweight and having diabetes in this dataset? before building the complex model
# chi-square just tell us is there a association or not, it do not tell us "strength" and "direction" like correlation.
# TODO: we need to convert continuous bmi variable in to categorical one -> we define 'High MBI' if bmi > 30

# Note: 
# correlation -> continuous data --> find association for variable
# chi-square -> categorical data -|

# load the dataset
data(Pima.tr)
str(Pima.tr)

# 'data.frame':   200 obs. of  8 variables:
#  $ npreg: int  5 7 5 0 0 5 3 1 3 2 ...
#  $ glu  : int  86 195 77 165 107 97 83 193 142 128 ...
#  $ bp   : int  68 70 82 76 60 76 58 50 80 78 ...
#  $ skin : int  28 33 41 43 25 27 31 16 15 37 ...
#  $ bmi  : num  30.2 25.1 35.8 47.9 26.4 35.6 34.3 25.9 32.4 43.3 ...
#  $ ped  : num  0.364 0.163 0.156 0.259 0.133 ...
#  $ age  : int  24 55 35 26 23 52 25 24 63 31 ...
#  $ type : Factor w/ 2 levels "No","Yes": 1 2 1 1 1 2 1 1 1 2 ...

# npreg: number of pregnancies.
# glu: plasma glucose concentration in an oral glucose tolerance test.
# bp: diastolic blood pressure (mm Hg).
# skin: triceps skin fold thickness (mm).
# bmi: body mass index (weight in kg/(height in m)).
# ped: diabetes pedigree function.
# age: age in years.
# type: Yes or No, for diabetic according to WHO criteria.

# First but the most important thing, we set 'No diabetic' subjects as our reference group
pima_data <- Pima.tr %>%
    mutate(type = factor(type, levels = c("No", "Yes"))) # change type from 1,2 to "No"/"Yes"

# Create a new categorical variable for BMI
# The ifelse() function is perfect for this
pima_data$bmi_cat <- ifelse(pima_data$bmi > 30, "High", "Normal")

# Create a contingency table to see the counts
contingency_table <- table(pima_data$bmi_cat, pima_data$type)
print(contingency_table)

#         No Yes
#   High   74  58
#   Normal 58  10

# Perform the Chi-squared test
# H0: There is no association between bmi_cat and type.
# H1: There is an association between bmi_cat and type.
chi_result <- chisq.test(contingency_table)
print(chi_result)

#         Pearson's Chi-squared test with Yates' continuity correction

# data:  contingency_table
# X-squared = 15.814, df = 1, p-value = 6.988e-05

# Note: watch only p-value -> p-value < 0.05 -> reject null hypothesis -> There is a highly statistically significant association between having a high BMI and having diabetes in this dataset.

# The contingency table shows that the proportion of "Yes" cases is much higher in the "High" BMI group.

#####################################################################################

# 2. Building a predictive model : Logistic regression
# build a model for predicting probability of having diabetes
