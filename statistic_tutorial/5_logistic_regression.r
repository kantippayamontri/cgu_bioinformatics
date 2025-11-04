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

# step A: Split data into Train(75%) and Test(25%) set

# Set a seed for reproducibility, so we all get the same split
# If you don't do this, you and I will get different results.
set.seed(123)

# The createDataPartition function from `caret` is great for this.
# It performs a "stratified" split, meaning the proportion of 'Yes' and 'No'
# in the 'type' column will be the same in both the train and test sets.
train_indices <- createDataPartition(pima_data$type, p = 0.75, list = FALSE)
print(train_indices)

# Create the training dataset
train_data <- pima_data[train_indices, ]

# Create the testing dataset
test_data <- pima_data[-train_indices, ]

# Check the dimensions of our new datasets
table(train_data$type) # 34% are diabetic
#  No Yes
#  99  51

table(test_data$type) # 34% are diabetic
#  No Yes
#  33  17

# 3. Train logistic regression model

# Let's start from the simple logistic regression model
# The family="binomial" argument tells glm() to perform logistic regression.
summary(glm(type ~ npreg, data = train_data, family = "binomial")) # family = "binomial" ==> logistic regression
# Call:
# glm(formula = type ~ npreg, family = "binomial", data = train_data)

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept) -1.27465    0.27018  -4.718 2.38e-06 ***
# npreg        0.16442    0.05258   3.127  0.00177 **
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# (Dispersion parameter for binomial family taken to be 1)

#     Null deviance: 192.31  on 149  degrees of freedom
# Residual deviance: 181.96  on 148  degrees of freedom
# AIC: 185.96

# Number of Fisher Scoring iterations: 4

summary(glm(type ~ glu, data = train_data, family = "binomial"))

##
## Call:
## glm(formula = type ~ glu, family = "binomial", data = train_data)
##
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)
## (Intercept) -5.111147   0.938038  -5.449 5.07e-08 ***
## glu          0.034865   0.007071   4.931 8.19e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## (Dispersion parameter for binomial family taken to be 1)
##
##     Null deviance: 192.31  on 149  degrees of freedom
## Residual deviance: 160.95  on 148  degrees of freedom
## AIC: 164.95
##
## Number of Fisher Scoring iterations: 4

summary(glm(type ~ bp, data = train_data, family = "binomial"))

# Call:
# glm(formula = type ~ bp, family = "binomial", data = train_data)

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept) -3.75344    1.22848  -3.055  0.00225 **
# bp           0.04252    0.01656   2.568  0.01024 *
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# (Dispersion parameter for binomial family taken to be 1)

#     Null deviance: 192.31  on 149  degrees of freedom
# Residual deviance: 185.17  on 148  degrees of freedom
# AIC: 189.17

# Number of Fisher Scoring iterations: 4

summary(glm(type ~ skin, data = train_data, family = "binomial"))

# Call:
# glm(formula = type ~ skin, family = "binomial", data = train_data)

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept) -1.81737    0.50578  -3.593 0.000327 ***
# skin         0.03925    0.01590   2.469 0.013538 *
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# (Dispersion parameter for binomial family taken to be 1)

#     Null deviance: 192.31  on 149  degrees of freedom
# Residual deviance: 185.42  on 148  degrees of freedom
# AIC: 189.42

# Number of Fisher Scoring iterations: 4

summary(glm(type ~ bmi, data = train_data, family = "binomial"))

##
## Call:
## glm(formula = type ~ bmi, family = "binomial", data = train_data)
##
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept) -3.54663    0.99272  -3.573 0.000353 ***
## bmi          0.08904    0.02974   2.994 0.002754 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## (Dispersion parameter for binomial family taken to be 1)
##
##     Null deviance: 192.31  on 149  degrees of freedom
## Residual deviance: 182.55  on 148  degrees of freedom
## AIC: 186.55
##
## Number of Fisher Scoring iterations: 4

summary(glm(type ~ ped, data = train_data, family = "binomial"))

##
## Call:
## glm(formula = type ~ ped, family = "binomial", data = train_data)
##
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)  -1.2867     0.3262  -3.945 7.98e-05 ***
## ped           1.3004     0.5667   2.295   0.0217 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## (Dispersion parameter for binomial family taken to be 1)
##
##     Null deviance: 192.31  on 149  degrees of freedom
## Residual deviance: 186.53  on 148  degrees of freedom
## AIC: 190.53
##
## Number of Fisher Scoring iterations: 4

summary(glm(type ~ age, data = train_data, family = "binomial"))

# Call:
# glm(formula = type ~ age, family = "binomial", data = train_data)

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept) -3.09362    0.59670  -5.185 2.17e-07 ***
# age          0.07214    0.01664   4.336 1.45e-05 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# (Dispersion parameter for binomial family taken to be 1)

#     Null deviance: 192.31  on 149  degrees of freedom
# Residual deviance: 170.60  on 148  degrees of freedom
# AIC: 174.6

# Number of Fisher Scoring iterations: 4

### Summary:
# npreg, p = 0.00177, AIC: 185.96
# glu, p = 8.19e-07, AIC: 164.95
# bp, p = 0.01024, AIC: 189.17
# skin, p = 0.013538, AIC: 189.42
# bmi, p = 0.002754, AIC: 186.55
# ped, p = 0.0217, AIC: 190.53
# age, p = 1.45e-05, AIC: 174.6
# The AIC is a statistical measure that helps us judge the quality of a model relative to other models. It provides a simple way to handle the fundamental trade-off between model complexity and goodness of fit.

# AIC -> tell the quality of model -> lower is better.

# Think of it as a scoring system where a lower score is better.

chart.Correlation(train_data[, -c(8:9)], histogram = TRUE, pch = 19)

# Note that many predictors are highly correlated with each other

# Build a full logistic regression model
# The family="binomial" argument tells glm() to perform logistic regression.
full_model <- glm(type ~ . - bmi_cat, data = train_data, family = "binomial") # We exclude our created bmi_cat

# We can check the multicollinearity problem
install.packages("car")
library(car)

vif(full_model) # check multicolinearity of the model
#    npreg      glu       bp     skin      bmi      ped      age
# 1.415172 1.086578 1.182252 1.896855 1.905138 1.068828 1.564258

# All variables are with vif < 5, suggesting no multicollinearity problem

# Use backward stepwise selection to find the best model
step_model <- step(full_model, direction = "backward", trace = 0)

# Print the summary of the final, selected model
summary(step_model)

# Call:
# glm(formula = type ~ npreg + glu + bmi + ped + age, family = "binomial",
#     data = train_data)

# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)
# (Intercept) -8.668044   1.613253  -5.373 7.74e-08 ***
# npreg        0.102159   0.069850   1.463 0.143592
# glu          0.029047   0.007587   3.828 0.000129 ***
# bmi          0.054648   0.035510   1.539 0.123820
# ped          1.584541   0.702416   2.256 0.024080 *
# age          0.040653   0.022180   1.833 0.066815 .
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# (Dispersion parameter for binomial family taken to be 1)

#     Null deviance: 192.31  on 149  degrees of freedom
# Residual deviance: 140.07  on 144  degrees of freedom
# AIC: 152.07

# Number of Fisher Scoring iterations: 5


# Interpretation of the Final Model:
# The stepwise selection kept the variables npreg, glu, bmi, ped, and age as the most significant predictors. For example, the coefficient for glu (blood glucose) is negative, indicating that higher glucose levels are strongly associated with a higher probability of having diabetes, which makes perfect biological sense.

# For the results here, you will see some non-significant variables were included in the final model. This is because the 'step' function selects predictors based on the AIC.
# A non-significant variable in the final model was chosen for the following reasons:
# 1. It's an important control variable
# 2. It contributes just enough predictive power
# 3. It acts as a suppressor variable (supressing irrelevant variance in other predictors)

final_coeffs <- summary(step_model)$coefficients
print(final_coeffs)
# Calculate the Odds Ratios
odds_ratios <- exp(final_coeffs[, "Estimate"])
print(odds_ratios)
# Combine and print a nice table
interpretation_table <- cbind(final_coeffs, "Odds Ratio" = odds_ratios)
print(interpretation_table)

##                Estimate  Std. Error   z value     Pr(>|z|)   Odds Ratio
## (Intercept) -8.66804415 1.613253053 -5.373022 7.742787e-08 0.0001719952
## npreg        0.10215902 0.069850202  1.462544 1.435921e-01 1.1075595827
## glu          0.02904674 0.007587436  3.828268 1.290484e-04 1.0294727079
## bmi          0.05464766 0.035510008  1.538937 1.238197e-01 1.0561684195
## ped          1.58454127 0.702415909  2.255845 2.408035e-02 4.8770536270
## age          0.04065347 0.022179628  1.832920 6.681454e-02 1.0414911384

## Important!!!!
# Interpretation:
# glu: For every 1 mg/dL increases in glucose, the odds of having diabetes increase by 2.9% ((1.029-1)*100).
# ped: for every 1 unit increase in the diabetes pedigree function, the odds of having diabetes are multiplied by 4.88 (or increase by 388%)

# Evaluating the Training Model with an ROC Curve An ROC curve shows how well our model can distinguish between the <U+2018>Yes<U+2019> and <U+2018>No<U+2019> classes. The Area Under the Curve (AUC) is our performance metric: 1.0 is perfect, 0.5 is a random guess.

# Predict probabilities on the TRAINING data
train_probs <- predict(step_model, newdata = train_data, type = "response")

# Create the ROC curve object
roc_curve <- roc(train_data$type, train_probs)

# Setting levels: control = No, case = Yes
# Setting direction: controls < cases

# Plot the ROC curve
plot(roc_curve,
    main = "Corrected ROC Curve (Axes from 0 to 1)",
    col = "darkgreen",
    xlim = c(1, 0),
    ylim = c(0, 1),
    legacy.axes = TRUE,
    print.auc = TRUE
)

# Calculate and print the AUC
auc_value <- auc(roc_curve)
cat("AUC on Training Data:", auc_value, "\n")

# AUC on Training Data: 0.8312537

# Interpretation:
# An AUC of approximately 0.83 is quite good. It indicates that our model has a strong ability to discriminate between patients with and without diabetes on the data it was trained on.

# 4. Predicting on the Test Set and Final Evaluation
# Now for the true test. We will use our step_model (which has never seen the test_data) to make predictions and see how well it performs.

# 1) Predict probabilities on the unseen test_data
test_probs <- predict(step_model, newdata = test_data, type = "response")

# 2) Convert probabilities into class predictions ('Yes' or 'No')
# We use a standard cutoff threshold of 0.5. If P(diabetes) > 0.5, predict 'Yes'.

predicted_classes <- ifelse(test_probs > 0.5, "Yes", "No")
# It's important to make this a factor with the same levels as the original data
predicted_classes <- factor(predicted_classes, levels = levels(test_data$type))

# 3. Create a Confusion Matrix
# This table compares our model's predictions to the actual truth.
# We will use the powerful confusionMatrix() function from `caret`
final_evaluation <- confusionMatrix(data = predicted_classes, reference = test_data$type)

print(final_evaluation)

## Confusion Matrix and Statistics
## 
##           Reference
## Prediction No Yes
##        No  29   6
##        Yes  4  11
##                                           
##                Accuracy : 0.8             
##                  95% CI : (0.6628, 0.8997)
##     No Information Rate : 0.66            
##     P-Value [Acc > NIR] : 0.02272         
##                                           
##                   Kappa : 0.5413          
##                                           
##  Mcnemar's Test P-Value : 0.75183         
##                                           
##             Sensitivity : 0.8788          
##             Specificity : 0.6471          
##          Pos Pred Value : 0.8286          
##          Neg Pred Value : 0.7333          
##              Prevalence : 0.6600          
##          Detection Rate : 0.5800          
##    Detection Prevalence : 0.7000          
##       Balanced Accuracy : 0.7629          
##                                           
##        'Positive' Class : No              
## 

