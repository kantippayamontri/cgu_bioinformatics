# Correlation and Linear Regression
# we will working on continuous variables.
# correlation = "as one variable increases, what happens to another?" = find the relationship
# "Linear regression" -> build a model to predict an outcome

# we will working on "swiss" dataset

library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)

head(swiss)
##              Fertility Agriculture Examination Education Catholic
## Courtelary        80.2        17.0          15        12     9.96
## Delemont          83.1        45.1           6         9    84.84
## Franches-Mnt      92.5        39.7           5         5    93.40
## Moutier           85.8        36.5          12         7    33.77
## Neuveville        76.9        43.5          17        15     5.16
## Porrentruy        76.1        35.3           9         7    90.57
##              Infant.Mortality
## Courtelary               22.2
## Delemont                 22.2
## Franches-Mnt             20.2
## Moutier                  20.3
## Neuveville               20.6
## Porrentruy               26.6

# look at the structure of the data

str(swiss)
## 'data.frame':    47 obs. of  6 variables:
##  $ Fertility       : num  80.2 83.1 92.5 85.8 76.9 76.1 83.8 92.4 82.4 82.9 ...
##  $ Agriculture     : num  17 45.1 39.7 36.5 43.5 35.3 70.2 67.8 53.3 45.2 ...
##  $ Examination     : int  15 6 5 12 17 9 16 14 12 16 ...
##  $ Education       : int  12 9 5 7 15 7 7 8 7 13 ...
##  $ Catholic        : num  9.96 84.84 93.4 33.77 5.16 ...
##  $ Infant.Mortality: num  22.2 22.2 20.2 20.3 20.6 26.6 23.6 24.9 21 24.4 ...

# -----------------------------------------------------------------

# Variables in the dataset:

# * Fertility: A standardized fertility measure ("common standardized fertility measure"). This will be our outcome variable.

# * Agriculture: The percentage of males involved in agriculture as an occupation.

# * Examination: The percentage of draftees receiving the highest mark on an army examination. This is a proxy for education level.

# * Education: The percentage of draftees with education beyond primary school. (Note: This is highly correlated with Examination).

# * Catholic: The percentage of the population that is Catholic (as opposed to Protestant).

# * Infant.Mortality: The percentage of live births who live less than 1 year.

# -------------------------------------------------------------

# Our Research Questions: Our primary goal is to understand and predict the Fertility rate based on the other socioeconomic indicators. We can form several hypotheses:

# *Agriculture (positive correlation): More Argiculture -> good, Hypothesis: Agriculture will be positively correlated with Fertility.

# *Education & Examination(negatice correlation) : More educated people -> delay childbirth., Hypothesis: Examination and Education will be negatively correlated with Fertility.

# *Catholicism (positive correlation): Catholic doctrine traditionally opposes contraception. Hypothesis: A higher percentage of Catholic residents will be positively correlated with Fertility.

# *Infant Mortality(positive correlation): High rate (infant will died more) -> family will have more baby, Hypothesis: Infant.Mortality will be positively correlated with Fertility.

# ----------------------------------------------------------------

# 2. Exploring Relationships with Correlation
# What is Correlation? Correlation measures the strength and direction of a linear relationship between two continuous variables.
# The correlation coefficient (Pearson<U+2019>s r) ranges from -1 to +1:
# +1: Perfect positive linear relationship.
# 0: No linear relationship.
# -1: Perfect negative linear relationship.
# A correlation test assesses the null hypothesis that there is no correlation (r = 0).

cor.test(swiss$Fertility, swiss$Agriculture)

#         Pearson's product-moment correlation

# data:  swiss$Fertility and swiss$Agriculture
# t = 2.5316, df = 45, p-value = 0.01492
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.07334947 0.58130587
# sample estimates:
#       cor
# 0.3530792

# correlation coefficient= 0.3530791 and p-value = 0.01492
# The result suggests there is a positive correlation between Fertility and Agriculture
# This indicates more agricultural provinces are more traditional and thus have higher fertility, which fits our expectation

# Let's check the correlation
library(ggplot2)
ggplot(data = swiss, aes(x = Agriculture, y = Fertility)) +
    geom_point(color = "steelblue", size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink")

# We can check the correlation matrix
library(PerformanceAnalytics)
chart.Correlation(swiss, histogram = TRUE, pch = 19)

# 3. Building a Predictive Model: Linear Regression
# What is Linear Regression? While correlation tells us about the relationship, linear regression goes a step further. It finds the best-fitting straight line through the data that allows us to predict an outcome (Y) from one or more predictors (X).

# The model takes the form: Y = Intercept + beta1*X1 + beta2*X2 + ... + error

# Is Fertility Suitable for Linear Regression? A key assumption of linear regression is that the residuals (the errors or differences between the model<U+2019>s predictions and the actual data) are normally distributed and have constant variance. We can check this by fitting a model and plotting its residuals.

# Fit a preliminary full model just to get the residuals
prelim_model <- lm(Fertility ~ ., data = swiss) # create a linear model(lm) = linear regression
# Fertility ~ . = we want to predict Fertility from all other variables in the dataset
# Fertility is the outcome variable
# ~ means "is modeled as " or "is predicted by"
# . means is shorthand that means "all other variables in the dataset"
# the linear equation is Y = intercept + ("Agriculture")X1 + (Examination)X2 + (Education)X3 + (Catholic)X4 + (Infant.Mortality)X5 + error

# Set up a 2x2 plotting area
par(mfrow = c(2, 2))

# Plot the diagnostic plots for the model
plot(prelim_model)
# how to diagnose the plots
# 1. Residuals vs Fitted (Top Left):
# show residual(y-y_hat) against fitted(predicted)
# Good: point shoule be the horizontal around y=0(less error)
# Bad: other pattern like curve, funnel etc...

# 2. Normal Q-Q Plot (Top Right):
# tests if residuals are normally distributed
# Good: points should fall approximately along the diagonal line
# Bad: points deviate(<U+0E40><U+0E1A><U+0E35><U+0E48><U+0E22><U+0E07><U+0E40><U+0E1A><U+0E19>) significantly from the line, especially at the ends

# 3. Scale-Location Plot (Bottom Left):
# squared root of standardized residuals against fitted values
# Good: Horizontal line with randomly scratterd points
# Bad: increase or decrease pattern

# 4. Redidual vs Leverage (Bottom Right):
# indentify influential outlier using Cook's distance
# Good: All points inside the dashed lines (cook's distance)
# Bad: Points outside the dashed lines (highly influential outliers)

# How to Interpret Results:
# <U+2705> Model assumptions are met if:
# Plot 1: Random scatter around y=0
# Plot 2: Points follow diagonal line
# Plot 3: Horizontal trend with equal spread
# Plot 4: No points beyond Cook's distance

# <U+274C> Model assumptions violated if:
# Curved patterns (non-linearity)
# Funnel shapes (heteroscedasticity)
# Points far from Q-Q line (non-normality)
# Influential outliers present

# Interpreting the Diagnostic Plots:
# Residuals vs. Fitted: The red line is relatively flat and the points are scattered randomly around the 0 line. This is good; it suggests the relationship is linear and the variance is roughly constant (homoscedasticity).
# Normal Q-Q: The points fall very closely along the dashed line. This indicates that the residuals are approximately normally distributed.
# Conclusion: Based on these plots, our Fertility outcome variable is reasonably suitable for a linear regression model.

# Reset plotting area
par(mfrow = c(1, 1))

# 4. Finding the Best Predictors We will follow a formal process to build our model.
# Step A: Simple Linear Regression (One Predictor at a Time)

# Let<U+2019>s test each predictor individually to see its standalone relationship with Fertility.
# Fit a model for each predictor
model_agri <- lm(Fertility ~ Agriculture, data = swiss)
model_exam <- lm(Fertility ~ Examination, data = swiss)
model_educ <- lm(Fertility ~ Education, data = swiss)
model_cath <- lm(Fertility ~ Catholic, data = swiss)
model_inf <- lm(Fertility ~ Infant.Mortality, data = swiss)

summary(model_agri) # Adjusted R-squared:  0.1052; p-value = 0.0149
summary(model_exam) # Adjusted R-squared:  0.4042; P = 9.45e-07
summary(model_educ) # Adjusted R-squared:  0.4282; p = 3.66e-07
summary(model_cath) # Adjusted R-squared:  0.1976; P = 0.00103
summary(model_inf) # Adjusted R-squared:  0.1552; P = 0.00359
# R-squared -> tell you how well your data fits the regression model<U+2014>it measures the "goodness of fit."
# be careful higer R-squared does not always mean better model -> maybe it's mean overfitting
# p-value -> test the null hypothesis that the coefficient is equal to zero (no effect).

# We can see that Examination and Education have the strongest standalone effects (lowest p-values, highest R-squared)

# Step B: Multiple Regression (The Full Model)

# Now, let<U+2019>s include all predictors in one model. This helps us see the effect of each variable while controlling for the others.

# The formula `Fertility ~ .` is a shortcut for "use all other columns as predictors"
full_model <- lm(Fertility ~ ., data = swiss)
summary(full_model)

# Call:
# lm(formula = Fertility ~ ., data = swiss)

# Residuals:
#      Min       1Q   Median       3Q      Max
# -15.2743  -5.2617   0.5032   4.1198  15.3213

# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)      66.91518   10.70604   6.250 1.91e-07 ***
# Agriculture      -0.17211    0.07030  -2.448  0.01873 *
# Examination      -0.25801    0.25388  -1.016  0.31546
# Education        -0.87094    0.18303  -4.758 2.43e-05 ***
# Catholic          0.10412    0.03526   2.953  0.00519 **
# Infant.Mortality  1.07705    0.38172   2.822  0.00734 **
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 7.165 on 41 degrees of freedom
# Multiple R-squared:  0.7067,    Adjusted R-squared:  0.671
# F-statistic: 19.76 on 5 and 41 DF,  p-value: 5.594e-10

# interpret watch Pr(>|t|) and Adjusted R-squared
# Coefficients: Agriculture, Education, and Catholic are still significant predictors even when controlling for others.
# The Surprise: Examination is no longer significant! Why? Because it's highly correlated with Education. Once Education is in the model, Examination provides no new information. This is called multicollinearity.

# Step C: Stepwise Regression for an Optimized Model

# We can use an automated algorithm called stepwise regression to find a good combination of predictors. It starts with the full model and iteratively removes the least useful predictor until only significant ones remain.

# The step() function performs stepwise model selection
step_model <- step(full_model, direction = "backward", trace = 0) # trace=0 hides the step-by-step output

# Print the summary of the final, optimized model
summary(step_model)

# Call:
# lm(formula = Fertility ~ Agriculture + Education + Catholic +
#     Infant.Mortality, data = swiss)

# Residuals:
#      Min       1Q   Median       3Q      Max
# -14.6765  -6.0522   0.7514   3.1664  16.1422

# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)      62.10131    9.60489   6.466 8.49e-08 ***
# Agriculture      -0.15462    0.06819  -2.267  0.02857 *
# Education        -0.98026    0.14814  -6.617 5.14e-08 ***
# Catholic          0.12467    0.02889   4.315 9.50e-05 ***
# Infant.Mortality  1.07844    0.38187   2.824  0.00722 **
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 7.168 on 42 degrees of freedom
# Multiple R-squared:  0.6993,    Adjusted R-squared:  0.6707
# F-statistic: 24.42 on 4 and 42 DF,  p-value: 1.717e-10

# Interpret
# Final Model: The stepwise procedure kept Agriculture, Education, Catholic, and Infant.Mortality as the most important predictors. Its Adjusted R-squared is 0.70, meaning this model explains about 70% of the variance in the Fertility rate across the Swiss provinces.

# Create a data frame with two new hypothetical provinces
# 2 province A and B
new_provinces <- data.frame(
    Agriculture = c(60, 10), # Province A is highly agricultural, B is not
    Examination = c(10, 40), # This variable wasn't in the final model, but we include it for completeness
    Education = c(5, 50), # Province A has low education, B has high education
    Catholic = c(95, 20), # Province A is highly Catholic, B is not
    Infant.Mortality = c(22, 18) # Province A has higher infant mortality
)

rownames(new_provinces) <- c("Province A (Traditional)", "Province B (Modern)")

print(new_provinces)

# Use the predict() function with our final model
predicted_fertility <- predict(step_model, newdata = new_provinces)

# Show the predicted outcomes
print(predicted_fertility)

# Province A (Traditional)      Province B (Modern) 
#                 83.49198                 33.44723

# Do you notice that ‘Agriculture’ is positively correlated with fertility rate in the crude model, but negatively correlated with fertility rate in the prediction model?
#   * Simple Regression (Bivariate): Shows the crude association. In this case, Agriculture acted as a proxy for a traditional, less-educated society, and was thus positively associated with Fertility.

#   *Multiple Regression: Shows the adjusted association, controlling for other factors. It attempts to isolate the independent effect of one variable.

#   *The Sign Flip: A sign flip is a strong indicator that your simple model was suffering from omitted variable bias (i.e., you were missing a key confounder).

#   *Conclusion: The power of multiple regression is its ability to disentangle these complex, overlapping relationships. It allows us to see that once we account for the powerful effect of Education, the independent contribution of Agriculture to Fertility is actually slightly negative.

# In the final prediction model, because some of our predictors are highly correlated with each other (see the correlation matrix), we would wonder if there is a multicollinearity problem in the model.

# What is Multicollinearity? Multicollinearity occurs when two or more predictor variables in a regression model are highly correlated with each other. It means that one predictor can be linearly predicted from the others with a substantial degree of accuracy.

# What are the Consequences? It does not reduce the overall predictive power of the model (the R-squared will still be high).

# It dramatically inflates the standard errors of the coefficients for the correlated predictors. This makes the coefficients unstable and difficult to interpret. The model doesn’t know which variable to “give credit” to, so the p-values and even the signs of the coefficients can swing wildly depending on which other variables are in the model.

# How Do We Diagnose It? The best way to diagnose multicollinearity is by calculating the Variance Inflation Factor (VIF) for each predictor in the model.

# VIF Interpretation:

#   * VIF = 1: No correlation between this predictor and others.

#   *VIF > 5: A common threshold for concern. It suggests moderate multicollinearity.

#   *VIF > 10: A common threshold for a serious problem. It suggests high multicollinearity that should be addressed.

# Let’s test our final model (step_model) for multicollinearity. We’ll use the vif() function from the car package.

# Load the library
install.packages("car")
library(car)

## Loading required package: carData
## 
## Attaching package: 'car'
## The following object is masked from 'package:dplyr':
## 
##     recode

# The final model from our previous analysis
# step_model <- lm(formula = Fertility ~ Agriculture + Education + Catholic + Infant.Mortality, 
#                  data = swiss)

# Calculate the VIF for each predictor in the model
vif_values <- vif(step_model)

# Print the results
print(vif_values)

# Agriculture        Education         Catholic Infant.Mortality 
#     2.147153         1.816361         1.299916         1.107528

# from the example the vif is not tool high to be consider to be a problem (less than 5)

print(vif(full_model))
