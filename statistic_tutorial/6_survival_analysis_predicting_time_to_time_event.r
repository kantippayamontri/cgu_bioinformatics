# logistic regression -> we predict is event occur? -> yes/no
# survival analysis -> we predict "when" the event will occur -> we predict when it happen.
#   -> the question like "Does new therapy extend the time until cancer recurs", "What patient characteristics are associated with longer survival?", "can we predict a specific patient's probability of surviving for 5 years"

# logistic regression -> interested in finding the association btw risk factors and the outcome -> ex) the presence or absence of disease
# survival analysis -> only finding association about "time" to the event
#   -> time until cancer/disease death after treatment intervention.
#   -> time until disease occurrence (e.g. AIDS event for HIV patients.)
#   -> time until disease/tumor recurrence.

# we will use gbsg dataset

# workflow
# 1. Understand the data
# 2. Form Hypothesis
# 3. Clean & Prepare
# 4. Analyze
# 5. Interpret & Predict

# 1. Understand the data
# options(repos = c(CRAN = "https://cloud.r-project.org"))
# install.packages("survival")
# install.packages("dplyr")
# install.packages("nloptr", dependencies = TRUE)
# install.packages("lme4", dependencies = TRUE)
# install.packages("car", dependencies = TRUE)
# install.packages("ggpubr", dependencies = TRUE)
# install.packages("survminer", dependencies = TRUE)
# install.packages("survminer")
# install.packages("ggplot2")
# install.packages("PerformanceAnalytics")

library(survival) # The core package for survival analysis
library(dplyr) # For data manipulation
library(survminer) # For beautiful survival plots, including forest plots
library(ggplot2) # For general plotting
library(PerformanceAnalytics) # For correlation matrix

# The gbsg dataset is in the 'survival' package
gbsg <- survival::gbsg

# Look at the structure
str(gbsg)

# 'data.frame':   686 obs. of  11 variables:
#  $ pid    : int  132 1575 1140 769 130 1642 475 973 569 1180 ...
#  $ age    : int  49 55 56 45 65 48 48 37 67 45 ...
#  $ meno   : int  0 1 1 0 1 0 0 0 1 0 ...
#  $ size   : int  18 20 40 25 30 52 21 20 20 30 ...
#  $ grade  : int  2 3 3 3 2 2 3 2 2 2 ...
#  $ nodes  : int  2 16 3 1 5 11 8 9 1 1 ...
#  $ pgr    : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ er     : int  0 0 0 4 36 0 0 0 0 0 ...
#  $ hormon : int  0 0 0 0 1 0 0 1 1 0 ...
#  $ rfstime: int  1838 403 1603 177 1855 842 293 42 564 1093 ...
#  $ status : int  0 1 0 0 0 1 1 0 1 1 ...

# ** Major outcomes
# rfstime: Recurrence-free survival time in days (our time variable).
# status: Event indicator (0 = alive/censored, 1 = recurrence or death) (our event variable).

# ** Predictor/Risk factors
# meno: menopausal status (0= premenopausal, 1= postmenopausal)
# size: tumor size, mm
# grade: tumor grade (1-3)
# nodes: number of positive lymph nodes
# pgr: progesterone receptors (fmol/l)
# er: estrogen receptors (fmol/l)
# hormon: hormonal therapy (0=no, 1=yes)

# 2. Data Cleaning: Spotting and Correcting Data Types

# Let's clean the data using dplyr
gbsg_clean <- gbsg %>%
  mutate(
    # Convert menopausal status to a factor
    meno = factor(meno, levels = c(0, 1), labels = c("Premenopausal", "Postmenopausal")),

    # Convert tumor grade to an ordered factor
    grade = factor(grade, ordered = TRUE, levels = c("1", "2", "3")),

    # Convert hormonal therapy to a factor
    hormon = factor(hormon, levels = c(0, 1), labels = c("No", "Yes")),

    # Also convert the event status to a factor for clarity in plots
    status = factor(status, levels = c(0, 1), labels = c("Censored", "Event"))
  )

# Let's check the dataset again
str(gbsg_clean)

## 'data.frame':    686 obs. of  11 variables:
##  $ pid    : int  132 1575 1140 769 130 1642 475 973 569 1180 ...
##  $ age    : int  49 55 56 45 65 48 48 37 67 45 ...
##  $ meno   : Factor w/ 2 levels "Premenopausal",..: 1 2 2 1 2 1 1 1 2 1 ...
##  $ size   : int  18 20 40 25 30 52 21 20 20 30 ...
##  $ grade  : Ord.factor w/ 3 levels "1"<"2"<"3": 2 3 3 3 2 2 3 2 2 2 ...
##  $ nodes  : int  2 16 3 1 5 11 8 9 1 1 ...
##  $ pgr    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ er     : int  0 0 0 4 36 0 0 0 0 0 ...
##  $ hormon : Factor w/ 2 levels "No","Yes": 1 1 1 1 2 1 1 2 2 1 ...
##  $ rfstime: int  1838 403 1603 177 1855 842 293 42 564 1093 ...
##  $ status : Factor w/ 2 levels "Censored","Event": 1 2 1 1 1 2 2 1 2 2 ...

# 3. Understanding Variables & Forming Hypotheses
# meno: Postmenopausal women(ผญ หมดประจำเดือน) are generally older -> risk factor
# size: larger tumor -> higher risk of recurrence(อาการกำเริบ)(Higher Hazard ratio)
# grade: tumor grade = measure of cell abnormality -> higher grade(3vs1) = higher risk
# nodes: number of positive lymp node -> indicator of cancer spread -> higher = higher risk
# pgr & er: Progesterone and estrogen receptors -> hormone sensitivity. higher -> lower risk => higher pgr&er -> lower risk (Hazard ratio < 1)
# hormon: receiving hormonal therapy. higher -> lower risk.

summary(gbsg_clean)

# 4. Performing exploratory data analysis

install.packages("gtsummary")
library(gtsummary)

table1 <- gbsg_clean %>%
  # 1. Select all columns EXCEPT for pid
  select(-pid) %>%
  # 2. This is the main function to create the table
  tbl_summary(
    # --- Column Settings ---
    by = status, # This tells gtsummary to create columns for each level of 'status'

    # --- Statistics Formatting ---
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", # For continuous vars: show Mean (SD)
      all_categorical() ~ "{n} ({p}%)" # For categorical vars: show N (%)
    )
    # The tbl_summary() function ends here
  ) %>% # <--- THIS IS THE FIX: A pipe to add the next step

  # 3. Now we ADD the p-value column to the table we just created
  add_p()

# 4. Print the final table
table1

# Furthermore, we also check the associations between predictors before building the survival model.
# From the previous lectures, we know:
# Continous vs. Continuous: Pearson’s correlation
# Continuous vs. Dichotomous: Independent T-test
# Continous vs. Polytomous (>2): ANOVA
# Categorical vs. Categorical: Chi-square test

# 1. Relationship between Continuous Predictors: Correlation
# Question: Are the two hormone receptor levels, pgr and er, related to each other?
cor_test_receptors <- cor.test(gbsg_clean$pgr, gbsg_clean$er)
cor_test_receptors


# or we can do an overal check
X11() # on Linux
chart.Correlation(gbsg_clean[, c(2, 4, 6:8)], histogram = TRUE, pch = 19)

# 2. Relationship between Continuous and Categorical (2 levels): T-test
# Question: Do postmenopausal women have, on average, a different tumor size than premenopausal women?

var.test(size ~ meno, data = gbsg_clean, ) # sig, suggest variances are not equal

#         F test to compare two variances

# data:  size by meno
# F = 1.2995, num df = 289, denom df = 395, p-value = 0.01595
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  1.050103 1.614055
# sample estimates:
# ratio of variances
#           1.299533

# p-value < 0.05 -> the mean the variance btw 2 postmeno and premeno are significatly different

ttest_size_meno <- t.test(size ~ meno, data = gbsg_clean, var.equal = F)
ttest_size_meno

##
##  Welch Two Sample t-test
##
## data:  size by meno
## t = 1.0782, df = 573.29, p-value = 0.2814
## alternative hypothesis: true difference in means between group Premenopausal and group Postmenopausal is not equal to 0
## 95 percent confidence interval:
##  -0.9986919  3.4294477
## sample estimates:
##  mean in group Premenopausal mean in group Postmenopausal
##                     30.03103                     28.81566

# Nope, postmenopausal women don't have a different tumor size than premenopausal women.
# Even, postmenopausal women have a smaller mean tumor size (28.82 mm) compared to premenopausal women (30.03 mm).

# แม้ว่า mean จะต่างกัน แต่ว่า tumor size ไม่ต่าง (p-value > 0.05)

# 3. Relationship between Continuous and Categorical (>2 levels): ANOVA

# Question: Does the number of positive lymph nodes differ across the three tumor grades?

# Perform ANOVA
anova_nodes_grade <- aov(nodes ~ grade, data = gbsg_clean)
summary(anova_nodes_grade)

##              Df Sum Sq Mean Sq F value   Pr(>F)
## grade         2    437  218.62   7.429 0.000643 ***
## Residuals   683  20100   29.43
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

TukeyHSD(anova_nodes_grade)

##   Tukey multiple comparisons of means
##     95% family-wise confidence level
##
## Fit: aov(formula = nodes ~ grade, data = gbsg_clean)
##
## $grade
##         diff         lwr      upr     p adj
## 2-1 1.679930  0.14042299 3.219437 0.0285082
## 3-1 2.827697  1.09194305 4.563451 0.0004166
## 3-2 1.147767 -0.02445302 2.319988 0.0564866

# Here ANOVA test shows at least one of the grade group has significantly different nodes number.
# Using the Tukey's posthoc test, we can see the order of nodes number is grade 3 > 2 > 1.
# Both grade 2 vs. grade 1, and grade 3 vs. grade 1 show a significant differences of nodes number.

# A boxplot is the best way to visualize this!
X11()
ggplot(gbsg_clean, aes(x = grade, y = nodes, fill = grade)) +
  geom_boxplot() +
  labs(
    title = "Number of Positive Nodes by Tumor Grade",
    x = "Tumor Grade", y = "Number of Positive Nodes"
  ) +
  theme_minimal()

# 4. Relationship between Categorical Predictors: Chi-squared Test

# Question: Is there an association between menopausal status and tumor grade?

# Create a contingency table
contingency_table_meno_grade <- table(gbsg_clean$meno, gbsg_clean$grade)
print(contingency_table_meno_grade)

# Perform the Chi-squared test
chisq_test_meno_grade <- chisq.test(contingency_table_meno_grade)
print(chisq_test_meno_grade)
# The result shows there is no statistical association between grade and menopausal status.

# You can use the same approaches to examine the associations between your variables.

# 5. Performing the Survival Analysis

# Step A: Univariate Analysis (Simple Models)

# Quantifying risk: continous variables
# test each predictor one by one -> see the association with survival.
# we will start by creating "Survival Object", and use Cox proportional Hazards model for the analysis.

# The key output from a Cox model is the Hazard Ratio (HR).
# HR = 1: The predictor has no effect on the hazard of the event.
# HR > 1: The predictor is a risk factor. An increase in the predictor is associated with an increased hazard of the event.
# HR < 1: The predictor is a protective factor. An increase in the predictor is associated with a decreased hazard of the event.

# Create the Survival object
# This combines the time-to-event (rfstime) and the event status (status)
surv_obj <- Surv(time = gbsg_clean$rfstime, event = gbsg_clean$status == "Event")
head(surv_obj)
# [1] 1838+  403  1603+  177+ 1855+  842
# + mean status censored
# (no sign) mean status Event
# 1838+, 1603+ -> 1838 days (Still alive at end of study) or (Lost contact, last seen healthy) but we don't know exactly but we stopped observing the patient before the event occurred.
# 403, 842 days -> cancer occured

summary(coxph(surv_obj ~ age, data = gbsg_clean))
## Call:
## coxph(formula = surv_obj ~ age, data = gbsg_clean)
##
##   n= 686, number of events= 299
##
##          coef exp(coef)  se(coef)      z Pr(>|z|)
## age -0.004485  0.995525  0.005887 -0.762    0.446
##
##     exp(coef) exp(-coef) lower .95 upper .95
## age    0.9955      1.004    0.9841     1.007
##
## Concordance= 0.519  (se = 0.018 )
## Likelihood ratio test= 0.58  on 1 df,   p=0.4
## Wald test            = 0.58  on 1 df,   p=0.4
## Score (logrank) test = 0.58  on 1 df,   p=0.4

summary(coxph(surv_obj ~ size, data = gbsg_clean))
## Call:
## coxph(formula = surv_obj ~ size, data = gbsg_clean)
##
##   n= 686, number of events= 299
##
##          coef exp(coef) se(coef)     z Pr(>|z|)
## size 0.014839  1.014949 0.003507 4.231 2.32e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##      exp(coef) exp(-coef) lower .95 upper .95
## size     1.015     0.9853     1.008     1.022
##
## Concordance= 0.572  (se = 0.018 )
## Likelihood ratio test= 15.68  on 1 df,   p=7e-05
## Wald test            = 17.9  on 1 df,   p=2e-05
## Score (logrank) test = 17.91  on 1 df,   p=2e-05

summary(coxph(surv_obj ~ nodes, data = gbsg_clean))
## Call:
## coxph(formula = surv_obj ~ nodes, data = gbsg_clean)
##
##   n= 686, number of events= 299
##
##          coef exp(coef) se(coef)     z Pr(>|z|)
## nodes 0.05860   1.06035  0.00674 8.694   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##       exp(coef) exp(-coef) lower .95 upper .95
## nodes      1.06     0.9431     1.046     1.074
##
## Concordance= 0.645  (se = 0.016 )
## Likelihood ratio test= 50.04  on 1 df,   p=2e-12
## Wald test            = 75.59  on 1 df,   p=<2e-16
## Score (logrank) test = 78.46  on 1 df,   p=<2e-16

summary(coxph(surv_obj ~ pgr, data = gbsg_clean))
## Call:
## coxph(formula = surv_obj ~ pgr, data = gbsg_clean)
##
##   n= 686, number of events= 299
##
##           coef  exp(coef)   se(coef)      z Pr(>|z|)
## pgr -0.0027720  0.9972319  0.0005759 -4.813 1.49e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##     exp(coef) exp(-coef) lower .95 upper .95
## pgr    0.9972      1.003    0.9961    0.9984
##
## Concordance= 0.636  (se = 0.016 )
## Likelihood ratio test= 34.05  on 1 df,   p=5e-09
## Wald test            = 23.17  on 1 df,   p=1e-06
## Score (logrank) test = 21.19  on 1 df,   p=4e-06

summary(coxph(surv_obj ~ er, data = gbsg_clean))
## Call:
## coxph(formula = surv_obj ~ er, data = gbsg_clean)
##
##   n= 686, number of events= 299
##
##          coef  exp(coef)   se(coef)      z Pr(>|z|)
## er -0.0009464  0.9990540  0.0004632 -2.043    0.041 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##    exp(coef) exp(-coef) lower .95 upper .95
## er    0.9991      1.001    0.9981         1
##
## Concordance= 0.591  (se = 0.017 )
## Likelihood ratio test= 4.7  on 1 df,   p=0.03
## Wald test            = 4.17  on 1 df,   p=0.04
## Score (logrank) test = 4.18  on 1 df,   p=0.04

# NOTE:
# We interpret the results using exp(coef)(Hazard Ratio).
# Age: for every 1 year increase in age, the hazard of recurrence increases by a multiplicative factor of 0.995525. (not significant)
# Size: for every 1mm increase in tumor size, the hazard of recurrence increases by a multiplicative factor of 1.014949 (significant).
# nodes: for every 1 increase in nodes number, the hazard of recurrence increases by a multiplicative factor of 1.06035 (significant).
# pgr: for every 1 unit increase in pgr, the hazard of recurrence increases by a multiplicative factor of 0.9972319 (significant).
# er: for every 1 unit increase in tumor size, the hazard of recurrence increases by a multiplicative factor of 0.9990540 (marginal significant).

# Quantifying risk: categorical variables using Kaplan-Meier (K-M curve)
# A K-M curve is a non-parametric method -> plot survival plot over time.
#                                         -> allow us to compare the survival experience of differences groups (e.g. patients with Grade 1 vs Grade 3 tumors)

# the best tool for ploting survival plot is "ggsurvplot" <- build on ggplot2

fit_meno <- survfit(surv_obj ~ meno, data = gbsg_clean)
fit_grade <- survfit(surv_obj ~ grade, data = gbsg_clean)
fit_hormon <- survfit(surv_obj ~ hormon, data = gbsg_clean)

ggsurvplot(
  fit_meno,
  data = gbsg_clean,
  # --- Core Plot Customization ---
  conf.int = TRUE, # Show confidence intervals
  pval = TRUE, # Show log-rank test p-value
  # --- Aesthetics ---
  palette = c("steelblue", "salmon"),
  legend.labs = c("Premenopausal", "Postmenopausal"),
  legend.title = "Menopausal Status",
  # --- Risk Table Settings ---
  risk.table = TRUE, # This is the key argument to add the table
  tables.height = 0.2, # Adjust the height of the table
  # --- Labels ---
  title = "Kaplan-Meier Curve for Recurrence-Free Survival by Menopausal Status", xlab = "Time (in days)"
)

ggsurvplot(
  fit_grade,
  data = gbsg_clean,
  conf.int = TRUE,
  pval = TRUE,
  # --- Aesthetics ---
  palette = c("steelblue", "salmon", "seagreen"),
  legend.labs = c("Grade 1", "Grade 2", "Grade 3"),
  legend.title = "Grade Status",
  risk.table = TRUE,
  tables.height = 0.2,
  title = "Kaplan-Meier Curve for Recurrence-Free Survival by Grade", xlab = "Time (in days)"
)

ggsurvplot(
  fit_hormon, 
  data = gbsg_clean,
  conf.int = TRUE,          
  pval = TRUE,             
  # --- Aesthetics ---
  palette = c("steelblue", "salmon"),
  legend.labs = c("Hormone_No", "Hormone_Yes"),
  legend.title = "Hormone Treatment Status",
  risk.table = TRUE,       
  tables.height = 0.2,      
  title = "Kaplan-Meier Curve for Recurrence-Free Survival by Hormone Treatment",xlab = "Time (in days)")

# Hormone Plot:

# The Axes: The X-axis is time. The Y-axis is the estimated probability of being recurrence-free (survival probability).

# The Curves: Each colored line represents a group. The lines are “step functions” that drop down each time an event (a recurrence) occurs in that group. A steeper drop means a worse survival outcome. We can see the “Hormone_No” curve is consistently below the “Hormone_Yes” curve, suggesting worse survival.

# The P-value (Log-Rank Test): The p-value of 0.0034 is statistically significant (p < 0.05). This is the formal hypothesis test confirming that the difference we see between the two curves is unlikely to be due to random chance.

# The Risk Table: This table at the bottom is crucial. It shows the number of patients still at risk of having an event at specific time points in each group. It gives context to the curve; for example, you can see how many patients are left in the study at the 2000-day mark.

# step B: Multivariate Analysis & Stepwise Model Selection
# we will build the model with include all predictor

# Before we move on to the final model, let's check coxph for 'grade'
# Because we defined 'grade' as an order factor

summary(coxph(surv_obj~grade,data=gbsg_clean))
## Call:
## coxph(formula = surv_obj ~ grade, data = gbsg_clean)
## 
##   n= 686, number of events= 299 
## 
##            coef exp(coef) se(coef)      z Pr(>|z|)    
## grade.L  0.8158    2.2610   0.1849  4.411 1.03e-05 ***
## grade.Q -0.2408    0.7860   0.1212 -1.988   0.0469 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##         exp(coef) exp(-coef) lower .95 upper .95
## grade.L     2.261     0.4423    1.5736    3.2488
## grade.Q     0.786     1.2723    0.6199    0.9967
## 
## Concordance= 0.577  (se = 0.015 )
## Likelihood ratio test= 24.23  on 2 df,   p=5e-06
## Wald test            = 19.75  on 2 df,   p=5e-05
## Score (logrank) test = 21.1  on 2 df,   p=3e-05

# as you can see, the results now shows 'grade.L' and 'grade.Q'
# How to interpret your likely output:
# grade.L is significant (p < 0.05): This is the most important result. It tells you there is a significant linear trend. As the grade increases, the risk of recurrence consistently increases.
# grade.Q is marginal significant: This is also very common. It means that a simple straight-line trend is roughly a good enough description of the relationship (preferable p > 0.05). There's no statistical evidence for a more complex U-shape or curve.

# We can change it to category factor

# In your gbsg_clean creation code, change this line:
gbsg_clean <- gbsg %>%
  mutate(
    # Convert tumor grade to an ordered factor
    grade = factor(grade, ordered = FALSE, levels = c("1", "2", "3")))

# Now, re-run the Cox model with this new data frame
fit_grade_unordered <- coxph(surv_obj ~ grade, data = gbsg_clean)
summary(fit_grade_unordered)
## Call:
## coxph(formula = surv_obj ~ grade, data = gbsg_clean)
## 
##   n= 686, number of events= 299 
## 
##          coef exp(coef) se(coef)     z Pr(>|z|)    
## grade2 0.8718    2.3912   0.2460 3.543 0.000395 ***
## grade3 1.1537    3.1701   0.2615 4.411 1.03e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##        exp(coef) exp(-coef) lower .95 upper .95
## grade2     2.391     0.4182     1.476     3.873
## grade3     3.170     0.3155     1.899     5.293
## 
## Concordance= 0.577  (se = 0.015 )
## Likelihood ratio test= 24.23  on 2 df,   p=5e-06
## Wald test            = 19.75  on 2 df,   p=5e-05
## Score (logrank) test = 21.1  on 2 df,   p=3e-05

# As you can see here, now we have new results with grade 2 vs. grade 1 and grade 3 vs. grade 1.
# It means "Patients with Grade 3 tumors have a 3.17 times higher risk of recurrence than patients with Grade 1 tumors" and "Patients with Grade 2 tumors have a 2.39 times higher risk of recurrence than patients with Grade 1 tumors."

# 1. Build the full model with all potential predictors
full_model <- coxph(surv_obj ~ age + meno + size + grade + nodes + pgr + er + hormon, data = gbsg_clean)

library(car)
vif(full_model)
##            GVIF Df GVIF^(1/(2*Df))
## age    2.483294  1        1.575847
## meno   2.407322  1        1.551555
## size   1.200401  1        1.095628
## grade  1.078552  2        1.019085
## nodes  1.196050  1        1.093641
## pgr    1.140847  1        1.068104
## er     1.195659  1        1.093462
## hormon 1.065986  1        1.032466
# Warning message:
# In vif.default(full_model) : No intercept: vifs may not be sensible.

# Because we have a variable 'grade' with 3 levels, here we checked GVIF instead.
# GVIF^(1/(2*Df)) is a standardized value, adjusted for the degrees of freedom, that is designed to be directly comparable to the standard VIF scale.
# From our result, it seems that we don't have multicollinearity problem.

# Bare in mind these factors are not significant: age and er (marginal significant but highly correlated with pgr). Also menopausal status.

# 2. Use backward stepwise selection to find the optimal model
final_model <- step(full_model, direction = "backward", trace = 0)
summary(final_model)
## Call:
## coxph(formula = surv_obj ~ size + grade + nodes + pgr + hormon, 
##     data = gbsg_clean)
## 
##   n= 686, number of events= 299 
## 
##              coef  exp(coef)   se(coef)      z Pr(>|z|)    
## size    0.0073129  1.0073397  0.0038897  1.880   0.0601 .  
## grade2  0.6440346  1.9041479  0.2490184  2.586   0.0097 ** 
## grade3  0.7882290  2.1994977  0.2682585  2.938   0.0033 ** 
## nodes   0.0489976  1.0502178  0.0074529  6.574 4.89e-11 ***
## pgr    -0.0022168  0.9977856  0.0005538 -4.003 6.26e-05 ***
## hormon -0.3235201  0.7235974  0.1258239 -2.571   0.0101 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##        exp(coef) exp(-coef) lower .95 upper .95
## size      1.0073     0.9927    0.9997    1.0150
## grade2    1.9041     0.5252    1.1688    3.1022
## grade3    2.1995     0.4546    1.3001    3.7211
## nodes     1.0502     0.9522    1.0350    1.0657
## pgr       0.9978     1.0022    0.9967    0.9989
## hormon    0.7236     1.3820    0.5655    0.9260
## 
## Concordance= 0.689  (se = 0.015 )
## Likelihood ratio test= 102.5  on 6 df,   p=<2e-16
## Wald test            = 112.2  on 6 df,   p=<2e-16
## Score (logrank) test = 118.5  on 6 df,   p=<2e-16

# From the results, with larger tumor size, higher level of tumor grade, more nodes, there are increased risk of recurrence. Alternatively, higher level of pgr and those with hormone treatment will have lower risk of recurrence.

# 5. Visualizing the Final Model with a Forest Plot
# A forest plot is the best way to visualize the results of a survival model. It shows the Hazard Ratio and 95% confidence interval for each predictor.

# Use the ggforest function from the survminer package
ggforest(final_model, data = gbsg_clean)
# How to Read the Forest Plot:
# Dots: Represent the Hazard Ratio for each predictor.
# Horizontal Lines: Represent the 95% confidence interval.
# The Vertical Line at 1.0: This is the line of "no effect." If a confidence interval crosses this line, the predictor is not statistically significant at the p=0.05 level.

# 6. Building a Prediction Model and Predicting Risk
# Finally, we can use our final_model to predict a risk score for new patients. This score is a relative measure of how a patient’s risk compares to the average patient in the dataset.

# Let's create two new, hypothetical patients
new_patients <- data.frame(
  grade = factor(c("1", "3")),
  hormon = factor(c("Yes","No")),
  nodes = c(1, 15), # Patient A has few nodes, Patient B has many
  pgr = c(100, 5),   # Patient A has high pgr, Patient B has low pgr
  size=c(20,40))

rownames(new_patients) <- c("Patient A (Low Risk Profile)", "Patient B (High Risk Profile)")
new_patients
##                               grade hormon nodes pgr size
## Patient A (Low Risk Profile)      1    Yes     1 100   20
## Patient B (High Risk Profile)     3     No    15   5   40

# Use the predict() function with type="risk"
# This gives us the prognostic index (or risk score)
predicted_risk <- predict(final_model, newdata = new_patients, type = "risk")
predicted_risk

# Interpretation:

# The model predicts a risk score of 0.31 for Patient A and 1.65 for Patient B. This means that Patient B’s hazard (or instantaneous risk) of recurrence is much higher than Patient A’s at any given time point. Specifically, Patient B’s risk is 1.65/0.31= 5.241574 times higher than Patient A’s. This risk score is a powerful tool for stratifying patients and guiding clinical decisions.