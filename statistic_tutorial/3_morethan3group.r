# Scenario C: More than Two groups (ANOVA)

# ANOVA's null hypothesis is that the means of ALL groups are equal.
# H<U+2080>: <U+03BC>_0.5 = <U+03BC>_1.0 = <U+03BC>_2.0
# H<U+2090>: At least one group mean is different.

data(ToothGrowth)
table(ToothGrowth$dose)

# First, it's very important to tell R that 'dose' is a categorical variable (a factor), not a number.
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

# Now, we build the ANOVA model
anova_model <- aov(len ~ dose, data = ToothGrowth)
summary(anova_model) # Use the summary() function to see the results

##             Df Sum Sq Mean Sq F value   Pr(>F)
## dose         2   2426    1213   67.42 9.53e-16 ***
## Residuals   57   1026      18
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### How to Interpret the R Output:
# Pr(>F): This is the p-value for the ANOVA test. You will see a very small number (e.g., 9.53e-16).
# Conclusion: The p-value is far less than 0.05. We reject the null hypothesis. We have strong evidence that at least one dose level leads to a different mean tooth growth.
# The Limitation of ANOVA: ANOVA tells us that there is a difference somewhere among the groups, but it doesn't tell us which specific groups are different. Is 0.5 different from 1.0? Is 1.0 different from 2.0?

# For that, we need a post-hoc test.
# Follow-up: Tukey's Honest Significant Differences (HSD) Test
# Run a post-hoc test to see which specific pairs of groups are different.
TukeyHSD(anova_model)

##   Tukey multiple comparisons of means
##     95% family-wise confidence level
##
## Fit: aov(formula = len ~ dose, data = ToothGrowth)
##
## $dose
##         diff       lwr       upr    p adj
## 1-0.5  9.130  5.901805 12.358195 0.00e+00
## 2-0.5 15.495 12.266805 18.723195 0.00e+00
## 2-1    6.365  3.136805  9.593195 4.25e-05

#### How to Interpret the TukeyHSD Output:
# Look at the p adj (adjusted p-value) column for each pairwise comparison.
# You will see that the p-values for 1.0-0.5, 2.0-0.5, and 2.0-1.0 are all very small.
# Final Conclusion: All three dose groups are significantly different from one another. Increasing the dose significantly increases tooth growth.

ToothGrowth$dose <- as.factor(ToothGrowth$dose)

library(ggplot2)
# Create the plot
ggplot(ToothGrowth, aes(x = dose, y = len, fill = dose)) +

    # 1. Add boxplots
    geom_boxplot(alpha = 0.7) +

    # 2. Add jittered data points
    geom_jitter(width = 0.2, color = "black", size = 1.5) +

    # 3. Add labels and a title
    labs(
        title = "Tooth Growth by Supplement Dose",
        subtitle = "Comparing three dose levels",
        x = "Dose (mg/day)",
        y = "Tooth Length"
    ) +

    # 4. Use a clean theme
    theme_minimal() +

    # 5. Remove the redundant legend
    guides(fill = "none")
