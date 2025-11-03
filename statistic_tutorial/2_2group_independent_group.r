# independent groups - comparing two separate groups

# We will use the built-in 'ToothGrowth' dataset.
# magine we are testing a vitamin supplement for its effect on tooth growth. We have two independent groups of subjects: one group receives orange juice (OJ) and the other receives ascorbic acid (VC). The subjects are not related.
# OJ = orange juice
# VC = ascorbic acid (VC)

# Load and inspect the data
data(ToothGrowth)
head(ToothGrowth)
table(ToothGrowth$supp, ToothGrowth$dose)
colnames(ToothGrowth)

# whether there is a tooth length differences between supplement method? -> two sample t-test

##### It's very important to do a variance test before t-test -> to check the variance from 2 groups are equal or not equal !!!!!
# if the variances are equal -> use Student's t-test
# if the variances are not equal -> use Welch's t-test

# BUT! before that, you need to perform a variance test
# a variance test is to test variations between tge two groups
# first of all, test the variances of these two groups
var.test(len ~ supp, data = ToothGrowth)

#         F test to compare two variances

# data:  len by supp
# F = 0.6386, num df = 29, denom df = 29, p-value = 0.2331
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  0.3039488 1.3416857
# sample estimates:
# ratio of variances
#          0.6385951

## p-value = 0.2231: This P-value is greater than 0.05.
# That means the variances are equal between these two groups.

# Two-sample t-test assumption:
# H0: There is no difference in mean tooth growth between the OJ and VC groups.
# Ha: There is a difference.

# The formula is the same, but we set paired = FALSE (which is the default)
independent_test_result <- t.test(len ~ supp, data = ToothGrowth, var.equal = TRUE)
print(independent_test_result)

##
##  Two Sample t-test
##
## data:  len by supp
## t = 1.9153, df = 58, p-value = 0.06039
## alternative hypothesis: true difference in means between group OJ and group VC is not equal to 0
## 95 percent confidence interval:
##  -0.1670064  7.5670064
## sample estimates:
## mean in group OJ mean in group VC
##         20.66333         16.96333

#### How to Interpret the R Output:
# p-value = 0.06063: This p-value is greater than 0.05.
# Conclusion: We fail to reject the null hypothesis. We do not have statistically significant evidence to conclude that there is a difference in mean tooth growth between the OJ and VC supplements.

# Create the plot
library(ggplot2)
ggplot(ToothGrowth, aes(x = supp, y = len, fill = supp)) +

    # 1. Add boxplots to summarize the distributions
    # We make them slightly transparent with alpha
    geom_boxplot(alpha = 0.6) +

    # 2. Add the individual data points with a bit of horizontal "jitter"
    # This helps us see the raw data and the sample size
    geom_jitter(width = 0.2, color = "black", size = 1.5) +

    # 3. Add labels and a title
    labs(
        title = "Tooth Growth by Supplement Type",
        subtitle = "Comparing Orange Juice (OJ) vs. Ascorbic Acid (VC)",
        x = "Supplement Type",
        y = "Tooth Length"
    ) +

    # 4. Use a clean theme and customize colors
    theme_minimal() +
    scale_fill_manual(values = c("OJ" = "orange", "VC" = "skyblue")) +

    # 5. Remove the legend since the x-axis is already labeled
    guides(fill = "none")

# from the graph we can saw that drinking orange juice will have effect to the length of teeth more than VC -> see from the graph that mean of OJ is greater

# if you think who take the orange juice will hage longer tooth
t.test(len ~ supp, data = ToothGrowth, var.equal = TRUE, alternative="greater")
#p-value = 0.0302 < 0.05
# it means we failed to reject alternatives hypothesis, that mean take OJ will have longer tooth.

# suggestion: use two-sided if you don't know the assumptions