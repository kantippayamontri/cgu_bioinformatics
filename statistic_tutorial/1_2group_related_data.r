# Scenario A: Paired Data - Before and After/Two interventions in the same person # nolint
# We will use the built-in 'sleep' dataset.
# This dataset shows the effect of two soporific drugs (let's call them Drug 1 and Drug 2) on 10 patients. The extra column is the increase in hours of sleep compared to a baseline. # nolint

# load and inspect data
# extra = more sleep hour(+) or less sleep hour(-) than baseline.
# ID = patients ID -> 10 person -> ID is 1 to 10
# group = medicine type 1 and 2
data(sleep) # load sleep dataset
head(sleep)
str(sleep)

sleep <- sleep[order(sleep$ID), ] # order sleep by ID
head(sleep)

# The data is in "long" format. We have one row per patient per drug.
# The 'ID' column shows that patient 1 was measured for group 1 AND group 2. This is PAIRED data. # nolint

# H0: There is no difference in mean sleep increase between Drug 1 and Drug 2.
# Ha There is a difference.
# Let's make the data in another format
library(tidyr)
sleep_wide <- pivot_wider(
    data = sleep,
    id_cols = ID,
    names_from = group,
    values_from = extra,
    names_prefix = "group"
)
head(sleep_wide)

# running the test on wided data
library(t)
t.test(sleep_wide$group1, sleep_wide$group2, paired = TRUE) # default paired=False -> if you want to do paired test you need to change to TRUE

# this is the output
#         Paired t-test

# data:  sleep_wide$group1 and sleep_wide$group2
# t = -4.0621, df = 9, p-value = 0.002833
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#  -2.4598858 -0.7001142
# sample estimates:
# mean difference
#           -1.58 -> (mean group 1 - mean group 2) = -1.58 -> group 1 helps to sleep less than group 2 -> group 2 helps to sleep more then group 1 1.58 hours on average

#### How to Interpret the R Output:
# p-value = 0.002833: This is the most important number. It is much less than our alpha of 0.05.
# Conclusion: We reject the null hypothesis. There is a statistically significant difference in the mean increase in sleep between the two drugs.
# mean of the differences: The output shows -1.58. This means Drug 2 resulted in an average of 1.58 more hours of sleep increase than Drug 1. Which means drug 2 helps to sleep more.

# Strategy to interpret the output:
# 1. check your p-value. if your p-value is less than the significant level (0.05), that mean you reach statistical significant!
# 2. that means you will reject null hypothesis, claim that there is a difference between you paired groups
# 3. if you are interested in the effect, drug 2 helps you to sleep 1.58 more hours (effect)

library(ggplot2)
# Create the plot
ggplot(data = sleep, aes(x = group, y = extra, group = ID)) +
    # 1. Add the lines connecting the points for each ID
    # We make them slightly transparent (alpha=0.5) and grey to see trends
    geom_line(color = "grey", alpha = 0.8) +

    # 2. Add points to show the actual measurements
    # We can color the points by group to make them stand out
    geom_point(aes(color = group), size = 3) +

    # 3. Improve the labels and title for clarity
    labs(
        title = "Change in Extra Sleep for Each Patient",
        subtitle = "Comparing Drug 1 vs. Drug 2",
        x = "Drug Group",
        y = "Increase in Hours of Sleep"
    ) +

    # 4. Use a clean theme
    theme_minimal() +

    # 5. Optional: Customize the legend and x-axis labels
    scale_color_discrete(name = "Drug Group", labels = c("Drug 1", "Drug 2")) +
    scale_x_discrete(labels = c("Drug 1", "Drug 2"))

# Note:
# H0: there is no difference in mean sleep increase  between drug 1 and drug 2 (no difference hypothesis) <- H0 is always this

# alternative hypothesis
# Ha: There is a difference. (two-sided hypothesis, I know there is some differences, but I don't know which one is better.)

# p-value = 0.002833: This is the most important number. It is much less than our alpha of 0.05.
# Conclusion: We reject the null hypothesis. There is a statistically significant difference in the mean increase in sleep between the two drugs.
t.test(sleep_wide$group1, sleep_wide$group2, paired = TRUE) # two-sided by default

# Ha: drug 1 helps to sleep more. (one-sided hypothesis, I hope drug 1 perform better than drug 2) 
t.test(sleep_wide$group1, sleep_wide$group2, paired = TRUE, alternative="greater") # two-sided by default
# (only p-value change) p-value=0.9986 -> failed to reject null hypothesis
# it's seems drug 1 doesn't have different effect in increasing sleep hours compare to drug 2

# Ha: drug 2 helps to sleep more. (one-sided hypothesis, I hope drug 2 perform better than drug 1)
t.test(sleep_wide$group1, sleep_wide$group2, paired = TRUE, alternative="less") # two-sided by default
# p-value=0.001416 (less than 0.002833 - two-sided), is less than 0.05
# we reject the null hypothesis, there is a statistically significant difference in the mean increase in sleep between the two drugs.

