#Load the ks2 dataset. Remove any observations that have any ungraded test 
##scores.

library(tidyverse)

ks2 <- read.csv("ks2.csv")

ks2 <- ks2[complete.cases(ks2),]

##Task 2- Calculate the means and standard deviations of each of the three subjects.
##Write a brief description of the insights that can be drawn from these values.


with(ks2, c(mean(reading),sd(reading)))

with(ks2, c(mean(maths),sd(maths)))

with(ks2, c(mean(gps),sd(gps)))

# The mean mark of GPS (105.76) is higher than reading (103.56) and maths (104.70).
# This indicates that on average students performed better in GPS than reading or maths.
# The standard deviation of reading (5.42) is higher than maths (5.10) and GPS (4.61).
# This indicates that marks are more spread out from the mean for reading than
# they are for maths and GPS.

##Task 3- Conduct a t-test to see if the means of each of the three subjects' marks
##are statistically different from 100 at the 95% confidence level. Identify three
##ways that suggest statistically significant or insignifcant differences.

t.test(ks2$reading, alternative = "two.sided", mu=100)

t.test(ks2$maths, alternative = "two.sided", mu=100)

t.test(ks2$gps, alternative = "two.sided", mu=100)

##In the t-test results all of the p-values are lesser than 0.05, and since the 
##smaller the p-value the stronger the evidence against the null hypothesis, this 
##shows a statistical difference. Secondly, none of the confidence intervals have 
##the value of 100 in them at a 95% confidence level, showing that they are 
##statistically different from 100. Finally the t-value is greater than 2, which 
##acts as a rule of thumb when calculating at a 95% confidence level as in the 
##t-score tables at 2 standard deviations 95.4% of the data falls within the interval
##Additionally, none of the mean values for the variables were equal to 100.

##Task 4- Conduct a t-test to see if the mean of the average score of the three
##tests is statistically less than 105 at the 99% confidence level. Interpret the 
##the results


t.test(ks2$avg_all, alternative = "less", mu=105, conf.level = 0.99)

##Mean is statistically less than 105 at the 99% confidence level.

##Task 5- The following questions will look at students' English abilities generally
##a. Create a new variable called english, which consists of the average of the
##reading and grammar, punctuation, and spelling variables.

ks2$english <- with(ks2, (reading + gps)/2)

##b. Conduct a t-test to see if the mean of the average score of the new English
##variable is statistically less than 105 at the 99.9% confidence level. Interpret
##the results.

t.test(ks2$english, alternative = "less", mu=105, conf.level = 0.999)

##The mean is statistically lesser than 105 at a 99.9% confidence level.

##c. Conduct a t-test to see if the mean of the average score of the new English 
##variable is statistically different from 105 at the 99.9% confidence level.
##Interpret the results.

t.test(ks2$english, alternative = "two.sided", mu=105, conf.level = 0.999)

##The mean is not statistically different from 105 at the 99.9% confidence level.
##This is due to the confidence interval at 99.9% including 105 within it.

##d.Is there any difference in the interpretation between the two above tests?
##Are there any differences in the results? Why?

##The first t test is statistically significant but the second one is not,
##despite having the same mu value and the same values in the variable.
##The second t test loses its statistical significance due to being a
##two-sided t test whilst the first t test is a one-sided t test. This means
##that the first test has a statistically significant tail consisting of
## 0.001 of the total area under the normal probability distribution,
##whilst the second test has two statistically significant tails consisting
##a total of 0.001 of the total area under the distribution, or 0.0005 under
##the distribution for each individual tail. Because of this, the confidence
##interval of the two-sided t test is wider whilst the one for the one-sided
##t test is smaller, meaning it is harder for the two-sided t test to
##be statistically significant, showing why it is not significant when
##the one-sided t test is significant.

##Task 6- You are tasked with investing the performance of students who passed 
##in mathematics those who did not. For the following tests, use a 95% confidence
##level. 
##a. Create a binary variable that has two categories: those who passed mathematics
##(100 <= mark) and those who failed mathematics (mark<100).


ks2 <- ks2 %>%
  mutate(maths_pass = cut(maths ,
                            breaks=c(80,100,120),
                            labels=c("Fail", "Pass"),
                          right=FALSE,
                          include.lowest=TRUE))

##b. Conduct a proportion test to see if the proportion of students that fail 
##mathematics is 10% or greater. Interpret the results


prop.test(table(ks2$maths_pass), alternative = "less", p=0.10, conf.level = 0.95)

##The results from the test are statistically insignificant as we fail to reject
##the null hypothesis of 10% or more students failing mathematics, this is due to
##the p-value of the test being 0.3733 which is much greater than the critical
##p-value of 0.05 for 95% confidence levels. And when the p-value is greater than the
##critical p-value, there is insufficient evidence present to reject the null hypothesis.

##c.Conduct a t-test on the group who fail mathematics to see if they, on average,
##have marks for English statistically less than 100. Interpret the results.


t.test(ks2$english[ks2$maths_pass == "Fail"], alternative = "less", mu=100, conf.level = 0.95)

##The results are statistically significant as the mean of x is statistically lower
##than 100, implying that at a 95% confidence level we can say that students who
##fail maths also fail English.

##d.Conducting a t-test on the group who pass mathematics to see if they, on average,
##have marks for grammar, spelling, and punctuation statistically greater than 105.
##Interpret the results.


t.test(ks2$gps[ks2$maths_pass == "Pass"], alternative = "greater", mu=105, conf.level = 0.95)

##the results are statistically significant as the mean is statistically greater 
##than 105, implying that students who passed mathematics usually score above 
##105 in GPS.

##Task 7- Now it is worth investigating how students who fail at least one subject
##perform.
##a. Create a binary variable that has two categories: those who passed all three
##subjects (all marks greater than or equal to 100) and those who failed at least 
##one subject(one or more marks less than 100)


ks2 <- ks2 %>%
  mutate(pass_fail=ifelse(reading<100|maths<100|gps<100, "Fail", "Pass"))

##b. It can be hypothesised that the group of students who failed will have a mean
##of all the test marks significantly below the pass mark of 100. Test this and
##interpret the findings with respect to the statistical and practical significance
##of the test.


t.test(ks2$avg_all[ks2$pass_fail == "Fail"], alternative = "less", mu=100, conf.level = 0.95)

# Statistically significant, indicating that students who fail one or more test
# have an overall average mark less than 100, on average. This is expected. However,
# the practical significance is worth drawing attention to; the mean of this group
# is 98.88% and the upper CI is 99.23%, indicating that the mean mark of this
# group is only just below the pass mark, suggesting that whilst the difference
# is statistically significant, there is not much practical significance as
# students who fail only seem to fail relatively narrowly on average.

##Task 8- Imagine you are part of a team work working within the Department for 
##Education, tasked with investigating this sample to produce recommendations for 
##policymakers.

##a. Normalise the variable that contains the average of all three marks by setting
##the lowest mark (80) to 0, the highest mark (120) to 100, and the minimum pass
##mark (100) to 50. Justify why it might be useful to normalise these marks to this
##scale for non-specialist policymakers.

ks2$normal <- with(ks2, ((avg_all-80)/(120-80))*100)

# This scale is useful since it is now percentage based, with 0 representing
# a 0% score (excluding ungraded marks), 50 representing a 50% score,
# and 100 representing a 100% score. This makes it more easy to interpret than
# the previous 80-120 scale.

##b.Construct a categorical variable that consists of 5 categories: 0.49.99 (Fail),
##50-59.99 (Pass), 60-69.99(Merit), 70-79.99(Distinction), and 80+ (Distinction+)


ks2 <- ks2 %>%
  mutate(grades = cut(normal , breaks=c(0,50,60,70,80,100),
                          labels=c("Fail", "Pass","Merit","Distinction","Distinction+"),
                          right=FALSE,
                          include.lowest=TRUE))

##c. Answer the following questions but write your answers as if intended for a 
##non-specialist policy making with no knowledge of statistics:
##i. Are the averages of the Pass, Merit, and Distinction groups different from
##their middle marks (55,65, and 75, respectively)? If so, which direction?

##Pass

t.test(ks2$normal[ks2$grades == "Pass"], alternative = "two.sided", 
       mu=55, conf.level = 0.95)

##No there is no difference, the average of this group is around 55%.

##Merit

t.test(ks2$normal[ks2$grades == "Merit"], alternative = "two.sided", 
       mu=65, conf.level = 0.95)

##Yes there is a difference, the average is lower that 65, being 64%.

##Distinction

t.test(ks2$normal[ks2$grades == "Distinction"], alternative = "two.sided", 
       mu=75, conf.level = 0.95)

##Yes, there is a difference, the average is 73%, which is lower than 75%

##ii. Is the average mark of the Distinction+ group lower than the maximum mark
##of the group?


t.test(ks2$normal[ks2$grades == "Distinction+"], alternative = "less", 
       mu=max(ks2$normal), conf.level = 0.95)

##Yes, the average mark of the Distinction+ group is 83% which is lower than the 
##maximum of 91%.

##iii. Is the mean mark of the Fail group higher than the median mark of the group?
##Which skew does this indicate in the distribution?

t.test(ks2$normal[ks2$grades == "Fail"], alternative = "two.sided", 
       mu=median(ks2$normal[ks2$grades == "Fail"]), conf.level = 0.95)

##No, the mean mark is 41% which is lower than the median of 45%, indicating a negative
##skew.