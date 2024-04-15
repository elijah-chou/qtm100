#Elijah Chou
#Lab 11
#Lab Practice 9

###Setting up Environment###

install.packages("Rmisc") 
library(Rmisc)

#Set up working directory
setwd("C:/Users/ecool/Desktop/R Code/Data Sets")

#Import data
lead <- read.csv("C:/Users/ecool/Desktop/R Code/Data Sets/lead.csv")

###Answering Questions###

#Question 1a: Mean IQ & SD
summarySE(data = lead, measurevar = "Iqf")

#mean: 91.08065, SD = 14.40393

#Question 1b: Is IQ normally distributed?
hist(lead$Iqf)

#Approximately yes, because sample size is >=30

#Question 2a: one-sample t-test

#Question 2b: Ho: The average IQs of the two studies are not significantly different; Ha: IQs are different

#Question 2c: test statistic + p-value
t.test(lead$Iqf, mu = 85)
#test statistic = 4.7009
#p-value = 6.825e-06

#Question 2d: CI = (88.52022, 93.64107)

#Question 2e: We reject the null hypothesis because the value tested in the null hypothesis lies outside of the CI

#Question 3a: These are not independent (are instead paired) because they are the same children, but just at different points of time in history

#Question 3b: Mean + SD of Ld72-Ld73
lead$diff72.73 <- lead$Ld72 - lead$Ld73
summarySE(data = lead, measurevar = "diff72.73", na.rm = T)

#121 differences are not missing
#Mean: 3.371901, SD: 9.853199

#Question 3c: Describe distribution shape
hist(lead$diff72.73)
#Approximately normal

#Question 3d: On average, blood lead levels decreased, indicates less exposure over time

#Question 4a: Paired t-test

#Question 4b: Difference in blood lead levels between 72 and 73

#Question 4c: Ho: average difference = 0; Ha: average difference != 0

#Question 4d: Yes, data is randomized and has an approximately normal distribution, and paired t-tests can run paired variables

#Question 4e: test statistic
t.test(lead$diff72.73)
#T = 3.7644

#Question 4f: p-value = 0.0002599

#Question 4g: Reject null at alpha= 0.05

#Question 4h: Estimate parameter of interest + CI
# 3.371901, with CI of (1.598387, 5.145414)

#Question 4i: The blood lead levels of the children changed significantly between 1972 and 1973.

#Question 5: two-sample t-test of diff in lead children vs. diff in control children
t.test(lead$diff72.73~lead$Group, var.equal=T)
#There is evidence that the change in blood lead levels differs by group

#Question 6a: 
boxplot(lead$Iqf~lead$Group)
#I produced a side-by-side boxplot, and there may be a difference in IQ based on group

#Question 6b: 
summarySE(data = lead, measurevar = "Iqf", groupvars = "Group", na.rm = T)
#control --> 78 ppl, mean = 92.88462, SD = 15.34451
#lead --> 46 ppl, mean = 88.02174, SD = 12.20654

#Question 6c: describe distribution of groups in IQ
hist(lead$Iqf[lead$Group=="lead"]) #approximately normal, with a little bit of left-skewness
hist(lead$Iqf[lead$Group=="control"]) #approximately normal

#Question 7a: two-sample t-test

#Question 7b: Difference between control mean IQ and lead mean IQ

#Question 7c: Ho: difference = 0, Ha: difference!= 0

#Question 7d: Yes, because the data are indepedent within and between each other, and both samples are approx. normal

#Question 7e: Test statistic
t.test(lead$Iqf~lead$Group, var.equal = F)
#T = 1.9439

#Question 7f: p-value = 0.05442

#Question 7g: Fail to reject null

#Question 7h: estimate parameter + CI
(-0.09391511+9.81966762)/2
#Estimate = 4.862876, CI = (-0.09391511, 9.81966762)

#Question 7i: There is no significant difference between the IQs of lead vs. control children.