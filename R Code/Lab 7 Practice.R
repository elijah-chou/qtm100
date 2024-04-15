#Elijah Chou
#Lab 11
#Lab Practice 7

###Setting up Environment###

install.packages("Rmisc") 
library(Rmisc)

#Set up working directory
setwd("C:/Users/ecool/Desktop/R Code/Data Sets")

#Import data
gardasil <- read.table("gardasil.txt", header = T)

#Question 1:
table(gardasil$LocationType)
prop.table(table(gardasil$LocationType))

#Ans: Suburban: 68.15%, Urban: 31.85%

#Question 2a) one sample z-test

#Question 2b) Ho: p=0.7, Ha: p != 0.26

#Question 2c) 
prop.test(963,963+450,p=0.7,correct=F)
sqrt(2.2957)

#p-value = 0.1297
#z-score = 1.515157
#confidence interval = (0.656773, 0.705300)

#Question 2d) Because p-val = 0.1297 > 0.05, we cannot reject the null hypothesis, which means
#that the data set for 

#Question 3a) AgeGroup

#Question 3b) Find proportion of women under 18
prop.table(table(gardasil$AgeGroup))
#Ans: 0.49611

#Question 3c) Ho: p = 0.53, Ha: p != 0.53

#Question 3d) 
sqrt(6.5159)
prop.test(table(gardasil$AgeGroup), p = 0.53, correct = F)
#p-value = 0.01069
#z-statistic = 2.552626
#confidence interval = (0.4700839, 0.5221523)

#Question 3e) Because our value falls outside of the confidence interval, we can conclude that 
# the observed difference in statistically different, and that we reject the null hypothesis

#Question 4) 
2*pnorm(-2.552626)
#We are calculating the probability of a sample mean falling outside of the z-score by finding
#the probability of the lower tail and multiplying it by two to also account for the upper tail,
# because this test is two-tailed.

#Question 5) The Johns Hopkins team should be worried that their sample is inappropriate 
#because their sample's AgeGroup indicates that the the proportion of women under the age of 18
#was 49%, which is statistically significantly less than the true population proportion of 53%.
