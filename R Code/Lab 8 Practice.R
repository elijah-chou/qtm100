#Elijah Chou
#Lab 11
#Lab Practice 8

###Setting up Environment###

install.packages("Rmisc") 
library(Rmisc)

#Set up working directory
setwd("C:/Users/ecool/Desktop/R Code/Data Sets")

#Import data
pharynx <- read.csv("C:/Users/ecool/Desktop/R Code/Data Sets/pharynx.csv")

##Question 1a: Create new variable about whether each observation survived >=500 days
pharynx$survival500 <- factor(NA, levels=c("Yes", "No"))  #creates the new variable
pharynx$survival500[pharynx$TIME>=500]<-"Yes"  #puts observations >=500 in TIME to "Yes"
pharynx$survival500[pharynx$TIME<500]<-"No"  #puts observations <500 in TIME to "No"

summary(pharynx$survival500)

##Question 1b: We could perform the 2 sample z test or a chi-squared test

#Ho: p1 = p2
#Ha: p1 != p2
#2 sample z test -> determines whether or not there is a significant difference between 2 proportions
#chi-squared test -> determines whether there is strong evidence of association between two variables

##Question 1c: Chi-squared test
Treatment_Efficacy_Table<-table(pharynx$TX, pharynx$survival500)  #creates new contingency table comparing treatment and survival past 500 days
addmargins(Treatment_Efficacy_Table) #adds margins to table to show row/column totals
Treat.Eff.test<-chisq.test(Treatment_Efficacy_Table,correct=F)  #stores chi-squared test results in variable
Treat.Eff.test  #displays results

#According to the p-value, we fail to reject the null hypothesis that p1=p2, and this also means that there is no strong
#evidence that any of the treatment groups are more effective than the other.

##Question 1d: This analysis only tells us whether there is strong evidence of association, but not about the strength 
#of the actual association.

##Question 2a: All expected cell counts should be at least 5

##Question 2b: Yes, it is satisfied, because each of the expected cell counts exceed 5
Treat.Eff.test$expected  #displays expected cell counts

##Question 2c: 
TumorStage_Survival_Table<-table(pharynx$T_STAGE, pharynx$survival500)  #creates new contingency table for tumor stage and survival
Tumor.Survival.test<-chisq.test(TumorStage_Survival_Table,correct=F)  #stores chi-squared test results in variable
Tumor.Survival.test$expected  #displays results

#Answer: the assumptions are not satisfied, because two of the expected cell counts are below 5. An alternative test
#that could be conducted is Fisher's Exact test

##Question 3: 
#a) ??2 = 1, df = 1, p = 0.3173105 
pchisq(1, df=1, lower.tail=F)
#b) ??2 = 3, df = 1, p = .08326452
pchisq(3, df=1, lower.tail=F)
#c) ??2 = 5, df = 1, p = .02534732, is significant
pchisq(5, df=1, lower.tail=F)
#d) ??2 = 1, df = 2, p = .6065307
pchisq(1, df=2, lower.tail=F)
#e) ??2 = 3, df = 2, p = .2231302
pchisq(3, df=2, lower.tail=F)
#f) ??2 = 5, df = 2, p = .082085
pchisq(5, df=2, lower.tail=F)

#As test statistic increase, p-value decreases; larger test statistics provide more evidence against Ho
#Same test statistic with different df can result in different conclusion for specified level of significance
