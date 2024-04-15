#Elijah Chou
#Lab 11
#Lab Practice 10

###Setting up Environment###

install.packages("Rmisc") 
library(Rmisc)

#Set up working directory
setwd("C:/Users/ecool/Desktop/R Code/Data Sets")
source("TestingFunction.R")

#Import data
yrbss2013 <- read.csv("C:/Users/ecool/Desktop/R Code/Data Sets/yrbss2013.csv")

###Answer Code###

#Question 8a: Population distribution of height_m
hist(yrbss2013$height_m) #approximately normal
summarySE(data = yrbss2013, measurevar = "height_m")
#Mean is 1.68729,  SD is 0.1029791

#Question 8b: They are satisfied because the distribution seems approximately normal

#Question 8c:
# We are testing  H0: mean = 1.687 versus  Ha: mean != 1.687.  In the hypothesis test, we run the risk of committing a 
# (***Type I***/Type II)error because in reality the null hypothesis is actually (***true***/false).  
# The targeted(***Type I***/Type II)error rate is ***5%*** and the targeted confidence interval coverage is ***95%***. 
# Because sampling distribution assumptions (***are***/are not)satisfied,  we expect the observed Type I 
# error rate and confidence interval coverage from simulation results to (***equal***/not equal)the targeted levels.

#Question 8d:
sim1 <-inference.means(variable = yrbss2013$height_m,
                       sample.size = 20,
                       alpha = 0.05,
                       num.reps = 100)
sim1
#Histogram of sample means
hist(sim1$samp.est, main = "Sample Means")  #approx. normal
#Histogram of test statistics
hist(sim1$test.stat, main = "t test statistics")  #approx. normal
#Histogram of p-values
hist(sim1$p.val, main = "p-values")  #approx. uniformly distributed
#Frequency table of decisions
table(sim1$decision)  # 6% of samples commit Type 1 error

#Question 8e: 
plot.ci(results = sim1, true.val = 1.687)
table(sim1$capture)
# 94% of the CIs capture the true pop. mean, appear to be about same length, randomly scattered about the mean, provide reasonable bounds

#Question 8f: no, not everyone gets the same, and the tests are working as they should be

#Question 9a: Population distribution of days_smoke
hist(yrbss2013$days_smoke) #right-skewed
summarySE(data = yrbss2013, measurevar = "days_smoke")
#Mean is 1.496227,  SD is 5.7

#Question 9b: They are not satisfied for valid inference because the underlying population does not have a normal distribution

#Question 9c:
# We are testing  H0: mean = 1.496 versus  Ha: mean != 1.496.  In the hypothesis test, we run the risk of committing a 
# (***Type I***/Type II)error because in reality the null hypothesis is actually (***true***/false).  
# The targeted(***Type I***/Type II)error rate is ***5%*** and the targeted confidence interval coverage is ***95%***. 
# Because sampling distribution assumptions (are/***are not***)satisfied,  we expect the observed Type I 
# error rate and confidence interval coverage from simulation results to (equal/***not equal***)the targeted levels.

#Question 9d:
sim2 <-inference.means(variable = yrbss2013$days_smoke,
                       sample.size = 20,
                       alpha = 0.05,
                       num.reps = 100)
sim2
#Histogram of sample means
hist(sim2$samp.est, main = "Sample Means")  #right-skewed
#Histogram of test statistics
hist(sim2$test.stat, main = "t test statistics")  #left-skewed
#Histogram of p-values
hist(sim2$p.val, main = "p-values")  # bimodal
#Frequency table of decisions
table(sim2$decision)  # 36% of samples commit Type 1 error

#Question 9e: 
plot.ci(results = sim2, true.val = 1.496)
table(sim2$capture)
# 64% of the CIs capture the true pop. mean, vary in length, most scattered along the right of mean, does not provide reasonable bounds

#Question 9f: no, not everyone gets the same, and the tests aren't working as they should be 

#Question 10: in spreadsheet
# height_m n=20 
sim1long <-inference.means(variable = yrbss2013$height_m, sample.size = 20, alpha = 0.05,num.reps = 10000)
#Frequency table of captures
table(sim1long$capture)
#Frequency table of decisions
table(sim1long$decision)
528/10000*100  #calculate observed type 1 error rate

# height_m n=100
sim2long <-inference.means(variable = yrbss2013$height_m, sample.size = 100, alpha = 0.05,num.reps = 10000)
#Frequency table of captures
table(sim2long$capture)
#Frequency table of decisions
table(sim2long$decision)
497/10000*100  #calculate observed type 1 error rate

#days_smoke n=20
sim3long <-inference.means(variable = yrbss2013$days_smoke, sample.size = 20, alpha = 0.05,num.reps = 10000)
#Frequency table of captures
table(sim3long$capture)
#Frequency table of decisions
table(sim3long$decision)
hist(sim3long$samp.est, main = "Sample Means")
3189/10000*100  #calculate observed type 1 error rate

#days_smoke n=50
sim4long <-inference.means(variable = yrbss2013$days_smoke, sample.size = 50, alpha = 0.05,num.reps = 10000)
#Frequency table of captures
table(sim4long$capture)
#Frequency table of decisions
table(sim4long$decision)
hist(sim4long$samp.est, main = "Sample Means")
1309/10000*100  #calculate observed type 1 error rate

#days_smoke n=100
sim5long <-inference.means(variable = yrbss2013$days_smoke, sample.size = 100, alpha = 0.05,num.reps = 10000)
#Frequency table of captures
table(sim5long$capture)
#Frequency table of decisions
table(sim5long$decision)
hist(sim5long$samp.est, main = "Sample Means")
894/10000*100  #calculate observed type 1 error rate

#Question 11a: yes, it does lower error rate and improve CI inclusion rate, most likely because the sampling distribition becomes
# more normal as the sample size increases

#Question 11b: We don't always have a a valid inference despite having satisfied assumptions.


# height_m n=20 long run performance
sim1long <-inference.means(variable = yrbss2013$height_m, sample.size = 20, alpha = 0.05,num.reps = 10000) #runs inference.means w/ n=20
table(sim1long$capture)#creates frequency table of captures
# height_m n=100 long run performance
sim2long <-inference.means(variable = yrbss2013$height_m, sample.size = 100, alpha = 0.05,num.reps = 10000)
table(sim2long$capture) #creates frequency table of captures

