#Elijah Chou
#Lab 11
#Lab Practice 6

###Setting up Environment###

install.packages("Rmisc") 
library(Rmisc)

#Set up working directory
setwd("C:/Users/ecool/Desktop/R Code")

#Import data
yrbss2013 <- read.csv("C:/Users/ecool/Desktop/R Code/Data Sets/yrbss2013.csv")

####Questions####

#Question 1A: Create new height variable in cm instead of m
yrbss2013$height_cm <- yrbss2013$height_m*100

#Question 1B: Report mean and std dev. of height_cm, plot & check for normality
summarySE(data = yrbss2013, measurevar = "height_cm") #Returns mean = 168.729 & std dev. = 10.29791
hist(yrbss2013$height_cm)  #prints histogram of data, seems close to normal distribution

#Question 2A: Compare height values against "sad" variable
summarySE(data = yrbss2013, measurevar = "height_cm", groupvars = "sad")
#yes sad: mean = 166.4107, sd = 10.23188
#not sad: mean = 169.6967, sd = 10.17103

#Question 2B: Separate the heights into "yes sad" and "no sad" groups & check for normality
yrbss2013sad <- subset(yrbss2013, sad=="yes")   #creates subset of people who are sad
yrbss2013notsad <- subset(yrbss2013, sad=="no")  #creates subset of people who are not sad
hist(yrbss2013sad$height_cm)   #Makes histogram of subset "sad"
hist(yrbss2013notsad$height_cm)  #Makes histogram of subset "not sad"

#Conclude that the people who are sad have a distribution that is right-skewed,
#while it is normal for those who are not sad.

#Question 3A: Loop for 100 samples of size 10, report mean and sd + check distribution
sample100_means10 <- rep(NA, 100) #Creates an empty vector of 100 lines
for(i in 1:100){
  samp <- sample(yrbss2013sad$height_cm, 10) #Creates a vector with 10 samples from the "height_cm" vector
  sample100_means10[i] <- mean(samp) #Adds the mean of samp to the sample100_means10 vector
}
summary(sample100_means10)  #returns mean
sd(sample100_means10)  #returns sd
hist(sample100_means10, breaks = 25)  #Checking distribution

#Question 3B: Loop for 500 samples of size 10, report mean and sd + check distribution
sample500_means10 <- rep(NA, 500) #Creates an empty vector of 500 lines
for(i in 1:500){
  samp <- sample(yrbss2013sad$height_cm, 10) #Creates a vector with 10 samples from the "height_cm" vector
  sample500_means10[i] <- mean(samp) #Adds the mean of samp to the sample500_means10 vector
}
summary(sample500_means10)  #returns mean
sd(sample500_means10)  #returns sd
hist(sample500_means10, breaks = 25)  #Checking distribution

#Question 3C: Loop for 1000 samples of size 10, report mean and sd + check distribution
sample1000_means10 <- rep(NA, 1000) #Creates an empty vector of 1000 lines
for(i in 1:1000){
  samp <- sample(yrbss2013sad$height_cm, 10) #Creates a vector with 10 samples from the "height_cm" vector
  sample1000_means10[i] <- mean(samp) #Adds the mean of samp to the sample1000_means10 vector
}
summary(sample1000_means10)  #returns mean
sd(sample1000_means10)  #returns sd 
hist(sample1000_means10, breaks = 25)  #Checking distribution

#Question 3D: Loop for 5000 samples of size 10, report mean and sd + check distribution
sample5000_means10 <- rep(NA, 5000) #Creates an empty vector of 5000 lines
for(i in 1:5000){
  samp <- sample(yrbss2013sad$height_cm, 10) #Creates a vector with 10 samples from the "height_cm" vector
  sample5000_means10[i] <- mean(samp) #Adds the mean of samp to the sample5000_means10 vector
}
summary(sample5000_means10)  #returns mean
sd(sample5000_means10)  #returns sd 
hist(sample5000_means10, breaks = 25)  #Checking distribution

#Question 3E: When the number of samples increases, the more normal the distribution of the samples are
#Question 4A: Loop for 5000 samples of size 100, report mean and sd + check distribution
sample5000_means100 <- rep(NA, 5000) #Creates an empty vector of 5000 lines
for(i in 1:5000){
  samp <- sample(yrbss2013sad$height_cm, 100) #Creates a vector with 100 samples from the "height_cm" vector
  sample5000_means100[i] <- mean(samp) #Adds the mean of samp to the sample5000_means100 vector
}
summary(sample5000_means100)  #returns mean
sd(sample5000_means100)  #returns sd 
hist(sample5000_means100, breaks = 25)  #Checking distribution

#Question 4B: Loop for 5000 samples of size 200, report mean and sd + check distribution
sample5000_means200 <- rep(NA, 5000) #Creates an empty vector of 5000 lines
for(i in 1:5000){
  samp <- sample(yrbss2013sad$height_cm, 200) #Creates a vector with 200 samples from the "height_cm" vector
  sample5000_means200[i] <- mean(samp) #Adds the mean of samp to the sample5000_means200 vector
}
summary(sample5000_means200)  #returns mean
sd(sample5000_means200)  #returns sd 
hist(sample5000_means200, breaks = 25)  #Checking distribution

#Question 4C: Loop for 5000 samples of size 500, report mean and sd + check distribution
sample5000_means500 <- rep(NA, 5000) #Creates an empty vector of 5000 lines
for(i in 1:5000){
  samp <- sample(yrbss2013sad$height_cm, 500) #Creates a vector with 500 samples from the "height_cm" vector
  sample5000_means500[i] <- mean(samp) #Adds the mean of samp to the sample5000_means100 vector
}
summary(sample5000_means500)  #returns mean
sd(sample5000_means500)  #returns sd 
hist(sample5000_means500, breaks = 25)  #Checking distribution

#Question 4D: Increasing the size draw causes the standard deviation of the means to decrease

#Question 5A: Mean, Distribution, and Normality of days_drink
mean(yrbss2013$days_drink) #retunrs 1.45402
hist(yrbss2013$days_drink) #prints histogram, is very right skewed
#Very unlike height_cm

#Question 5B: Loop for 500 samples of size 10, report mean
sample500_means10 <- rep(NA, 500) #Creates an empty vector of 500 lines
for(i in 1:500){
  samp <- sample(yrbss2013$days_drink, 10) #Creates a vector with 10 samples from the "days_drink" vector
  sample500_means10[i] <- mean(samp) #Adds the mean of samp to the sample500_means10 vector
}
mean(sample500_means10) #returns mean

#Question 5C: Loop for 500 samples of size 50, report mean
sample500_means50 <- rep(NA, 500) #Creates an empty vector of 500 lines
for(i in 1:500){
  samp <- sample(yrbss2013$days_drink, 50) #Creates a vector with 50 samples from the "days_drink" vector
  sample500_means50[i] <- mean(samp) #Adds the mean of samp to the sample500_means50 vector
}
mean(sample500_means50) #returns mean

#Question 5D: Loop for 5000 samples of size 10, report mean
sample5000_means10 <- rep(NA, 5000) #Creates an empty vector of 5000 lines
for(i in 1:5000){
  samp <- sample(yrbss2013$days_drink, 10) #Creates a vector with 10 samples from the "days_drink" vector
  sample5000_means10[i] <- mean(samp) #Adds the mean of samp to the sample5000_means10 vector
}
mean(sample5000_means10) #returns mean

#Question 5E: Loop for 5000 samples of size 500, report mean
sample5000_means500 <- rep(NA, 5000) #Creates an empty vector of 5000 lines
for(i in 1:5000){
  samp <- sample(yrbss2013$days_drink, 500) #Creates a vector with 500 samples from the "days_drink" vector
  sample5000_means500[i] <- mean(samp) #Adds the mean of samp to the sample5000_means500 vector
}
mean(sample5000_means500) #returns mean

#Question 5F:
#Changing both sample size and number of samples affect the estimate of the mean

#Question 5G: 
hist(sample5000_means500)

#They are similar in the way that both are normal distributions


#Question 6A: Create subsets for "bullied" and "not bullied"
yrbss2013Bullied <- subset(yrbss2013, bullied == "yes")
yrbss2013NotBullied <- subset(yrbss2013, bullied == "no")

sampleBullied <- rep(NA, 5000) #Creates an empty vector of 5000 lines
for(i in 1:5000){
  samp <- sample(yrbss2013sad$weight_kg, 100) #Creates a vector with 100 samples from the "height_cm" vector
  sampleBullied[i] <- mean(samp) #Adds the mean of samp to the sampleBullied vector
}
mean(sampleBullied) #returns mean

sampleNotBullied <- rep(NA, 5000) #Creates an empty vector of 5000 lines
for(i in 1:5000){
  samp <- sample(yrbss2013sad$weight_kg, 100) #Creates a vector with 100 samples from the "height_cm" vector
  sampleNotBullied[i] <- mean(samp) #Adds the mean of samp to the sampleNotBullied vector
}
mean(sampleNotBullied) #returns mean


#Question 6B: Produce histograms
hist(yrbss2013Bullied$weight_kg)
hist(yrbss2013NotBullied$weight_kg)
