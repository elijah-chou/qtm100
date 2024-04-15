#Elijah Chou
#Lab 11
#Lab Practice 5

###Setting up Environment###

install.packages("Rmisc") 
library(Rmisc)

#Set up working directory
setwd("C:/Users/ecool/Desktop/R Code")

#Import data
fruitfly <- read.csv("C:/Users/ecool/Desktop/R Code/Data Sets/fruitfly.csv")

####Inspecting Dataset####
summary(fruitfly)

####Questions####

#Question 1a) Comparing distribution of lifespan
boxplot(fruitfly$lifespan ~ fruitfly$type)

#Question 1b) Return mean and std dev. of lowest avg. lifespan group (5 is lowest)
summarySE(data = fruitfly, measurevar = "lifespan", groupvars = "type")
#Ans: Avg: 38.72, SD: 12.10207

#Question 2a) Complete table (in Google Drive)
pnorm(q=30, mean = 38.7, sd = 12.1)   #returns probability of lifespan <=30 days (0.24)
pnorm(q=50, mean = 38.7, sd = 12.1) - pnorm(q=30, mean = 38.7, sd = 12.1)  #returns probability of lifespan 30-50 days (0.59)
pnorm(q=70, mean = 38.7, sd = 12.1) - pnorm(q=50, mean = 38.7, sd = 12.1)  #returns probability of lifespan 50-70 days (0.17)
1 - pnorm(q=70, mean = 38.7, sd = 12.1) #returns probability of lifespan >70 days (0.0048)

#Question 2b) It would most likely be from the 8 virgins because there is a higher probability of flies from that group to be 50+ days old

#Question 3) Finding theoretical and observed quantiles
fruitflysubset<-subset(fruitfly,type==5)  #creates subset of flies that are provided with 8 virgin female flies

#10th Quartile
qnorm(p = 0.10, mean = 38.7, sd = 12.1) #theoretical (23.2)
quantile(fruitflysubset$lifespan, probs = 0.10) #observed (21.8)

#25th Quartile
qnorm(p = 0.25, mean = 38.7, sd = 12.1) #theoretical (30.5)
quantile(fruitflysubset$lifespan, probs = 0.25) #observed (32)

#50th Quartile
qnorm(p = 0.50, mean = 38.7, sd = 12.1) #theoretical (38.7)
quantile(fruitflysubset$lifespan, probs = 0.50) #observed (40)

#75th Quartile
qnorm(p = 0.75, mean = 38.7, sd = 12.1) #theoretical (46.9)
quantile(fruitflysubset$lifespan, probs = 0.75) #observed (47)

#90th Quartile
qnorm(p = 0.90, mean = 38.7, sd = 12.1) #theoretical (54.2)
quantile(fruitflysubset$lifespan, probs = 0.90) #observed (54)

#Question 4a) Calculate proportion of flies in type 5 that survived at least 50 days
sum(fruitflysubset$lifespan >= 50) / length(fruitflysubset$lifespan) #Returns 0.2

#Question 4b) Complete table using binomial distribution commands
Fruitfly9 <- data.frame(x = 0:10, prob = dbinom(x = 0:10, size = 10, prob = 0.2)) #stores probabilities in table
Fruitfly9 #inspects data table

#Question 4c) Preg: 0.13, Virgin: 0.09, the 8 newly pregnant ones have higher chance

#Question 4d) For each group, how many flies is most likely going to survive
#Newly Preg: 8 flies
#Virgin: 8 flies

#Virgin is more likely to have more flies to survive at least 50 days

#Question 4e) Which group is more likely to have at least 5 flies survive past 50 days?

sum(Fruitfly9$prob[Fruitfly9$x >= 5]) #calculates probability of at least 5 flies from the 8 virgins group to survive at least 50 days
#Returns 0.9936

sum(dbinom(x=5:10, size = 10, prob = 0.76)) # calculates probability of at least 5 flies from the 8 newly preg. group to survive at least 50 days
#Returns 0.9839

#Answer: The 8 Virgins group.

dbinom(x=6, size = 10, prob = 0.2)
