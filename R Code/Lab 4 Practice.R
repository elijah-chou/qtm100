#Elijah Chou
#Lab 11
#Lab Practice 4

###Setting up Environment###

install.packages("Rmisc") 
library(Rmisc)

#Set up working directory
setwd("C:/Users/ecool/Desktop/R Code")

#Import data
abductees <- read.csv("C:/Users/ecool/Desktop/R Code/Data Sets/abductees.csv")

####Inspecting Dataset####
summary(abductees)

####Questions####

#Question 1a) Recode "sex" variable
abductees$sex2 <- abductees$sex
abductees$sex2[abductees$sex == "Female"]<-"female"  #corrects "Female" values to "female"
table(abductees$sex2)    #check to see if change is reflected in table
abductees$sex2<-factor(abductees$sex2)  #removes extra "Female" category

#Question 1b) Look at boxplot of abduction claims, change out 60 and 100 to NA
boxplot(abductees$abdtimes)

abductees$abdtimes2<-abductees$abdtimes
abductees$abdtimes2==60  #shows that the 2nd entry has value of 60
abductees$abdtimes2==100   #shows that the 49th entry has value of 100
abductees$abdtimes2[2]<-NA   #Recodes the 2nd entry to NA
abductees$abdtimes2[49]<-NA    #Recodes the 49th entry to NA

abductees$abdtimes2>59   #Check to see if changes were made

#Question 2) Create new variable with age
abductees$age<-(90-abductees$yearbir)

#Question 3) Create new categorical variable "edulevel" using "educat"

#Creates new variable with three different categories
abductees$edulevel<-factor(NA,levels=c("high school","college education","more than college"))

#Labels observations with <= 12 years of education as "high school"
abductees$edulevel[abductees$educat<=12]<-"high school" 

#Labels observations with >= 13 & <=16 years of education as "college education"
abductees$edulevel[abductees$educat>=13 & abductees$educat<=16]<-"college education"  

#Labels observations with >= 17 years of education as "more than college"
abductees$edulevel[abductees$educat>=17]<-"more than college"   
table(abductees$educat)  #Check

#Question 4: Recode "marstat" so that there are only 2 categories

#Makes new variable that has two categories
abductees$marstatNew<-factor(NA,levels=c("Married","Other"))

#Puts "Married" into "Married" category
abductees$marstatNew[abductees$marstat=="Married"]<-"Married"

#Puts everything else into "Other" category
abductees$marstatNew[abductees$marstat=="Divorced" | abductees$marstat=="Separated" | abductees$marstat=="Single" | abductees$marstat=="NA"] <- "Other"

abductees$marstatNew #Check
table(abductees$marstatNew)
#Question 5a) Side-by-side boxplot of Age vs. "abdfeel"
boxplot(abductees$age ~ abductees$abdfeel)

#There does not seem to be any correlation between the two variables..
#The abdfeel categories are not lined up in ordinal fashion

#Question 5b) Reorder the variables appropriately
abductees$abdfeel<-factor(abductees$abdfeel, levels=c("Entirely negative","Mostly negative","About equally positive and negative","Mostly positive","Entirely positive"))

boxplot(abductees$age ~ abductees$abdfeel)
#Now I can identify that there seems to be a decreasing trend in age as the experience becomes more negative

#Question 6a) Average age of female respondants
summarySE(data = abductees, measurevar = "age", groupvars = "sex2", na.rm = TRUE)
#Average age is approximately 41.6 for female respondants

#Question 6b) Average number of abduction times of ppl with "Other" marital status
summarySE(data = abductees, measurevar = "abdtimes2", groupvars = "marstatNew", na.rm = TRUE)
# Approximately 3.3 times

#Question 6c) number of individuals with college education
table(abductees$edulevel) 
# 29 people

#Question 6d) General trend is somewhat positive... as the age increases, the feelings of the
#abductees also increase in positivity as well.
