#Elijah Chou
#Lab 11
#QTM 100 Final Project

###Setting up Environment###

install.packages("Rmisc") 
library(Rmisc)

#Set up working directory
setwd("C:/Users/ecool/Desktop/R Code")

#Import data
corona <- read.csv("C:/Users/ecool/Desktop/R Code/Data Sets/COV Dataset.csv")

####Recoding variables####

##Recoding the "death" variable
summary(corona$death) #check death variable, need to change categories from numbers to labels

corona$death2<-factor(NA, levels=c("Died", "Did Not Die"))  #create new categorical variable
corona$death2[corona$death==0]<-"Did Not Die"  #reassigns those who did not die to respective category
corona$death2[corona$death==1]<-"Died"  #reassigns those who did die to respective category

summary(corona$death2) #verify changes to variable

##Recoding the "gender" variable
summary(corona$gender)  #check gender variable, need to combine the two "male" groups into one, do same for "female" categories, and ignore 999 values

corona$gender2<-factor(NA, levels=c("male", "female"))  #create duplicate "gender" variable
corona$gender2[corona$gender=="MALE" | corona$gender=="male"]<-"male"  ##reassigns males to respective category
corona$gender2[corona$gender=="Female" | corona$gender=="female"]<-"female"  ##reassigns females respective category

summary(corona$gender2)  #verify changes to variable

##Recoding the "lag.onset.hosp" variable
summary(corona$lag.onset.hosp) #check lag.onset.hosp variable, noticed that there are two problems: a -1 value and many 999 values

corona$lag.onset.hosp2<-corona$lag.onset.hosp #create duplicate "lag.onset.hosp" variable
corona$lag.onset.hosp2[corona$lag.onset.hosp2<0]<-NA #recode all entries of this variable where delay is less than 0 to NA
corona$lag.onset.hosp2[corona$lag.onset.hosp2==999]<-NA #recode all "coded NAs" of 999 back to NA

summary(corona$lag.onset.hosp2) #verify changes to variable

##Creating the "Continental.Region" variable from "Country" variable
summary(corona$Country) #check country variable to determine which countries there are

corona$Country[corona$Country=="china"]<-"China"  #recode erroneous china values
corona$Continental.Region<-factor(NA, levels=c("Australia", "Asia", "Europe", "North America"))  #creates new categorical variable with the four continental regions

corona$Continental.Region[corona$Country=="Australia"]<-"Australia" #reassign values of the new Australia Continental Region
corona$Continental.Region[corona$Country=="Cambodia" | corona$Country=="China" | 
                            corona$Country=="Hong Kong" | corona$Country=="India" | 
                            corona$Country=="Japan" | corona$Country=="Malaysia" |
                            corona$Country=="Nepal" | corona$Country=="Russia" |
                            corona$Country=="Singapore" | corona$Country=="South Korea" |
                            corona$Country=="Sri Lanka" | corona$Country=="Taiwan" |
                            corona$Country=="Thailand" | corona$Country=="UAE" |
                            corona$Country=="Vietnam"]<-"Asia"  #reassign values of the new Asia Continental Region
corona$Continental.Region[corona$Country=="France" | corona$Country=="Germany" | 
                            corona$Country=="Italy" | corona$Country=="UK"]<-"Europe"   #reassign values of the new Europe Continental Region
corona$Continental.Region[corona$Country=="Canada" | corona$Country=="USA"]<-"North America"  #reassign values of the new North America Continental Region

summary(corona$Continental.Region) #verify successful creation of new variable


##Recoding the "age" variable
summary(corona$age) #check age variable, need to recode 999 to NA

corona$age2<-corona$age #create duplicate "age" variable
corona$age2[corona$age2==999]<-NA  #recode all "coded NAs" of 999 back to NA

summary(corona$age2) #verify changes to variable

###Creating Contigency Tables###

#age vs. death
summarySE(data = corona, measurevar = "age2", na.rm = T)  #overall age vs. death descriptive statistics
summarySE(data = corona, measurevar = "age2", groupvars = "death2", na.rm = T) #Provides descriptive statistics for age vs. death

summary(corona$death2)
#gender vs. death
table(corona$gender2) #contingency table of gender vs. death
201/(201+281) #proportion of women
281/(201+281) #proportion of men
prop.table(table(corona$gender2, corona$death2), margin = 2) #proportion table containing column proportions

#continental region vs. death
prop.table(table(corona$Continental.Region)) #proportion table for Continental.Region
prop.table(table(corona$Continental.Region, corona$death2), margin = 2)  #proportion table of Continental Region vs. death

###Tests for Statistical Analysis###

#Table 2: Two-sample t-test for gender vs. death
Gender_Death_Table<-table(corona$gender2, corona$death2) #makes contingency table
addmargins(Gender_Death_Table) #adds margins to contingency table
prop.table(Gender_Death_Table, margin=1) #creates table of proportions based on row
prop.test(Gender_Death_Table, correct=F) #runs the 2 sample z test
sqrt(2.4566)  #calculates z-score from chi-squared

#Table 3: ANOVA Test for lag.onset.hosp vs Continental.Region
anova.corona <- aov(corona$lag.onset.hosp2~corona$Continental.Region) #stores the ANOVA test results in anova.corona
summary(anova.corona) #prints summary of test results

#Table 4: Linear Regression for lag.onset.hosp vs. age
m1<-lm(corona$lag.onset.hosp2~corona$age2) #stores linear regression test in m1
summary(m1)  #prints out test results
confint(m1)  #provides the confidence intervals for age and constant

###Creating Plots###

#Figure 1: death vs. gender (side-by-side bar plots)
table1 <- table(corona$death2, corona$gender2)  #create two-way contigency table
proptable1 <- prop.table(table1, margin=2)  #create column proportions table
barplot(proptable1, beside=TRUE, legend.text = T, xlab = "Gender", ylab = "Proportion of Deaths Reported")  #create bar plot using proportions with side-by-side bars

#Figure 2: lag.onset.hosp vs Continental.Region (side-by-side boxplots)
boxplot(corona$lag.onset.hosp2 ~ corona$Continental.Region, xlab = "Continental Region", ylab = "Delay in Days Between Onset of Symptoms & Seeking Hospital Care")

#Figure 3: lag.onset.hosp vs age (linear regression)
plot(corona$age2, corona$lag.onset.hosp2, xlab = "Age", ylab = "Delay in Days Between Onset of Symptoms & Seeking Hospital Care")
abline(m1)  #adds the linear regression line onto scatterplot above

