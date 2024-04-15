#Elijah Chou
#Lab 11
#Preliminary Project

###Setting up Environment###

#Set up working directory
setwd("C:/Users/ecool/Desktop/R Code")

#Import data
corona <- read.csv("C:/Users/ecool/Desktop/R Code/Data Sets/COV Dataset.csv")

####Inspecting Dataset####
summary(corona)

####Recoding variables####

##Recoding the "death" variable
summary(corona$death) #check death variable, need to change categories from numbers to labels

corona$death2<-factor(NA, levels=c("Died", "Did Not Die"))  #create new categorical variable
corona$death2[corona$death==0]<-"Did Not Die"  #reassigns those who did not die to respective category
corona$death2[corona$death==1]<-"Died"  #reassigns those who did die to respective category

summary(corona$death2) #verify changes to variable

##Recoding the "lag.onset.hosp" variable
summary(corona$lag.onset.hosp) #check lag.onset.hosp variable, noticed that there are two problems: a -1 value and many 999 values

corona$lag.onset.hosp2<-corona$lag.onset.hosp #create duplicate "lag.onset.hosp" variable
corona$lag.onset.hosp2[corona$lag.onset.hosp2<0]<-NA #recode all entries of this variable where delay is less than 0 to NA
corona$lag.onset.hosp2[corona$lag.onset.hosp2==999]<-NA #recode all "coded NAs" of 999 back to NA

summary(corona$lag.onset.hosp2) #verify changes to variable

##Recoding the "gender" variable
summary(corona$gender)  #check gender variable, need to combine the two "male" groups into one, do same for "female" categories, and remove 999 values

corona$gender2<-corona$gender  #create duplicate "gender" variable
corona$gender2[corona$gender2=="MALE"]<-"male"  #recode erroneous male values
corona$gender2[corona$gender2=="Female"]<-"female"  #recode erroneous female values
corona$gender2[corona$gender2==999]<-NA   #recode all "coded NAs" of 999 back to NA
corona$gender2<-factor(corona$gender2)  #drop erroneous categories

summary(corona$gender2)  #verify changes to variable

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


####Creating Graphs & Figures####

#Figure 1: death vs. gender
table1 <- table(corona$death2, corona$gender2)  #create two-way contigency table
proptable1 <- prop.table(table1, margin=2)  #create column proportions table
barplot(proptable1, beside=TRUE, legend.text = T, xlab = "Gender", ylab = "Proportion")  #create bar plot using proportions with side-by-side bars

#Figure 2: death vs. Continental.Region
table2 <- table(corona$death2, corona$Continental.Region)  #create two-way contigency table
proptable2 <- prop.table(table2, margin=2)  #create column proportions table
barplot(proptable2, beside=T, legend.text = T, xlab = "Continental Region", ylab = "Proportion")  #create bar plot using proportions with side-by-side bars

#Figure 3: death vs. age
#creates side-by-side boxplots of the distributions of age based on whether they died or not
boxplot(corona$age2 ~ corona$death2, xlab = "Did Patient Die of Disease?", ylab = "Age") 

#Figure 4: lag.onset.hosp vs. gender
#creates side-by-side boxplots using lag.onset.hosp and gender variables
boxplot(corona$lag.onset.hosp2 ~ corona$gender2, xlab = "Gender", ylab = "Delay in Days Between Onset of Symptoms & Seeking Hospital Care") 

#Figure 5: lag.onset.hosp vs. Continental.Region
#creates side-by-side boxplots using lag.onset.hosp and Continental.Region variables
boxplot(corona$lag.onset.hosp2 ~ corona$Continental.Region, xlab = "Continental Region", ylab = "Delay in Days Between Onset of Symptoms & Seeking Hospital Care") 

#Figure 6: lag.onset.hosp vs. age
#creates scatterplot using lag.onset.hosp and age variables
plot(corona$age2, corona$lag.onset.hosp2, xlab = "Age", ylab = "Delay in Days Between Onset of Symptoms & Seeking Hospital Care")


