#Elijah Chou
#Lab 11
#Lab Manual 3

###Setting up Environment###

#Set up working directory
setwd("C:/Users/ecool/Desktop/R Code")

#Import data
babies <- read.table("babies.txt", header = T)

####Inspecting Dataset####
summary(babies)

#Create new variable "parity"
babies$parityF <- factor(babies$parity, labels = c("first born", "otherwise"))

#do same for smoking
babies$smokeF <- factor(babies$smoke, labels = c("not now", "yes now"))

#check recoding
table(babies$parity, baies$parityF)
str(babies$parityF)

####Using Rmisc####
#install Rmisc
install.packages("Rmisc")

library(Rmisc) # do this everytime you start a new R project

#summarySE can give us the same information as summary
summarySE(data=babies, measurevar = "bwt")

#can tell how mean varies across groups
summarySE(data = babies, groupvars = "parityF", measurevar = "bwt")

#Creating a 2x2 contingency table
smk.par.tab <- table(babies$parityF)

#adding totals
addmargins(smk.par.tab)

#Look at proportions
prop.table(smk.par.tab) #overall

prop.table(smk.par.tab, margin=1) #row proportion

prop.table(smk.par.tab, margin=2) #column proportion

#Box-Plots Visualizations
boxplot(babies$bwt) #one variable

boxplot(babies$bwt ~ babies$smokeF) #two variable

#Creating bar plot visualization
tableone <- table(babies$smokeF, babies$parityF)

barplot(tableone,
        beside=T,
        legend.text=T) # Order matters!




