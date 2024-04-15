#Elijah Chou
#Lab 11
#Lab Manual 3

###Setting up Environment###

install.packages("Rmisc") 
library(Rmisc)

#Set up working directory
setwd("C:/Users/ecool/Desktop/R Code")

#Import data
adni <- read.table("C:/Users/ecool/Desktop/R Code/Data Sets/ADNI.txt", header = T)

####Inspecting Dataset####

#Question 1: # of observations: 276
dim(adni)
summary(adni)

#Question 2: following code prints "structure" of variable, stating what kind of variable R recognizes it as
str(adni$AGE) #Age is quantitative, numerical
str(adni$GENDER)  #Gender is categorical, factor
str(adni$APOE4)  #APOE4 is categorical, but is and integer
str(adni$MMSE)  #MMSE is quantitative, int
str(adni$WholeBrain)   #WholeBrain is quantitative, int
str(adni$adas)  #adas is quanititative, numerical
str(adni$DX)  #DX is categorical, factor

#Question 3: Adds a new variable that makes the APOE4 data into factor type
adni$APOE4F <- factor(adni$APOE4, labels = c("0 copies", "1 copy", "2 copies"))
str(adni$APOE4F) #check whether the change is correct or not

#Question 4: Graphs the distribution of the age data
hist(adni$AGE)

#Question 5: Plots boxplots of DX vs MMSE & DX vs adas
#shows that those with Alzheimers Disease do worse than normal and MCI people
boxplot(adni$MMSE ~ adni$DX)

#shows that AD people tend to score higher than MCI and normal people
boxplot(adni$adas ~ adni$DX) #identifies potential outliers

#Question 6:
#Creates new proportioned Whole Brain variable
adni$propWholeBrain <- adni$WholeBrain/100000

#returns a summary of how many times each categorical outcome occurs
table(adni$DX) 

#returns the average age and standard dev. of age overall
summarySE(data = adni, measurevar = "AGE")
          
#returns the average age and standard deviation for age within DX categories
summarySE(data = adni, measurevar = "AGE", groupvars = "DX")

#returns the average brain volume in mm^3 and its standard deviation within DX categories
summarySE(data = adni, measurevar = "propWholeBrain", groupvars = "DX")

#creates contingency table of DX and APOE4 with row proportions
smk.tab <- table(adni$DX, adni$APOE4F)
smk.tab  #prints table
prop.table(smk.tab, margin = 1) #prints table but in row proportions

#creates contingency table of DX and gender with row proportions
smk.tab2 <- table(adni$DX, adni$GENDER)
smk.tab2  #prints table
prop.table(smk.tab2, margin = 1) #prints table but in row proportions

#creates contingency table of APOE4 overall
smk.tab1 <- table(adni$APOE4F)
smk.tab1 #prints table
prop.table(smk.tab1) #prints table but in overall proportions






