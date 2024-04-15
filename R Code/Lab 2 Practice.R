#Elijah Chou
#Lab 11
#Lab Practice 2

###Setting up Environment###

#Set up working directory
setwd("C:/Users/ecool/Desktop/R Code")

#Import data
source("http://www.openintro.org/stat/data/present.R")

#######Answering Questions######
#2. What are variable (column) names in this data set?
#Typing name of set shows data table:
present

# Prints the names of the variables (columns)
names(present)
#Ans: "year", "boys", "girls"

#3. What are the dimensions of the data frame? (How many observations and variables are there?)
# Gives dimensions of the data set
dim(present)
# Ans: 63 observations, 3 variables

#4. What years are included in this data set?
# shows first and last 6 observations in data
head(present)
tail(present)
# Ans: 1940 - 2002

#5. In the lab 2 manual you observed from the arbuthnot data set
# that boys were born in greater proportion than girls in London from 1629-1710. 
# Now examine the present data set -
# does the relationship hold true in the US from 1940-2002? Why or why not?

#Returns sum of the number of times that there were more boys than girls in each year
sum(present$boys > present$girls)

# Ans: yes, the trend still holds true

#6 + 7. Make a plot that displays the the proportion of boys over time. Do you see any trends?

#Creates a new variable: proportion of boys born in a year
present$propBoys <- present$boys / (present$boys + present$girls)

#Plots proportion of boys against time (year)
plot(x = present$year, y = present$propBoys, type = "l")

#Gives plot a title
title(main = "Proportion of boys over time")

#Ans: there seems to be a downward trend over time

#8. In how many years did the proportion of boys exceed 0.512?

# Returns the number of times that the proportion of boys exceeded 0.512
sum(present$propBoys > 0.512)

#Ans: 49 times

#9.  In which year did we see the largest total number of births in the U.S.?

#Creates a new variable that contains the total number of births in a given year
present$totalBirths <- present$boys + present$girls

#finds the index of the row in which the maximum number of births occurred
which.max(present$totalBirths)


#Ans: 1961
