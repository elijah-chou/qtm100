#Loading the file into code
yrbss2013 <- read.csv("C:/Users/ecool/Downloads/yrbss2013.csv")

#Retrieving the summary of the information saved in the csv file
summary(yrbss2013)

#graphing a plot of height vs. weight, labeling the x-axis as "Height (m)" and
# y-axis as "Weight (kg)"

plot(x = yrbss2013$height_m,
     y = yrbss2013$weight_kg,
     xlab = "Height (m)",
     ylab = "Weight (kg)")
