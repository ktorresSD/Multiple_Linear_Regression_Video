#########################################################################################
# Last Date modified: 11/23/2018
# Author: Katy Torres
# Description: Multiple linear regression with interactions
##########################################################################################

#VIDEO 2 - DATA 

#To the user: Set path to where data is stored
setwd("C:/Users/ktorr/Desktop/STAT610")
#________________________________________________________________________________________
# Read in data and view summary statistics
#----------------------------------------------------------------------------------------
#Read in data
cars0 <- read.csv("cars.csv",header=T,na.strings=c(NA, "?"))


names(cars0) #see the names of the variables in dataset
head(cars0) #to see the first 6 rows
dim(cars0) #dimensions of the dataset
View(cars0) #View dataset


str(cars0) # to see structure of data (name, class of variable, n's, first few entries per variable)
summary(cars0) #See min, max, quartiles median, mean, missing values

#________________________________________________________________________________________
# Dealing with missing values
#----------------------------------------------------------------------------------------
#what is the median?
median(cars0$horsepower)
#why? 
which(is.na(cars0$horsepower)) #missing values in these positions
View(cars0)

#excluding missing values
#saw 6 missing values in horsepower variable, will omit these rows
cars_complete<- na.omit(cars0)
dim(cars_complete)
summary(cars_complete)

#________________________________________________________________________________________
# Visualizing data
# ----------------------------------------------------------------------------------------
#In this example we will show you the code focusing on a few variables.
#If you want to investigate other varibales, it is
#easy to change the code with the variables you are interested in

#Exploratory plots example for 2 variables

#why do a histogram?
par(mfrow = c(1, 2))
hist(cars0$mpg, main= "histogram of mpg")
abline(v = median(cars0$mpg), col = "blue", lwd = 4)

#Exploratory boxplots
boxplot(cars0$mpg ~ cars0$cylinders, col = "red", main= "MPG  by cylinders")
boxplot(cars0$horsepower ~ cars0$cylinders, col = "red", main= "Horsepower  by cylinders")

#Scatterplot of 2 continous varibales
#why do a scatterplot?
plot(cars0$horsepower,cars0$mpg, main= "MPG and Horsepower")

#Scatterplot by cylinders
plot(cars0$horsepower,cars0$mpg, col = cars0$cylinders, main= "MPG and Horsepower by Cylinders")
legend("bottomright", legend = c("3", "4", "5", "6", "8"), col = c(3, 4, 5, 6, 8), pch = 19, bty = "n")
#levels(cars0$cylinders)


#________________________________________________________________________________________
# Dealing with outliers
#----------------------------------------------------------------------------------------
#check for outliers (uses boxplot IQR)
outliers <- boxplot(cars0$horsepower, plot=FALSE)$out
#showing the rows for the cars0 with outliers
cars0[which(cars0$horsepower %in% outliers),] 
#remove the cars with outliers
cars <- cars0[-which(cars0$horsepower  %in% outliers),] 

#Plot the datsert with outliers and without outliers
summary(cars$horsepower)
summary(cars$mpg)

# Plot of data with outliers
par(mfrow=c(1, 2))
plot(cars0$mpg, cars0$horsepower, xlim=c(0, 50), ylim=c(0, 250), main="With Outliers", xlab="mpg", ylab="hp", pch="*")
abline(lm(cars0$horsepower~cars0$mpg, data=cars0), col="blue", lwd=3, lty=2)

plot(cars$mpg, cars$horsepower, xlim=c(0, 50), ylim=c(0, 250), main="Without Outliers", xlab="mpg", ylab="hp", pch="*")
abline(lm(cars$horsepower~cars$mpg, data=cars), col="blue", lwd=3, lty=2)

#________________________________________________________________________________________
# Subsetting data
#----------------------------------------------------------------------------------------

#subsetting by specific cylinder number
#to see number of rows for each category, 
table(cars_complete$cylinders) 
dim(cars_complete)

#subset by "cylinders". Will keep 4 and 8 cylinder cars as they have large n
subset_4<- cars_complete[cars_complete$cylinders== "4",]
subset_8<- cars_complete[cars_complete$cylinders== "8",]

#Combines together data for the cylinders of interest
subsetcars <- rbind(subset_4, subset_8)

table(subsetcars$cylinders) 
dim(subsetcars)

summary(cars$horsepower)
summary(subsetcars$horsepower)

#________________________________________________________________________________________
# Visualizing data for variables of interest after subsetting cylinders
# ----------------------------------------------------------------------------------------
par(mfrow=c(1, 1))
plot(subsetcars$mpg,subsetcars$horsepower, col = subsetcars$cylinders,  main= "MPG and Horsepower by Cylinders")
legend("topright", legend = c( "4", "8"), col = c(4, 8), pch = 19, bty = "n")






























#________________________________________________________________________________________
# VIDEO 3 - Fitting linear models
#----------------------------------------------------------------------------------------
#plot all points for horsepower and horsepower
par(mfrow=c(1,1))
plot(subsetcars$mpg,subsetcars$horsepower,
     main="Car data",
     xlab = "mpg", ylab = "horsepower")

# #fit linear regression for different levels of cyl  inders
fit.4 <- lm(subsetcars$horsepower ~ subsetcars$mpg, subset=subsetcars$cylinders=="4")
fit.8 <- lm(subsetcars$horsepower ~ subsetcars$mpg, subset=subsetcars$cylinders=="8")

#Add regression lines to plot generated
abline(fit.4, col="blue")
abline(fit.8, col="magenta2")

legend("topright", legend = c("4 cylinders", "8 cylinders"), col = c("blue", "magenta2"), pch = 19, bty = "n")

#LINEAR REGRESSION FOR model with mpg and cylinders predicting horsepower
fit <- lm(subsetcars$horsepower ~ subsetcars$mpg + subsetcars$cylinders)
summary(fit)

# #LINEAR REGRESSION FOR model with interaction
fit.inter.1 <-lm(subsetcars$horsepower~subsetcars$mpg + subsetcars$cylinders + subsetcars$year+ subsetcars$mpg:subsetcars$cylinders)
summary(fit.inter.1)

# #LINEAR REGRESSION FOR model with interaction (Dropped variable)
fit.inter <- lm(subsetcars$horsepower~subsetcars$mpg + subsetcars$cylinders + subsetcars$mpg:subsetcars$cylinders)
summary(fit.inter)










#________________________________________________________________________________________
# VIDEO 4 - Diagnostics
#----------------------------------------------------------------------------------------
#install.packages("car")
library(car)
# 
# #Check outliers
# car::outlierTest(fit.inter)
# cooksd <- cooks.distance(fit.inter)
# plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's dista
# abline(h = 4*mean(cooksd, na.rm=T), col="red")  
# 
# 
# qqPlot(fit.inter, main="QQ Plot") #qq plot for studentized resid
# leveragePlots(fit.inter) # leverage plots 

#1.Checking Linearity #ISSUE
#https://www.r-bloggers.com/r-regression-diagnostics-part-1/
# component + residual plot 
crPlots(fit.inter) #ISSUE
qqPlot(fit.inter, main="QQ Plot")
shapiro.test(residuals(fit.inter)) #want to be high p value


#3. Check homoscedasticity #ISSUE
# plot studentized residuals vs. fitted values
spreadLevelPlot(fit.inter) #model missing some cylinders
ncvTest(fit.inter)
#LOW P, Therefore we reject hypothesis of equal error variances


#4. Checking Multicollinearity: using VIF score
car::vif(fit)

#5. Autocorrelation #ISSUE
library(lmtest)
dwtest(fit.inter) #DW should be close to 2 and pvalue should be greater than 0.5
