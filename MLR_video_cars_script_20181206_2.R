#########################################################################################
# Author: Katy Torres
# Description: Multiple linear regression with interactions video Series
##########################################################################################

# To the user: Set path to where data is stored
setwd("C:/Users/ktorr/Desktop/STAT610")
#________________________________________________________________________________________
# Read in data and view summary statistics
#----------------------------------------------------------------------------------------
# Read in data. This dataset is stored as a CSV and is called "cars.csv"
cars0 <- read.csv("cars.csv", header=T, na.strings=c(NA, "?"))

# Inspecting data frame
names(cars0) #to see the names of the variable in our dataset
head(cars0)  #to see the first 6 rows of our dataset
dim(cars0)   #to see the dimensions of our dataset
View(cars0) 

str(cars0) # In R, factors are used to represent categorical data.
summary(cars0) 

#________________________________________________________________________________________
# Dealing with missing values
#----------------------------------------------------------------------------------------
# Missing data are represented as NA.
# if the data you has some missing values, functions will often return NA when doing operations on numbers.
# You can add the argument na.rm=TRUE to calculate the result while ignoring the missing values.

median(cars0$horsepower)

# To see rows with missing values for horsepower:
which(is.na(cars0$horsepower)) # returns a vector with TRUE where it is missing an entry for horsepower
View(cars0)

# Excluding missing values
cars_complete<- na.omit(cars0) #creates a new dataset without missing data 
dim(cars_complete)
summary(cars_complete)



#________________________________________________________________________________________
# Visualizing data
# ----------------------------------------------------------------------------------------

# Histograms are a great wat to graphically summarize the distribution of a univariate data set. 
# The histogram graphically shows the following: 
# 1.	center 
# 2.	spread  
# 3.	skewness  
# 4.	presence of outliers and 
# 5.	presence of multiple modes in the data. 

hist(cars0$mpg, main= "Histogram of MPG")
#here I a adding a vertical line where the median MPG is. I am asking R to color it blue and 4 units wide
abline(v = median(cars0$mpg), col = "blue", lwd = 4)

# Exploratory boxplots
boxplot(cars0$mpg ~ cars0$cylinders, col = "red", main= "MPG  by cylinders")
boxplot(cars0$horsepower ~ cars0$cylinders, col = "red", main= "Horsepower  by cylinders")

# Now we will do a Scatterplot of 2 continous variables
# Scatterplots are a good way to see relationships between two variables
plot(cars0$horsepower,cars0$mpg, main= "MPG and Horsepower")

# Scatterplot by cylinders
plot(cars0$horsepower,cars0$mpg, col = cars0$cylinders, main= "MPG and Horsepower by Cylinders")
legend("topright", legend = c("3", "4", "5", "6", "8"), col = c(3, 4, 5, 6, 8), pch = 19, bty = "n")



#________________________________________________________________________________________
# Subsetting data by a specific cylinder number
#----------------------------------------------------------------------------------------
table(cars_complete$cylinders) 
dim(cars_complete)

# Subset by "cylinders". We Will keep 4 and 8 cylinder cars as they have largest n's
subset_4<- cars_complete[cars_complete$cylinders== "4",]
subset_8<- cars_complete[cars_complete$cylinders== "8",]

dim(subset_4)
dim(subset_8)

# Combine together data for the cylinders of interest
subsetcars <- rbind(subset_4, subset_8)

table(subsetcars$cylinders) 
dim(subsetcars)


#________________________________________________________________________________________
# Visualizing data for variables of interest after subsetting cylinders
# ----------------------------------------------------------------------------------------
plot(subsetcars$mpg, subsetcars$horsepower, col = cylinders,  main= "MPG and Horsepower by Cylinders")
legend("topright", legend = c( "4", "8"), col = c(4, 8), pch = 19, bty = "n")


#________________________________________________________________________________________
# Centering
# ----------------------------------------------------------------------------------------
subsetcars$c.mpg <- subsetcars$mpg - mean(subsetcars$mpg)

par(mfrow=c(1, 2))
# Histogram of centered values
hist(subsetcars$mpg, main= "Histogram of MPG")
abline(v = median(subsetcars$mpg), col = "blue", lwd = 4)


# Histogram of centered values
hist(subsetcars$c.mpg, main= "Histogram of centered MPG")
abline(v = median(subsetcars$c.mpg), col = "blue", lwd = 4)






















#________________________________________________________________________________________
# VIDEO 3 - Fitting linear models
#----------------------------------------------------------------------------------------
# Fitting the model
#----------------------------------------------------------------------------------------
# Simple Linear Regression
fit.simple0 <- lm(horsepower ~ c.mpg, data = subsetcars)
summary(fit.simple0)

# Multiple Linear Regression
fit.multiple <- lm(horsepower ~ c.mpg + cylinders, data = subsetcars)
summary(fit.multiple)

# Multiple Linear Regression with Interaction
fit.inter<- lm(horsepower~ c.mpg + cylinders + c.mpg:cylinders, data = subsetcars)
summary(fit.inter)

# Simple Linear Regression without centering (Used for plots)
fit.simple <- lm(horsepower ~ mpg, data = subsetcars)
summary(fit.simple)


# Plots of Regression Lines
#----------------------------------------------------------------------------------------
par(mfrow=c(1, 1))
# 1. Plot all points for horsepower and mpg (without Interaction)
plot(mpg,horsepower, main="Simple Linear Regression",
     xlab = "MPG", ylab = "Horsepower")
abline(fit.simple, col="green")
legend("topright", legend = c("No Interaction"), col = c("green"), pch = 19, bty = "n")


# 2. Plot all points for horsepower and mpg (with Interaction)
plot(mpg,horsepower, main="Multiple Linear Regression \n with Interaction",
     xlab = "MPG", ylab = "Horsepower")
# Fit linear regression for different levels of cylinders
fit.4 <- lm(horsepower ~ mpg, subset=cylinders=="4", data = subsetcars)
fit.8 <- lm(horsepower ~ mpg, subset=cylinders=="8", data = subsetcars)
# Add regression lines to plot generated
abline(fit.4, col="blue")
abline(fit.8, col="red")
legend("topright", legend = c("4 cylinders", "8 cylinders"), col = c("blue", "red"), pch = 19, bty = "n")


# 3. Plot all points for horsepower and mpg (comparing models with and without Interaction)
plot(mpg,horsepower, main="Comparing Simple Linear Regression and \n  Multiple Linear Regression with Interaction",
     xlab = "MPG", ylab = "Horsepower")
abline(fit.simple, col="green")
abline(fit.4, col="blue")
abline(fit.8, col="red")
legend("topright", legend = c("4 cylinders", "8 cylinders", "No Interaction"), col = c("blue", "red", "green"), pch = 19, bty = "n")
























#________________________________________________________________________________________
# VIDEO 4 - Diagnostics
#----------------------------------------------------------------------------------------

#1.Checking Linearity 
#----------------------------------------------------------------------------------------
attach(subsetcars)

# Scatter plus regression lines for 4 and 8 cylinders
plot(mpg,horsepower, main="Linear Regression of cars \n with 4 and 8 cylinders",
     xlab = "MPG", ylab = "Horsepower")
abline(fit.4, col="blue")
abline(fit.8, col="magenta2")

par(mfrow=c(1, 2))

# Plot Observed vs. Predicted
plot(x=fit.inter$fitted.values, y=horsepower, main="Observed vs Predicted Values", xlab="Predicted Value", ylab="Observed Value" )
lines(x=fit.inter$fitted.values, y=fit.inter$fitted.values, lty=1, col="red")

# Plot Residuals vs. Predicted
plot(x=fit.inter$fitted.values, y=fit.inter$residuals, main="Residuals vs Predicted Values", xlab="Predicted Value", ylab="Residual" )



#2. Autocorrelation, Independent error terms.
#----------------------------------------------------------------------------------------
#install.packages("car")
library(car)
durbinWatsonTest(fit.inter)  
#Interpretation: if the p-value < 0.05, then, the independent errors assumption is not plausible

par(mfrow=c(1, 1))
# Auto-correlation plot
auto.cor <- acf(fit.inter$residuals)

# Plot Residuals vs Time Order
n <- length(fit.inter$fitted)
plot(1:n, fit.inter$residuals, xlab="Time Order", ylab="Residual", main="Residuals vs Time Order", type="l")



#3. Check homoscedasticity. Constant Standard Deviation.
#----------------------------------------------------------------------------------------
ncvTest(fit.inter) 
#Interpretation: low p-value, therefore we reject hypothesis of equal error variances

subsetcars$cyls <- as.factor(cylinders)
#Brown-Forsythe test
#install.packages("onewaytests")
library(onewaytests)
bf.test(horsepower ~ cyls,  data = subsetcars)


#4. Check Normality
#----------------------------------------------------------------------------------------
# Normal Probability Plot
qqPlot(fit.inter, main="QQ Plot")

# Histogram of Studentized Residuals
#install.packages("MASS")
library(MASS)
studres(fit.inter) 
hist(studres(fit.inter), freq=FALSE, main="Distribution of Studentized Residuals")  # Look at the long right tail 


# Assessing Outliers
#----------------------------------------------------------------------------------------
tail(sort(horsepower))
#which vehicles are these?
subsetcars[c(215, 220, 225, 225, 225, 230), ]
#summary statsistics for variable horsepower)
summary(horsepower)
#visualizing this
boxplot(horsepower, main= "Horsepower")
 
outlierTest(fit.inter) # Bonferonni p-value for most extreme obs


#Checking Multicollinearity: Using VIF score
#----------------------------------------------------------------------------------------
car::vif(fit.multiple)



#From class code:

# The players with the higest PTS/gamme average were classified as outliers
# We should keep these players in the data set but consider a different model. 

