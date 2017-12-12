# Project : Multivariate Linear Regression General Motors

#Create a Multiple Linear Regression Model for General Motors (GM) Data set .

#Data collected for several hundred used General Motors (GM) cars allows us to develop a multivariate regression model to determine car values based on a variety of characteristics such as mileage, make, model, cruise control, and so on.

#Develop a Multivariate Regression Model for the data collected for several hundred used General Motors (GM) car values based on a variety of characteristics such as:
  
   #1. Price: suggested retail price for the used GM car
   #2. Mileage: number of miles the car has been driven
   #3. Make: manufacturer of the car such as Cadillac, Pontiac, and Chevrolet
  #4. Cylinder: number of cylinders in the engine
  #5. Liter: a more specific measure of engine size
  #6. Cruise: indicator variable representing whether the car has cruise control (1 = cruise)
  #7. Sound: indicator variable representing whether the car has upgraded speakers (1 = upgraded)
  #8. Leather: indicator variable representing whether the car has leather seats (1 = leather)

###Importing the dataset

#gm_dataset<-read.csv(file.choose())
gm_dataset<- read.csv("D:/Courses/acadgild/BA with R/Sessions/Session 23  - Project 2 3/Project 3/gm_dataset.csv")
dim(gm_dataset)
str(gm_dataset)
View(gm_dataset)

#Converting Cruise,Leather & Sound into Categorical Variables & then
###Splitting the dataset into 2 parts : training_gm_dataset & testing_gm_dataset
library(caTools)
gm_dataset$Cruise<-as.factor(gm_dataset$Cruise)
gm_dataset$Sound<-as.factor(gm_dataset$Sound)
gm_dataset$Leather<-as.factor(gm_dataset$Leather)
str(gm_dataset)

set.seed(2)
split<-sample.split(gm_dataset,SplitRatio = 0.80)
split
training_gm_dataset<-subset(gm_dataset,split=="TRUE")
dim(training_gm_dataset)
testing_gm_dataset<-subset(gm_dataset,split=="FALSE")
dim(testing_gm_dataset)


###Descriptive Analysis of the Predictor Variables for training_gm_dataset

# Variables in training_gm_dataset
names(training_gm_dataset)
str(training_gm_dataset)
library(psych)
attach(training_gm_dataset)

# Describing Mileage in training_gm_dataset
hist(training_gm_dataset$Mileage,main="Histogram of Mileage of GM Cars",ylab = "Frequency",xlab="Mileage",col="red")
boxplot(training_gm_dataset$Mileage,main="boxplot of Mileage",xlab="Mileage",col="red")


# Describing Cylinder in training_gm_dataset
hist(training_gm_dataset$Cylinder,main="Histogram of Cylinder of GM Cars",ylab = "Frequency",xlab="Cylinder",col="green")
boxplot(training_gm_dataset$Cylinder,main="boxplot of Cylinder",xlab="Cylinder",col="green")

# Describing Liter in training_gm_dataset
hist(training_gm_dataset$Liter,main="Histogram of Liter of GM Cars",ylab = "Frequency",xlab="Liter",col="blue")
boxplot(training_gm_dataset$Cylinder,main="boxplot of Liter",xlab="Liter",col="blue")

# Describing Mileage in training_gm_dataset
class(training_gm_dataset$Mileage)
describe(training_gm_dataset$Mileage)

# Describing MAke in training_gm_dataset
class(training_gm_dataset$Make)
levels(training_gm_dataset$Make)
table(training_gm_dataset$Make)

# Describing Cylinder in training_gm_dataset
class(training_gm_dataset$Cylinder)
describe(training_gm_dataset$Cylinder)

# Describing Liter in training_gm_dataset
class(training_gm_dataset$Liter)
describe(training_gm_dataset$Liter)

# Describing Cruise in training_gm_dataset
class(training_gm_dataset$Cruise)
levels(training_gm_dataset$Cruise)
table(training_gm_dataset$Cruise)     # 0 ->  Not Cruise   1-> Cruise

# Describing Sound in training_gm_dataset
class(training_gm_dataset$Sound)
levels(training_gm_dataset$Sound)
table(training_gm_dataset$Sound)     # 0 ->  Not Upgraded   1-> upgraded


# Describing Leather in training_gm_dataset
class(training_gm_dataset$Leather)
levels(training_gm_dataset$Leather)
table(training_gm_dataset$Leather)  # 0 ->  No Leather Seats   1->  Leather Seats

###Scatter Plot of predictor Variable Vs Response Variable to check what type of relationship is their between them
plot(training_gm_dataset$Mileage,training_gm_dataset$Price,main= "Mileage Vs Price (training dataset)" , xlab = "Mileage", ylab = "Price",col="red",abline(lm(Price~Mileage,data=training_gm_dataset)))

plot(training_gm_dataset$Cylinder,training_gm_dataset$Price,main= "Cylinder Vs Price (training dataset)" , xlab = "Cylinder", ylab = "Price",col="brown",abline(lm(Price~Cylinder,data=training_gm_dataset)))

plot(training_gm_dataset$Liter,training_gm_dataset$Price,main= "Liter Vs Price (training dataset)" , xlab = "Liter", ylab = "Price",col="green",abline(lm(Price~Liter,data=training_gm_dataset)))

plot(training_gm_dataset$Cruise,training_gm_dataset$Price,main= "Cruise Vs Price (training dataset)" , xlab = "Cruise", ylab = "Price",col="blue",abline(lm(Price~Cruise,data=training_gm_dataset)))

plot(training_gm_dataset$Sound,training_gm_dataset$Price,main= "Sound Vs Price (training dataset)" , xlab = "Sound", ylab = "Price",col="yellow",abline(lm(Price~Sound,data=training_gm_dataset)))

plot(training_gm_dataset$Leather,training_gm_dataset$Price,main= "Leather Vs Price (training dataset)" , xlab = "Leather", ylab = "Price",col="red",abline(lm(Price~Leather,data=training_gm_dataset)))

#Checking correalation of each of predictors with Response Variable in training dataset
cor(training_gm_dataset$Mileage,training_gm_dataset$Price)
cor(training_gm_dataset$Cylinder,training_gm_dataset$Price)
cor(training_gm_dataset$Liter,training_gm_dataset$Price)

###Scatter plot Matrix to interpret Relationship
#plotting a scatter matrix to understand the pattern or the relationship between the variables and the response variable
library(lattice)
splom(~training_gm_dataset[c(1:376),],groups=NULL,data=training_gm_dataset,axis.line.tck=0,axis.text.alpha=0)

###Multi-Collinearity in training dataset
# finding multicollinearity by removing price from the training_gm_dataset & checking correlation between predictor variables only
#we created a new dataset gm_dataset_a to find multicollinearity among the variable

#install.packages("caret")
library(caret)
training_gm_dataset_a=subset(training_gm_dataset,select = -c(Price))
numericdata<-training_gm_dataset_a[sapply(training_gm_dataset_a,is.numeric)]
descrCor<-cor(numericdata)
descrCor
cor.plot(descrCor,type="lower")

# we can see that predictor variables i.e. Cylinder & Liter are highly correlated  
# meaning multi-collinarity  exists among these two variables

###Multvariate Linear Regression Model building for training_gm_dataset

#building the model

#install.packages("CARS")
library(CARS)
library(car)
fit<-lm(Price~Mileage+Make+Cylinder+Liter+Cruise+Sound+Leather,data=training_gm_dataset)
# using all the variables ar predictor variables
summary(fit)
vif(fit)

#removing Liter from Predictor variable as Cylinder & Liter are highly Correlated
fit1<-lm(Price~Mileage+Make+Cylinder+Cruise+Sound+Leather,data=training_gm_dataset) # using all the variables as predictor variables except Liter 
summary(fit1)
vif(fit1)


# removing sound form 'fit1' and using others as predictor variables as less significant
fit2<-lm(Price~Mileage+Make+Cylinder+Cruise+Leather,data=training_gm_dataset) 
summary(fit2)
vif(fit2)

# removing Leather form 'fit2' and using others as predictor variables as less significant
fit3<-lm(Price~Mileage+Make+Cylinder+Cruise,data=training_gm_dataset)
summary(fit3)
vif(fit3)

# removing Cruise form 'fit3' and using others as predictor variables as less significant
fit4<-lm(Price~Mileage+Make+Cylinder,data=training_gm_dataset)
summary(fit4) # Our final model with R-square 0.8306
vif(fit4)

fit5<-lm(Price~Mileage+Cylinder,data = training_gm_dataset)
summary(fit5)  # having R-square 0.6865 so this model is not a good fit for our training dataset
vif(fit5)

# Our final Multivariate Regression model is  as it uses minimum variables and gives a good -square value comparable with other models
fit4<-lm(Price~Mileage+Make+Cylinder,data=training_gm_dataset)
summary(fit4)
vif(fit4)

#From all of the above we can see that he best Multivariate Regression model which fits our training_gm_dataset is fit4 .As the value of R square is .8306 which means 83.06% of the variance is explained by fit4 in the "Price" . This R square value is not much effected by removing other predictor variables like cruise sound and leather. Hence our model has minimum number of predictors and alsowith a good accuracy of around 83.06% 
  
  #Hence our  Fitted Regression Equation for each Make in training_gm_dataset becomes 

#Price = 1.017e+04 + ((-2.089e-01) * Mileage) + ((4.361e+03) * Cylinder) + ((-1.142e+04) * MakeChevrolet)  + ((-1.340e+04) * MakePontiac)

### Our Fitted Regression Model means for training_gm_dataset is

#If make is Cadillac  then the Fitted Regression Equation for Cadillac becomes (in this case both MakeChevrolet and MakePontiac are equal to zero & MakeCadillac = 1 )

####Price = 1.017e+04 + ((-2.089e-01) * Mileage) + ((4.361e+03) * Cylinder)
####      = 10170 + (-0.2089 * Mileage)  + ( 4361 * Cylinder )

#If make is Chevrolet  then the Fitted Regression Equation for Chevrolet becomes (in this case both MakeCadillac and MakePontiac are equal to zero & MakeChevrolet = 1)

####Price = 1.017e+04 + ((-2.089e-01) * Mileage) + ((4.361e+03) * Cylinder) + ((-1.142e+04) * MakeChevrolet)
####      = 10170 + (-0.2089 * Mileage)  + ( 4361 * Cylinder ) + (- 11420 * 1)
####      =  -1252 + (-0.2089 * Mileage)  + ( 4361 * Cylinder )

#If the make is Pontiac then the then the Fitted Regression Equation for Pontiac becomes (in this case both MakeCadillac and MakeChevrolet are equal to zero  & MakePontiac = 1 ) .

####Price = 1.017e+04 + ((-2.089e-01) * Mileage) + ((4.361e+03) * Cylinder) + ((-1.340e+04) * MakePontiac)

####= 10170 + (-0.2089 * Mileage)  + ( 4361 * Cylinder ) + (-13400*1)

####= -3230 + + (-0.2089 * Mileage)  + ( 4361 * Cylinder )



#Finding the predicted Value of price  through the model made  "fit4"
predictedPrice <- predict(fit4)
training_gm_dataset$predictedPrice <- predictedPrice
training_gm_dataset$predictedPrice


# error values of sales through the model is below. These values are giving us the residuals/errors 
#i.e difference between predicted and Actual price
training_gm_dataset$error<- residuals(fit4)
training_gm_dataset$error


# Adding observation No. Column n our dataset
training_gm_dataset$obsno<-c(1:376)
training_gm_dataset$obsno


#comparing model predicted values with actual values of Price using the graph for training_gm_dataset
#we can see the line curve as below
plot(training_gm_dataset$Price,type="l",lty=1.8,col="green")
lines(training_gm_dataset$predictedPrice,type="l",col="blue")

#most of the lines are overlapping meaning our model "fit4" is a good model based on the predictors given to us in gm_dataset for training_gm_dataset

#Assumptions test of our Model fit4

####1.  Normality test for model
hist(training_gm_dataset$error,main = "Normality check for our model (training dataset)", xlab="Residuals",col="orange")


####2.  Independence of observations 
plot(training_gm_dataset$obsno,training_gm_dataset$error,main="Independence of  error for model (training dataset)",xlab= " obsv no", ylab="residuals",col="red")


####3 Check of linear relationship 
plot(training_gm_dataset$Mileage,training_gm_dataset$Price,main="Linear Rltnship for model (trainingDataset)",xlab="Mileage",ylab="Price",col="red")
plot(training_gm_dataset$Cylinder,training_gm_dataset$Price,main="Linear Rltnship for model (training dataset)",xlab="Cylinder ",ylab=" Price ",col="red")
plot(training_gm_dataset$Make,training_gm_dataset$Price,main="Linear Rltnship for model (training dataset)",xlab="Make ",ylab="Price ",col="red")


####4 Check of Constant Error Variance : Homoscedacity
plot(training_gm_dataset$predictedPrice,training_gm_dataset$error,main="Constant error variance for model (training dataset)",xlab="Predited Price",ylab="errors",abline(h=0),col="red")


# for finding the confidence intervals & the predited values for model for training dataset
confint(fit4)
predicted_training<- predict(fit4, interval="confidence") #predicted with lower & upper limit of prediction by model "fit4"
dim(predicted_training)
predicted_training<-as.data.frame(predicted_training)


# comparing the predicted values from "fit4 & the actual values of Price in  training_gm_dataset in the dataframe
predicted_training$actualtrainingsprice<-training_gm_dataset$Price
head(predicted_training)  # now with actual sales values from training_gm_dataset

# Firstly Checking basics details about testing_sales_dataset
testing_gm_dataset
dim(testing_gm_dataset)
summary(testing_gm_dataset)

#Verifying the "fit4" model for our testing dataset

#Verifying our model on testing data set so that we get to know the accuracy and predicted values of our testing dataset and compare them with the actual price values in testing dataset
#Finding the predicted Value of price through the model made  "fit4"

predict_testing<-predict(fit4,testing_gm_dataset)
head(predict_testing)
testing_gm_dataset$predicted_testing_Price <- predict_testing
dim(testing_gm_dataset)

# error values of sales through the model is below. These values are giving us the residuals/errors 
#i.e difference between predicted and Actual sales
error_Price<-testing_gm_dataset$Price - testing_gm_dataset$predicted_testing_Price
testing_gm_dataset$error_Price<-error_Price

# Adding observation No. Column n our dataset
testing_gm_dataset$obsno<-c(1:124)

head(testing_gm_dataset)

#comparing model predicted values with actual values of Price using the graph for testing_gm_dataset
#we can see the line curve as below
plot(testing_gm_dataset$Price,type="l",lty=1.8,col="brown")
lines(testing_gm_dataset$predicted_testing_Price,type="l",col="blue")



### MULTIVARIATE REGRESSION MODEL _ GENERAL MOTORS DATASET 
