##Loading libraries



library(corrplot)
library(car)
library(perturb)
library(ggplot2)
library(MASS)

#Reading Data file
forest <- read.csv("forestfires.csv")

# Dimensions and variables of dataset
dim(forest)
#517rows and  13 columns
colnames(forest)
#"X"     "Y"     "month" "day"   "FFMC"  "DMC"   "DC"    "ISI"   "temp"  "RH"    "wind"  "rain"  "area"

#checking summary of variables and if any missing values
sum(is.na(forest))
summary(forest)


# No missing values. Month and day are as factors. From Summary FFMC, DMC and DC seem left skewed.
#ISI , Rain may be right skewed. Area heavily right skewed

# Splitting data into training and test set
set.seed(30032017)
row.number<- sample(1:nrow(forest), size=0.2*nrow(forest))
forest_test<-  forest[row.number,]
dim(forest_test) ## Size of the testing set
forest_train<- forest[-row.number,]
dim(forest_train)  ## Size of training set
summary(forest_train)

# Now we check the correlation matrix
M <- cor(forest_train[,-c(3,4)])
M

# And the correlation plot to visualize the correlation between variables in training data
corrplot(M,method='number')

##evident positive corr between DC & DMC,ISI &FFMC,X &Y, temp&DC.
#negative corr between RH& temp.

# lets see it visually by scatter plots

pairs(forest_train[,-c(3,4)])

#visual scatter plots rule out some correlation and we can shortlist below ones
#positive DC&DMC - this as per definition makes sense 
#positive temp &DMC - This somewhat does not makes much sense as moisture should decrease with temp
#neagtive RH & temp - This is also natural as temp increases humidity decreases.

# We then inspect the distribution of each variable in boxplots

boxplot(forest_train$X,main="X")
boxplot(forest_train$Y,main ='Y')
boxplot(forest_train$FFMC, main='FFMC') #outliers
boxplot(forest_train$DMC, main ='DMC') # outliers
boxplot(forest_train$DC, main='DC') # some outliers
boxplot(forest_train$ISI,main='ISI') # outliers
boxplot(forest_train$temp, main='temp') 
boxplot(forest_train$RH,main="RH") # outliers
boxplot(forest_train$wind, main='wind') #
boxplot(forest_train$rain, main='rain')  # heavy outliers...high variability in data
boxplot(forest_train$area, main='area') # heavy outliers..high variability in data

# asymmetry also observed in variables like X,Y,DC,FFMC

# Let's see the prob density distribution curve of response variable area

dar <- data.frame(x=forest_train$area)
ggplot(dar,aes(x=forest_train$area))+geom_density(fill='red')


# And density curve for other variables also
plot(density(forest_train$FFMC))
plot(density(forest_train$DMC))
plot(density(forest_train$DC))
plot(density(forest_train$ISI))
plot(density(forest_train$temp))   
plot(density(forest_train$RH))
plot(density(forest_train$wind))
plot(density(forest_train$rain))
plot(density(forest_train$area))

plot(density(log(forest_train$rain))) # log
plot(density(log(forest_train$area))) # log

## Above boxplots and density suggest reflected log transform of FFMC and log transform of rain
##and area, the response variable since it is highly  concentrated near zero and assymetrical 


summary(forest_train$area)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    0.00    0.19   14.04    6.25 1091.00
var(forest_train$area)
#4994.227
sd(forest_train$area)
#70.66984

# The variable distribution is very concentrated around 1 and 10, but we can
# some extreme outliers , even above 1000 !! 


# We run the first  Basic Model

mod1 <- lm(area~X+Y+month+day+FFMC+DMC+DC+ISI+temp+RH+wind+rain,data=forest_train)

summary(mod1)

# The R sq is very low at 4.6% and only DMC and DC seems significant regressors
par(mfrow=c(2, 2))

plot(mod1)

qqplot(mod1)

#There is negative linear reation between Residuals and Fitted values
# QQ plot of residuals is also not linear.

# this indicates there can be Collinearity problems
# Lets see residual plot with variables.

residualPlots(mod1)

# The residual plots suggests very significant pattern for fitted values and residuals. 
# Some square transformations in wind, temp, rain, RH is suggested. 

## But ffirst we observe that there are many zero values in area which is giving very irregular 
## results. Hence , we decided to remove the zero value rows and reduce the dataset and actually run only on 
## data where there is a burn area.

forest_train <- forest_train[forest_train$area>0,]
forest_test <- forest_test[forest_test$area>0,]

# Model 2
## Now we run model 2 on reduced subset of data

mod2 <- lm(area~X+Y+month+day+FFMC+DMC+DC+ISI+temp+RH+wind+rain,data=forest_train)

summary(mod2)
# R sq has sugnificantly improved

# Lets see plots

plot(mod2)

residualPlots(mod2)

# the fitted values plot and qq plot has improved, now we proceed with other transformations


# Model 3
## As discussed earlier we transform FFMC and Rain
FFMC_ref<- (log(max(forest_train$FFMC)+1-forest_train$FFMC))

Rain_log <- log(forest_train$rain+1)

mod3 <- lm(area~X+Y+month+day+FFMC_ref+DMC+DC+ISI+temp+RH+wind+Rain_log,data=forest_train)

summary(mod3)

residualPlots(mod3)

## the model is improved on R sq and residuals also.DMC and DC have emerged as significant.
## We still need to improve patterns in fitted values and residuals.

# Model 4
## we check the box cox for response variable transform
bc<- boxcox(mod3)
bc_df = as.data.frame(bc)
optimal_lambda =  bc_df[which.max(bc$y),1]
optimal_lambda

## the optimal lambda is very near to zero. Hence log transform is suitable here (with area+1 to counter the zero values)

mod4 <- lm(log(area+1)~X+Y+month+day+FFMC_ref+DMC+DC+ISI+temp+RH+wind+Rain_log,data=forest_train)

summary(mod4)

residualPlots(mod4)

## The fitted values vs residual is random now and model prediction is also improved. 
## Lets transform other variables.

#MOdel 5

## Here we check the interaction of the the various forest Fire Weather Index (FWI) as they
## are closely related and may have larger inetraction impact.
FFMC.DMC <- forest_train$FFMC*forest_train$DMC
FFMC.DC <-forest_train$FFMC*forest_train$DC
FFMC.ISI <-forest_train$FFMC*forest_train$ISI
DMC.DC<-forest_train$DMC*forest_train$DC
DMC.ISI<-forest_train$DMC*forest_train$ISI
DC.ISI<-forest_train$DC*forest_train$ISI


mod5 <- lm(log(area+1)~X+Y+month+day+FFMC+DMC+DC+ISI+FFMC.DMC+FFMC.DC+FFMC.ISI+DMC.DC+DMC.ISI+DC.ISI+
             temp+RH+wind+Rain_log,data=forest_train)

summary(mod5)

plot(mod5)

residualPlots(mod5)

## The model has even improved and the normal quantile plot of residuals is much better normal now.


#model 6

## Lets try to do the residual improvement of other variables. Hence we try the square of variables
## which are dense distributed and show some quadratic pattern.Also factors like wind, temp 
## should have greater impact on fire spread and area

wind_sq<-(forest_train$wind)^2
temp_sq<-(forest_train$temp)^2
rain_sq<-(forest_train$rain)^2
RH_sq<-(forest_train$RH)^2

## We check the interaction also of these factors.
temp.RH<-(forest_train$temp)*(forest_train$RH)
wind.rain<-(forest_train$wind)*(forest_train$rain)
wind.temp<-(forest_train$wind)*(forest_train$temp)

mod6 <- lm(log(area+1)~X+Y+month+day+FFMC+DMC+DC+ISI+FFMC.DMC+FFMC.DC+FFMC.ISI+DMC.DC+DMC.ISI+DC.ISI+
             temp+temp_sq+RH+RH_sq+wind+wind_sq+Rain_log+rain_sq+temp.RH+wind.rain+wind.temp,data=forest_train)

summary(mod6)

plot(mod6)

residualPlots(mod6)

## the model is improved in terms of  r sq and the residuals are also randomly distributed.
## We are very near to optimal model.

## we do some formal checks of collienearity and influential observations(as there were outliers in dataset)

#checking collinearity 

colldiag(forest_train[,-c(3,4,13)])

## We find that there is no collinearity in the variables , so we are safe on this front.

#checking influential obs

influence.measures(mod6)
influenceIndexPlot(mod6,id.n=5)

#checking influential obs values
forest_train[which(row.names(forest_train) %in% c(200,363,416,479,480)),]

## out f these only two are high outliers 416 and and 480. These seem to be burned 
# due to some other factors, may be intentional !! So we remove two observations.

forest_train_new <- forest_train[which(!row.names(forest_train) %in% c(416,480)),]


#Model 7 with removed influential obs
# defining transform variables again with new dataset
wind_sq<-(forest_train_new$wind)^2
temp_sq<-(forest_train_new$temp)^2
rain_sq<-(forest_train_new$rain)^2
RH_sq<-(forest_train_new$RH)^2
Rain_log <- log(forest_train_new$rain+1)
FFMC_ref<- (log(max(forest_train_new$FFMC)+1-forest_train_new$FFMC))

temp.RH<-(forest_train_new$temp)*(forest_train_new$RH)
wind.rain<-(forest_train_new$wind)*(forest_train_new$rain)
wind.temp<-(forest_train_new$wind)*(forest_train_new$temp)

FFMC.DMC <- forest_train_new$FFMC*forest_train_new$DMC
FFMC.DC <-forest_train_new$FFMC*forest_train_new$DC
FFMC.ISI <-forest_train_new$FFMC*forest_train_new$ISI
DMC.DC<-forest_train_new$DMC*forest_train_new$DC
DMC.ISI<-forest_train_new$DMC*forest_train_new$ISI
DC.ISI<-forest_train_new$DC*forest_train_new$ISI

mod7 <- lm(log(area+1)~X+Y+month+day+FFMC_ref+DMC+DC+ISI+FFMC.DMC+FFMC.DC+FFMC.ISI+DMC.DC+DMC.ISI+DC.ISI+
             temp+temp_sq+RH+RH_sq+wind+wind_sq+Rain_log+rain_sq+temp.RH+wind.rain+wind.temp,data=forest_train_new)


summary(mod7)

residualPlots(mod7)
plot(mod7)

##There seem to be no deviation but , the model has too many insignificant regressors.
## Also R sq and adjusted R sq are very far apart.
## Lets run a step AIC to remove the insignificant variables.

#Model#8

stepAIC(mod7)

##running suggested model

mod8 <- lm(formula = log(area + 1) ~ X + Y + month + DMC + DC + FFMC.DMC + 
     FFMC.DC + FFMC.ISI + DC.ISI + RH + RH_sq, data = forest_train_new)

#mod8 = lm(formula = log(area + 1) ~ DC + temp + temp_sq + RH_sq + wind + 
#           Rain_log + temp.RH + wind.temp, data = forest_train_new)

summary(mod8)

plot(mod8)

residualPlots(mod8)

## Although the R sq is low , but that seems due to the predictors available in data and there 
#might be missing some important variables. But our other factors and plots show that this is the optimal model.


## Now we test the model
FFMC.DMC <- forest_test$FFMC*forest_test$DMC
FFMC.DC <-forest_test$FFMC*forest_test$DC
FFMC.ISI <-forest_test$FFMC*forest_test$ISI
DC.ISI<-forest_test$DC*forest_test$ISI
RH_sq<-(forest_test$RH)^2
testData<-cbind(forest_test,FFMC.DMC,FFMC.DC,FFMC.ISI,DC.ISI,RH_sq)

model <-lm(formula = log(area + 1) ~ X + Y + month + DMC + DC + FFMC.DMC + 
                FFMC.DC + FFMC.ISI + DC.ISI + RH + RH_sq, data = testData)


y_hat<-predict.lm(model,newdata=testData, se.fit=TRUE)$fit
y_hat<-as.vector(y_hat) 
dev<-log(testData$area+1)-(y_hat) 
num<-sum(dev^2) 
dev1<-log(testData$area+1)-mean(log(testData$area+1)) 
den<-sum(dev1^2) 
Predicted.Rsq<-1-(num/den) 
Predicted.Rsq

##The predicted R sqaure is 28.59%. This is a considerably good fit as per the given dataset.


## Running on original data

FFMC.DMC <- forest$FFMC*forest$DMC
FFMC.DC <-forest$FFMC*forest$DC
FFMC.ISI <-forest$FFMC*forest$ISI
DC.ISI<-forest$DC*forest$ISI
RH_sq<-(forest$RH)^2


forest_new<-cbind(forest,FFMC.DMC,FFMC.DC,FFMC.ISI,DC.ISI,RH_sq)
forest_new <- forest_new[forest_new$area>0,]


model_full <-lm(formula = log(area + 1) ~ X + Y + month + DMC + DC + FFMC.DMC + 
             FFMC.DC + FFMC.ISI + DC.ISI + RH + RH_sq, data = forest_new)

summary(model_full)
plot(model_full)
residualPlots(model_full)



