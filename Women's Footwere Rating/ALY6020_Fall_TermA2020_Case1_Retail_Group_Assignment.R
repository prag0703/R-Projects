#File Name: ALY6020_Fall_TermA2020_Case1_Retail_Group_Assignment

#Case 1 - Retail

#Group Assignment 
# 1. Pragati Koladiya
# 2. Munitha Priyanka Kanukollu
# 3. Tanvi Bhagat 

#Model implementation:
#       KNN
#       Naive Bayes
#       Decision Tree
#       Random Forest
#       Time Series Forecasting
#         - Exponential smoothing, 
#         - Naive, 
#         - Mean forecast
# Goal: Predict the customer rating for footwear store. 
# Dataset: Data set is chosen from multiple source and store with file name "Shopper_Stop_Footwear.csv"

#--------------------------------------Importing the packages-------------------

#Clear the work space.
rm(list=ls())

library(broom)
library(caret)
library(caTools)
library(class)
library(corrgram)
library(corrplot)
library(data.tree)
library(depth)
library(devtools)
library(docstring)
library(dplyr)
library(e1071)
library(forecast)
library(FSelector)
library(fUnitRoots)
library(gbm)
library(gdata)
library(GGally)
library(ggcorrplot)
library(ggfortify)
library(ggplot2)
library(glmnet)
library(gmodels)
library(gridExtra)
library(lattice)
library(lubridate)
library(moderndive)
library(plotly)
library(randomForest)
library(RColorBrewer)
library(readr)
library(readxl)
library(readr)
library(reprex)
library(rgl)
library(rpart.plot)
library(rpart)
library(seriation)
library(stats)
library(tibble)
library(tidyr)
library(timetk)
library(tree)
library(tseries)
library(xts)
library(lmtest)
library(FitAR)
library(forecast)

#--------------------Reading the data file------------------------
SSdata<- read.csv("/Users/pragatikoladiya/OneDrive - Northeastern University/Q3-First/Predictive_Analytics/GroupAssignment-week6/FinalGroup/Shopper_Stop_Footwear.csv")
SSdata

#--------------------Data Dictionary------------------------
## Dataset Description

# Dataset is about Shopper Stop which sales different brands footwear.
# Data has been collected from 2015-1-1 to 2019-12-31 

# SrNo - the series number
# CalanderDate - the dates of calender from 2015-1-1 to 2019-12-31
# Sales -  sales on given day
# Categories - categories of different footwear
# Brand - Footwear brands name
# Colors - Colors of the footwear
# Size - size of footwear available from 37 to 41
# Discount - Discount is offered or not 
# Customer_Return - Customers return is done on a given day or not
# Rating - Customer rating is between 1 to 5

#----------------------------Data Exploration-------------------------------
View(SSdata)
str(SSdata)
summary(SSdata)

sapply(SSdata, function(x) length(unique(x)))

#----------------------------EXPLORATERY DATA ANALYSIS(EDA)----------------------

# Distribution of Customer Return
pie <- ggplot(SSdata, aes(x = "", fill = factor(Customer_Return))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5,size=22)) + 
  labs(fill="Customer Return", 
       title="Pie Chart of Customer Return ")

pie + coord_polar(theta = "y", start=0)

#Number of products on discount
SSdata %>% 
  ggplot(aes(x = Discount, fill = Discount)) +  geom_bar(aes(fill = Discount))+
  labs(title = 'Number of products on discount') + 
  scale_fill_brewer(palette = "Set3")

#Distribution of customer raiting using bar plot
table(SSdata$Rating)
SSdata %>% group_by(qui=factor(Rating))%>%summarise(floor_cnt=n())%>%
  ggplot(aes(x=qui,floor_cnt,fill=qui))+
  geom_bar(stat="identity",alpha=0.5)+
  scale_fill_manual(values =rainbow(8, start= .2, end = .96))+
  theme(legend.position="none")+
  labs(x="Rating (On the scale 1 to 5)",y="Count")+
  ggtitle("Customer product rating")

#Category Vs Sales
qplot(Sales, Categories, data = SSdata, geom = c("point"), xlab = "Sales", ylab = "Categories" , size = factor(Categories), color = factor(Categories)) + 
  scale_size_manual(values = c(1,2,3,4,5), name="Categories") + 
  scale_colour_manual(values = c("yellowgreen","yellow","deeppink","darkorchid1","darkolivegreen1"), name="Categories")

#Footwear count by categories grouped by Customer Return
cbPalette <- c("#B5EAD7", "#E2F0CB")
SSdata %>% 
  ggplot(aes(x = Categories)) +  geom_bar(aes(fill = Customer_Return))+
  labs(title = 'Footwear count by categories grouped by Customer Return') + 
  scale_fill_manual(values=cbPalette)

#Customer return w.r.t gender
cbPalette <- c("#B5EAD7", "#E2F0CB")
SSdata %>% 
  ggplot(aes(x = CategoryByGender)) +  geom_bar(aes(fill = Customer_Return))+
  labs(title = 'categories by gender grouped by Customer Return') + 
  scale_fill_manual(values=cbPalette)


#Footwear count by Brand grouped by Customer Return
SSdata %>% 
  ggplot(aes(x = Brand)) +  geom_bar(aes(fill = Customer_Return))+
  labs(title = 'Footwear count by Brands grouped by Customer Return') + 
  scale_fill_brewer(palette = 2)


#Footwear count by Brand grouped by Customer Return
SSdata %>% 
  ggplot(aes(x = Brand)) +  geom_bar(aes(fill = Discount))+
  labs(title = 'Footwear count by Brands grouped by discount') + 
  scale_fill_brewer(palette = "Pastel1")


#Product return w.r.t the rating.
cbPalette <- c("#FF9AA2", "#FFB7B2")
SSdata %>% 
  ggplot(aes(x = Rating)) +  geom_bar(aes(fill = Customer_Return))+
  labs(title = 'Product return w.r.t the rating') + 
  scale_fill_manual(values=cbPalette)

#Categories count w.r.t Customer Return
SSdata <- ggplot(SSdata, aes(Categories))
SSdata+ geom_bar(aes(fill = Customer_Return), position = "dodge")+
  labs(title = 'Categories count w.r.t Customer Return') + 
  scale_fill_brewer(palette = 11)

#check
SSdata <- ggplot(SSdata, aes(Categories))
SSdata+ geom_bar(aes(fill = In_Stock), position = "dodge")+
  labs(title = 'Categories count w.r.t Customer Return') + 
  scale_fill_brewer(palette = 11)

#Categories Vs Customer return
SSdata %>%
  ggplot(aes(x = Categories, fill = Customer_Return)) +
  geom_bar(aes(fill = Customer_Return))+
  labs(title = 'Categories Vs Customer return') + 
  scale_fill_brewer(palette = 14)

#----------------------------Data Cleaning and manipulation---------------------
SSdata<- read.csv("/Users/pragatikoladiya/OneDrive - Northeastern University/Q3-First/Predictive_Analytics/GroupAssignment-week6/FinalGroup/Shopper_Stop_Footwear.csv")
#checking for null values
sum(is.na(SSdata))

SSdata$CategoryByGender <- factor(SSdata$CategoryByGender, levels=c('Men','Women'),
                            labels=c(0,1))
table(SSdata$CategoryByGender)
typeof(SSdata$CategoryByGender)

SSdata$Categories <- factor(SSdata$Categories, levels=c('Flip Flops','Stilettos','Sneakers','Boots','Wedges'),
                            labels=c(1,2,3,4,5))
table(SSdata$Categories)
typeof(SSdata$Categories)

SSdata$Colors <- factor(SSdata$Colors, levels=c('White','Red','Black','Blue','Pink'),
                        labels=c(1,2,3,4,5))
table(SSdata$Colors)
typeof(SSdata$Colors)

SSdata$Brand <- factor(SSdata$Brand, levels=c('Tommy Hilfiger','Aldo','Adidas','Coach','Kate Spade'),
                       labels=c(1,2,3,4,5))
table(SSdata$Brand)
typeof(SSdata$Brand)


SSdata$Customer_Return <- factor(SSdata$Customer_Return, levels=c('No','Yes'),
                                 labels=c(0,1))
table(SSdata$Customer_Return)
typeof(SSdata$Customer_Return)

SSdata$In_Stock <- factor(SSdata$In_Stock, levels=c('No','Yes'),
                          labels=c(0,1))
table(SSdata$In_Stock)
typeof(SSdata$In_Stock)

SSdata$Discount <- factor(SSdata$Discount, levels=c('No','Yes'),
                          labels=c(0,1))
table(SSdata$Discount)
typeof(SSdata$Discount)

#Dropping 
SSdata =SSdata[,!names(SSdata) %in% 'SrNo']
SSdata =SSdata[,!names(SSdata) %in% 'CalendarDate']

head(SSdata)

SS_Num_Data <- as.data.frame(apply(SSdata, 2, as.numeric))
str(SS_Num_Data)

#Catagrizing rating column into 0, 1 
#As mentined above if rating is above 3 it will be 1 and less than that will be 0.
SS_Num_Data$Rating_categorical <- ifelse(SS_Num_Data$Rating>3, 1,0)
head(SS_Num_Data)

# Correlation plot of each variable in housing dataset
corr<-round(cor(SS_Num_Data),1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("red", "white", "blue"), 
           title="Correlogram of Diabetes data", 
           ggtheme=theme_bw)

#--------------------------------Implementing KNN Model-------------------------
sapply(SS_Num_Data, function(x) sum(is.na(x)))

#Dropping 
SS_Num_Data =SS_Num_Data[,!names(SS_Num_Data) %in% 'Rating']

#splitting the data into train and test with 70:30 ratio.
set.seed(101)
sample <- sample.split(SS_Num_Data$Rating_categorical,SplitRatio = 0.70)
train <- subset(SS_Num_Data,sample==TRUE)
test <- subset(SS_Num_Data,sample==FALSE)

head(SS_Num_Data)
dim(train)
dim(test)

#KNN implementation
predicted.Rating <- knn(train[1:10],test[1:10],train$Rating_categorical,k=1)

#Error in prediction
error <- mean(predicted.Rating!=test$Rating_categorical)

#Let us predict by using different K values from 1 to 10
predicted.Rating <- NULL
error.rate <- NULL

#checking the model fork values from 1 to 10.
for (i in 1:10) {
  predicted.Rating <- knn(train[1:7],test[1:7],train$Rating_categorical,k=i)
  error.rate[i] <- mean(predicted.Rating!=test$Rating_categorical)
}

head(predicted.Rating)
knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))

#Visualizing the best K value 
ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')


#----------------------------Implementing Naive Bayes Model---------------------
#converting Customer raiting 1 to 3 as 0 and 4,5 as 1.
# Encoding the target feature as factor
SS_Num_Data$Rating_categorical = factor(SS_Num_Data$Rating_categorical, levels = c(0, 1))
str(SS_Num_Data)
head(SS_Num_Data$Rating_categorical)
#Dropping 
SS_Num_Data =SS_Num_Data[,!names(SS_Num_Data) %in% 'Rating']
SS_Num_Data =SS_Num_Data[,!names(SS_Num_Data) %in% 'Customer_Return']

set.seed(123)
split = sample.split(SS_Num_Data$Rating_categorical, SplitRatio = 0.75)
training_set = subset(SS_Num_Data, split == TRUE)
test_set = subset(SS_Num_Data, split == FALSE)

#Viewing the dimention of train and test set
dim(training_set)
dim(test_set)

head(SS_Num_Data)
dim(training_set)
head(training_set)
dim(test_set)
head(test_set)

#implementing naivebayes model
NaiveBayes = naiveBayes(Rating_categorical ~ . ,data = training_set)
NaiveBayes

# Predicting the Test set results
prediction <-predict(NaiveBayes, test_set)

#Model evaluation using Confusion Matrix
ConfusionMatrix = table(test_set[,9], prediction)
ConfusionMatrix

CrossTable(prediction, test_set[,9],
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

Accuracy = (0+316)/(0+316+139+2)
Accuracy

#--> Naive Bayes Model Accuracy = 69.1%

#----------------------------DECISION TREE-----------------
#Dropping 
SS_Num_Data =SS_Num_Data[,!names(SS_Num_Data) %in% 'Customer_Return']

set.seed(123)
sample = sample.split(SS_Num_Data$Rating_categorical, SplitRatio = .80)
train = subset(SS_Num_Data, sample == TRUE)
test = subset(SS_Num_Data, sample == FALSE)

#view dimention of data frame train and test 
dim(train)
dim(test)

#View(test)

#Training the decision tree classifier 
DecisionTree <- rpart(Rating_categorical ~ ., data = train, method = 'class')
rpart.plot(DecisionTree)

#predictions
DT_prediction <- predict(DecisionTree, newdata = test, type = 'class')

#confusion matrix for evaluating the model
confusionMatrix(table(DT_prediction ,test$Rating_categorical))


coeffs <- summary(DecisionTree)$coefficients
View(data.frame(DT_prediction, test$Rating_categorical))
CrossTable(x = DT_prediction, y = test$Rating_categorical,
           prop.chisq = FALSE)

Accuracy = (14+238)/(4+238+16+97)
Accuracy

#--> Decision Tree Model Accuracy = 70.9%

#----------------------------Random Forest------------------------------

set.seed(123)
sample = sample.split(SS_Num_Data$Rating_categorical, SplitRatio = .80)
train = subset(SS_Num_Data, sample == TRUE)
test = subset(SS_Num_Data, sample == FALSE)

head(SS_Num_Data)

# Random forest
RandomForest <- randomForest(Rating_categorical ~., data = train, mtry=4, ntree=2000, importance=TRUE)
plot(RandomForest)

prediction <-predict(RandomForest, test)

#confusion matrix for evaluating the model
ConfusionMatrix = table(test[,9], prediction)
ConfusionMatrix

result <- data.frame(train$Rating_categorical, predict(RandomForest, train, type="response"))
#View(data.frame(prediction, train$Rating_categorical))
result

Accuracy = (1+301)/(1+0+63+301)
Accuracy

#--> Random Forest Accuracy = 82.7%

#----------------------------Implementing Time Series----------------------
#Loading the data again as we have mentined earlier for time series 
#we will use data column which we have droped to implement other models 

SSdata <- read_excel("/Users/pragatikoladiya/OneDrive - Northeastern University/Q3-First/Predictive_Analytics/GroupAssignment-week6/FinalGroup/Time Series Excel.xlsx")
str(SSdata)
summary(SSdata$CalendarDate)

SSdata$Date  <- as.Date(SSdata$CalendarDate, format = "%Y%m/%d")
summary(SSdata$Date)

#Before we begin any analysis, we will be splitting the data to remove 2019 to use as our test set.
SSdata_train <- ts(SSdata$Sales, start=c(2015, 01), end=c(2019, 12), freq=12)
plot.ts(SSdata_train,main = "Monthly sales for SS training data", xlab = "Year", ylab = "Closing values")
#Boxplot for monthly trend:
boxplot(SSdata_train ~ cycle(SSdata_train), xlab = "Month", 
        ylab = "Closing values", main = "Monthly sales for ss training data- Boxplot",
        col=rainbow(10,alpha =0.4))


#Quarterly Trend:
SSdata_train.qtr <- aggregate(SSdata_train, nfrequency=4)
plot.ts(SSdata_train.qtr,main = "Quarterly sales for SS training data", xlab = "Year", ylab = "Closing values")
#Boxplot for quarterly trend:
boxplot(SSdata_train.qtr ~ cycle(SSdata_train.qtr), xlab = "Quarterly", 
        ylab = "Closing values", main = "Quarterly sales for ss training data- Boxplot",
        col=rainbow(10,alpha =0.4))

#Yearly trend:
SSdata_train.yr <- aggregate(SSdata_train, nfrequency=1)
plot.ts(SSdata_train.yr,main = "Yearly sales for ss training data", xlab = "Year", ylab = "Closing values")
#Boxplot for yearly trend:
boxplot(SSdata_train.yr ~ cycle(SSdata_train.yr), xlab = "Yearly", 
        ylab = "Closing values", main = "Yearly sales for ss training data- Boxplot",
        col=rainbow(10,alpha =0.4))


#Decompose of data to break down into seasonal component, trend, and residuals.
SSdata_train.decomp <- decompose(SSdata_train)
plot(SSdata_train.decomp, yax.flip = TRUE)

#Seasonality trend per every year:
ggseasonplot(SSdata_train, year.labels=TRUE, year.labels.left=FALSE)  +
  ggtitle("Seasonal plot: SS Training data")

#Transforming our data to adjust for non-stationary
#Using Diff() function.
tsDiff <- diff(SSdata_train)
plot.ts(tsDiff, xlab = "years",
        ylab = "Closing values",main="Plot for Diference in Time series")

# To adjust the Seasonality
urkpssTest(SSdata_train, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)

#Auto Arima Model Fitting
#To find the best model out of different orders
autoarima <-auto.arima(SSdata_train, trace=TRUE)
#Plotting for the ARIMA model with residuals, auto correlation and p-values.
ggtsdiag(auto.arima(SSdata_train))


#Considering the order(1,0,0) as the best model
fitARIMA <- arima(SSdata_train, order=c(1,0,0),seasonal = list(order = c(0,0,1), period = 12),method="ML")
fitARIMA
accuracy(fitARIMA)

#Predicting the next 5 months sales
predict(fitARIMA,n.ahead = 5)
#Coefficient values:
coeftest(fitARIMA) 
confint(fitARIMA)
acf(fitARIMA$residuals)


#Observing the white noise for forecast
boxresult<-LjungBoxTest (fitARIMA$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)
#Residuals plot:
residFit <- ggplot(data=fitARIMA, aes(residuals(fitARIMA))) + 
  geom_histogram(aes(y =..density..),  
                 binwidth = 1000,
                 col="turquoise4", fill="#FF000099") +
  geom_density(col="blue") +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot for ARIMA Model Residuals") 

residFit

#Forecasting
futurVal <- forecast(fitARIMA,h=12, level=c(99.5))
plot(futurVal)

#Testing the data
create_ts <- function(x){
  ts(x, start = c(2019, 1), frequency = 6)
}
SSdata_test <- lapply(SSdata, create_ts)
SSdata_testing <-ts(SSdata_test$Sales, start =c(2019,1), frequency = 6)
SSdata_testing <- ts(SSdata_test$Sales, start=c(2019, 1), end=c(2019, 12), freq=6)

#Comparision with other models:
#Exponential Smoothing:
Es.fit.f <- forecast(SSdata_train, h = 6)
#Accuracy
accuracy(Es.fit.f)
#PLOT:
Es <- autoplot(Es.fit.f, 
               holdout = SSdata_testing,
               forc_name = 'forecast',
               ts_object_name = 'SS_data',main = "Exponential Smoothing- forecast for ss data")

ggplotly(Es)

#MeanF Method:
Mean.fit.a <- meanf(SSdata_train, h = 6)
#Accuracy
accuracy(Mean.fit.a)
#PLOT:
a <- autoplot(Mean.fit.a, 
              holdout = SSdata_testing,
              forc_name = 'Mean',
              ts_object_name = 'SS_data',main = "Mean forecast for ss data")

ggplotly(a)

#Naive Method:
naive.fit.n <- naive(SSdata_train, h = 6)
#Accuracy
accuracy(naive.fit.n)
#PLOT:
N <- autoplot(naive.fit.n, 
              holdout = SSdata_testing,
              forc_name = 'Naive',
              ts_object_name = 'SS_data',main = "Naive forecast for ss data") 

ggplotly(N)

# From the above models, the root mean sqaure error performance is better than the arima model.

