# File Name: ALY6020_Fall_TermA2020_Pragati_Koladiya_Assignment1
# Assignment 1 
# Goal: Predict the selling prices of houses in the region. 
# Dataset Information: Data set is stored in the file name "Housing"

#--------------------------------------Importing the packages-------------------
#install.packages('caTools')
library(caTools)
library(ggplot2)
library(stats)
#install.packages("dplyr")
library(dplyr)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("GGally")
library("GGally")
library(readr)
library(seriation) 
#install.packages("moderndive")
library(moderndive)
#install.packages("corrgram")
library(corrgram)
library(RColorBrewer)
library(lattice)

#--------------------------------------Importing CSV ---------------------------
#reading the data file name Housing.csv
Housing <- read_csv("/Users/pragatikoladiya/OneDrive - Northeastern University/Q3-First/Predictive_Analytics/Housing.csv")

#print head of the dataset 
head(Housing)

#structure of the data
str(Housing)
#Data Dictionary:

# 1. No. - Series number
# 2. price- The price of the house.
# 3. Lotsize - Area of the house in SQFT(Square Feet)
# 4. bedrooms - Number of bedrooms
# 5. bathrms - Number of bathrooms 
# 6. stories - Number of storage units
# Categorical variables with values Yes or no
# driveway, recroom, fullbase, gashw, airco, garagepl(Unique values are 0 and 1), and prefarea

#summary of the data
summary(Housing)

#----------------------------Exploratory Data Analysis(EDA)---------------------

#House price distribution using histogram
hist(Housing_Num_Data$price,
     data = Housing_Num_Data,
     main ='House price distribution',
     xlab='Price',
     ylab= 'Frequency',
     col = 'pink',
     bins = 10
)
#--> Most of the house price lies between 50k to 100k

#lotsize distribution using histogram
hist(Housing_Num_Data$lotsize,
     data = Housing_Num_Data,
     main ='Distribution of lotsize',
     xlab='lotsize',
     ylab= 'Frequency',
     col = 'orchid4',
     bins = 10
)
# Distribution of lot size of all the dataset
ggplot(Housing, aes(lotsize)) + geom_histogram(aes(fill = lotsize), color = "darkorchid4",
                                               binwidth = 2)


#Density plot for price and lotsize
options(scipen=999)  #option controls how many digits are printed
options(repr.plot.width=6, repr.plot.height=3,align="center") 
# Distribution of Price and lot size
g1<-ggplot(Housing,aes(x=price))+geom_density(fill="paleturquoise4")
g2<-ggplot(Housing,aes(x=lotsize))+geom_density(fill="paleturquoise3")
grid.arrange(g1,g2,nrow=1,ncol=2)
#--> Distribution of house prices & lotsize was right skewed 

#bedrooms

#Box plot for price vs bedrooms
ggplot(data = Housing) +
  geom_boxplot(mapping = aes(x=Housing$bedrooms, y=Housing$price ), fill="powderblue" ) +
  xlab("Bedroom") +
  ylab("Price") +
  scale_x_discrete(breaks=NULL) +
  ggtitle("Price Vs bedroom")
  coord_flip()
  
#Price Vs number of bedrooms
options(repr.plot.width=6, repr.plot.height=3) #set the size of the plot
mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6)) #RColorBrewer package to use my own color choice
ggplot(Housing,aes(x=bedrooms,y=price,col=bedrooms))+
  geom_point(alpha=0.5,size=2)+
  geom_smooth(method="lm",se=F)+
  labs("title=Sqft Living vs Price")+
  scale_color_gradientn(colors=mycolors)+
  theme(legend.position="none")

#Batherooms
ggplot(Housing, aes(x=bathrms)) + 
  coord_cartesian(xlim=c(0,4)) +
  geom_bar(fill="darkgoldenrod2")+
  ggtitle("Bathrooms frequency count")

#fullbasement and price
ggplot(Housing,aes(factor(fullbase),price,fill=factor(fullbase)))+
  geom_boxplot(alpha=0.6)+scale_fill_manual(values=rainbow(6))+
  theme(legend.position="none")+
  ggtitle("Price Vs Full basement")
  labs(x="Full basement")

#Stories Vs Price
ggplot(Housing_Num_Data,aes(factor(stories),price,fill=factor(stories)))+
  geom_boxplot(alpha=0.6)+scale_fill_manual(values=rainbow(12))+
  theme(legend.position="none")+
  ggtitle("Price Vs Stories")
  labs(x="stories")

#Number of houses by stories
table(Housing_Num_Data$stories)
Housing_Num_Data %>% group_by(stor=factor(stories))%>%summarise(floor_cnt=n())%>%
  ggplot(aes(x=stor,floor_cnt,fill=stor))+
  geom_bar(stat="identity",alpha=0.5)+
  scale_fill_manual(values =rainbow(n=6))+
  theme(legend.position="none")+
  labs(x="Stories",y="Number of houses")+
  ggtitle("Number of house by stories")

#Price Vs garage palace
ggplot(Housing_Num_Data,aes(factor(garagepl),price,fill=factor(garagepl)))+
  geom_boxplot(alpha=0.6)+scale_fill_manual(values=rainbow(6))+
  theme(legend.position="none")+
  labs(x="Garage places")

# Price by SQFT
price_per_sqft <- Housing$price/Housing$lotsize
head(price_per_sqft)
plot(price_per_sqft, main = 'Price by Sqft', xlab = 'Lot size(square feet)', ylab = 'Price', col = 'hotpink4')


#----------------------------Data Cleaning and manipulation---------------------
#checking for null values
sum(is.na(Housing))

# converting string to number 
#converts 0's to no and 1's to yes
Housing$driveway <- factor(Housing$driveway, levels=c('no','yes'),
                           labels=c(0,1))
table(Housing$driveway)
typeof(Housing$driveway)

Housing$recroom <- factor(Housing$recroom, levels=c('no','yes'),
                          labels=c(0,1))
table(Housing$recroom)
typeof(Housing$recroom)

Housing$fullbase <- factor(Housing$fullbase, levels=c('no','yes'),
                           labels=c(0,1))
table(Housing$fullbase)
typeof(Housing$fullbase)

Housing$gashw <- factor(Housing$gashw, levels=c('no','yes'),
                        labels=c(0,1))
table(Housing$gashw)
typeof(Housing$gashw)

Housing$airco <- factor(Housing$airco, levels=c('no','yes'),
                        labels=c(0,1))
table(Housing$airco)
typeof(Housing$airco)

Housing$prefarea <- factor(Housing$prefarea, levels=c('no','yes'),
                           labels=c(0,1))
table(Housing$prefarea)
typeof(Housing$prefarea)

Housing_Num_Data <- as.data.frame(apply(Housing, 2, as.numeric))
Housing_Num_Data <- Housing_Num_Data[,-1]
head(Housing_Num_Data)
str(Housing_Num_Data)

# Correlation plot of each variable in housing dataset
corrgram(Housing_Num_Data, lower.panel=panel.shade, upper.panel=NULL, test.panel=panel.txt, main ="Correlation between each varaible")

#----------------------------Implementing linear Regression Model 1-------------
# Considering independent variable X = lotsize, bedrooms, bathrms, stories, driveway, recroom, fullbase, gashw, airco, garagepl, prefarea
# Considering dependent variable Y = price

set.seed(1234)
split_data <- sample(2,nrow(Housing_Num_Data),prob=c(0.70,0.30),replace=TRUE)
train <- Housing_Num_Data[split_data==1,]
test <- Housing_Num_Data[split_data==2,]
str(train)
nrow(train)
nrow(test)
#View(train)

#cor(train$price, train$lotsize)

#creating the baseline model
ModelFit1 <- lm(price ~ ., data = train)
result <- predict(ModelFit1, newdata = test, type = "response")
View(result)
range(result)

#table is created to view the confusion matrix in out case the price is not a categorical variable
#The confusion matrix displays in form of table. 
#i have given the threshold value greater than 119k meaning if the predicted value is lower then threashold
#it will considered as a false
#table(test$price, result>119000.00)
#displaying range of predicted price

#regresstion table for model fit 
get_regression_table(ModelFit1)

#Summary of model
summary(ModelFit1)

#Round the Coefficients Table
coeffs <- summary(ModelFit1)$coefficients
coeffs <- round(coeffs, 4)
coeffs

#ploting fitted model with regression line
ggplot(data = Housing_Num_Data, aes(x = predict(ModelFit1,Housing_Num_Data), y = price)) +
  geom_point() +
  stat_smooth(method = "lm", col = "olivedrab2") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Multiple inear Model-1 fitted to data using all variables")


#----------------------------Implementing linear Regression Model 2-------------
# Considering independent variable X = lotsize, bedrooms,  bathrmS, storieS, driveway, airco, garagepl and prefarea
# Considering dependent variable Y = price
split_data <- sample(2,nrow(Housing_Num_Data),prob=c(0.70,0.30),replace=TRUE)
train2 <- Housing_Num_Data[split_data==1,]
test2 <- Housing_Num_Data[split_data==2,]

#implementing model with 8 variables
ModelFit2 <- lm(price ~ lotsize + bedrooms + bathrms + stories + driveway + airco + garagepl + prefarea, data = train2)
#summary of the model
summary(ModelFit2)

#Coefficients of model2
coeffs <- summary(ModelFit2)$coefficients
coeffs <- round(coeffs, 4)
coeffs

#plot(Housing_Num_Data$lotsize, Housing_Num  _Data$price)
#plot(predict(ModelFit2,Housing_Num_Data), Housing_Num_Data$price, pch = 16, col="deeppink1", trans.val = .8)

ggplot(data = Housing_Num_Data, aes(x = predict(ModelFit2,Housing_Num_Data), y = price)) +
  geom_point() +
  stat_smooth(method = "lm", col = "indianred2") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Multiple inear Model-2 fitted to data with eight varaibles")

# Predicting the house price that you should be bidding for the house in the specified regio.
# Assuming the given input values are,
# lotsize 5500, bedroom = 4, bathroom = 2 and recroom = 2.
predict(ModelFit2, data.frame(lotsize= 5500,bedrooms=4,bathrms=2,stories=1,driveway=1,airco=1, garagepl=1, prefarea=0))
#output will be the predicted bidding price for the house based on the given conditions of input varaibles
