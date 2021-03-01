# File Name: ALY6020_Fall_TermA2020_Pragati_Koladiya_Assignment2_Part1_KNN
# Assignment 2 
#Part 1 - KNN model implementation
# Goal: Predict the airline pasanger satisfaction. 
# Dataset Information: Data set is chosen from kaggle and store with file name "AirlineSatisfaction.csv"

#--------------------------------------Importing the packages-------------------
#install.packages('caTools')
library(caTools)
library(ggplot2)
library(stats)
library(base)
#install.packages("dplyr")
library(dplyr)
#install.packages("gridExtra")
library(stats)
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
# import caret
#install.packages("caret")
library(caret)
#install.packages("gdata")
library(gmodels)
library(gdata)
library(class)

#--------------------------------------Importing CSV ---------------------------
air <- read_csv("/Users/pragatikoladiya/OneDrive - Northeastern University/Q3-First/Predictive_Analytics/AirlineSatisfaction.csv")

View(air) 
#structure of the data
str(air)
#print head of the dataset 
head(air)

#Data Dictionary:

#Gender:                            Gender of the passengers (Female, Male)
#Customer Type:                     The customer type (Loyal customer, disloyal customer)
#Age:                               The actual age of the passengers
#Type of Travel:                    Purpose of the flight of the passengers (Personal Travel, Business Travel)
#Class:                             Travel class in the plane of the passengers (Business, Eco, Eco Plus)
#Flight distance:                   The flight distance of this journey
#Inflight wifi service:             Satisfaction level of the inflight wifi service (raiting from 1-5)
#Departure/Arrival time convenient: Satisfaction level of Departure/Arrival time convenient
#Ease of Online booking:            Satisfaction level of online booking
#Gate location:                     Satisfaction level of Gate location
#Food and drink:                    Satisfaction level of Food and drink
#Online boarding:                   Satisfaction level of online boarding
#Seat comfort:                      Satisfaction level of Seat comfort
#Inflight entertainment:            Satisfaction level of inflight entertainment
#On-board service:                  Satisfaction level of On-board service
#Leg room service:                  Satisfaction level of Leg room service
#Baggage handling:                  Satisfaction level of baggage handling
#Check-in service:                  Satisfaction level of Check-in service
#Inflight service:                  Satisfaction level of inflight service
#Cleanliness:                       Satisfaction level of Cleanliness
#Departure Delay in Minutes:        Minutes delayed when departure
#Arrival Delay in Minutes:          Minutes delayed when Arrival
#Satisfaction:                      Airline satisfaction level(Satisfaction, neutral or dissatisfaction)

#----------------------------Exploratory Data Analysis(EDA)---------------------

# Age distribution using histogram
hist(air$Age,
     data = air,
     main ='Airtravels age distibution',
     xlab='Age',
     ylab= 'Frequency',
     col = 'pink',
     bins = 10
)
#=>	Distribution of  age is normal distribution. Most passengers are of age 20 to 80.

#	Gender wise distribution using bar plot
options(scipen=10000)   

genderDist  = ggplot(data = air) +
  geom_bar(mapping = aes(x = Gender, y = ..count.., fill = Gender)) +
  labs(title = 'Customers count by gender') + 
  scale_fill_brewer(palette = 8)
print(genderDist)
#=>	Customer count by gender is almost identical. 

#	Distribution of `FlightDistance` using histogram
ggplot(air, aes(FlightDistance), main="Distribution of flight distance") + geom_histogram(aes(fill = FlightDistance), color = "darkolivegreen3",
                                               binwidth = 1)
#=> -	The above graph shows number of times the distance covered by flights. 
#     Most of the distance covered by airline is between 100miles to 1000miles.  

#	Different purpose of travel Vs Satisfaction
travel = ggplot(data = air, aes(x = TypeofTravel, y = ..count.., fill = TypeofTravel)) + 
  geom_bar(stat = 'count') +
  scale_fill_brewer(palette = 11) +
  theme(legend.position="none", axis.text = element_text(size = 6)) +
  labs(title = 'Different type of travel(Business or Personal) Vs Satisfaction', y = 'Satisfaction', x = 'Type of travel', fill = 'TypeofTravel')
grid.arrange(travel, ncol=1)
#=>  -	Passengers who are traveling for business purpose has more satisfaction compare 
#     to those who are travel because of other purpose like for holiday they have less than half satisfaction of business travelers.   


# Distribution of different variables of flight service 
  
par(mfrow = c(2,2))
hist(air$Checkinservice,
     data = air,
     main ='Check in service raitings',
     xlab='Checkinservice',
     ylab= 'Frequency',
     col = 'lightcoral'
)
hist(air$Inflightservice,
      data = air,
      main ='In flight service raitings',
      xlab='Inflightservice',
      ylab= 'Frequency',
      col = 'lightblue3'
)
hist(air$Inflightwifiservice,
     data = air,
     main ='In flight wifi service raitings',
     xlab='Inflightwifiservice',
     ylab= 'Frequency',
     col = 'hotpink3'
)
hist(air$Legroomservice,
     data = air,
     main ='Leg room service raitings',
     xlab='Legroomservice',
     ylab= 'Frequency',
     col = 'lightgoldenrod'
)
#=>	The flight service ratings on the scale of 0 to 5 has been plotted in the above graphs.
# o	Airline’s check in services most customers has given rating 3 and 4 which means checking service is moderate.
# o	In flight service’s like serving food or response immediately when customer need help, has highest rating 4 which is pretty good.
# o	Wifi service of the airline seems moderate as most of service rating is between 2 and 3.
# o	Leg room service has the highest rating 4 which shows people are impressed by the leg room. 

par(mfrow = c(2,2))
hist(air$Foodanddrink,
     data = air,
     main ='Food and drink service raitings',
     xlab='Foodanddrink',
     ylab= 'Frequency',
     col = 'maroon1'
)
hist(air$Onlineboarding,
     data = air,
     main ='Online boarding service raitings',
     xlab='Onlineboarding',
     ylab= 'Frequency',
     col = 'orange1'
)
hist(air$Inflightentertainment,
     data = air,
     main ='In flight entertainment service raitings',
     xlab='Inflightentertainment',
     ylab= 'Frequency',
     col = 'olivedrab1'
)
hist(air$Seatcomfort,
     data = air,
     main ='Seat comfort raitings',
     xlab='Seatcomfort',
     ylab= 'Frequency',
     col = 'yellow1'
)

par(mfrow=c(1,1))


#----------------------------Data Cleaning and manipulation---------------------

# converting string to number 
air$Gender <- factor(air$Gender, levels=c('Male','Female'),labels=c(0,1))
table(air$Gender)
typeof(air$Gender)

air$CustomerType <- factor(air$CustomerType, levels=c('Loyal Customer','disloyal Customer'),
                               labels=c(0,1))
table(air$CustomerType)
typeof(air$CustomerType)

air$TypeofTravel <- factor(air$TypeofTravel, levels=c('Business travel','Personal Travel'),
                           labels=c(0,1))
table(air$TypeofTravel)
typeof(air$TypeofTravel)


air$Class <- factor(air$Class, levels=c('Business','Eco','Eco Plus'),
                    labels=c(0,1,2))
table(air$Class)
typeof(air$Class)


air$satisfaction <- factor(air$satisfaction, levels=c('neutral or dissatisfied','satisfied'),
                      labels=c(0,1))
table(air$satisfaction)
typeof(air$satisfaction)


Air_Num_Data <- as.data.frame(apply(air, 2, as.numeric))
Air_Num_Data <- Air_Num_Data[,-1]
head(Air_Num_Data)
str(Air_Num_Data)

#checking data contains any missing values or not
sum(is.na(Air_Num_Data))
#Removing null values 
Air_Num_Data <- na.omit(Air_Num_Data)
#again printing sum of null values
sum(is.na(Air_Num_Data))

# Correlation plot of each variable in housing dataset
corrgram(Air_Num_Data, lower.panel=panel.shade, upper.panel=NULL, test.panel=panel.txt, main ="Correlation between each varaible")

# Dropping columns which has lower co-orelation 
Air_Num_Data =Air_Num_Data[,!names(Air_Num_Data) %in% 'id']
Air_Num_Data =Air_Num_Data[,!names(Air_Num_Data) %in% 'Gender']
Air_Num_Data =Air_Num_Data[,!names(Air_Num_Data) %in% 'CustomerType']
Air_Num_Data =Air_Num_Data[,!names(Air_Num_Data) %in% 'Age']
Air_Num_Data =Air_Num_Data[,!names(Air_Num_Data) %in% 'Class']
Air_Num_Data =Air_Num_Data[,!names(Air_Num_Data) %in% 'FlightDistance']
Air_Num_Data =Air_Num_Data[,!names(Air_Num_Data) %in% 'Gatelocation']
Air_Num_Data =Air_Num_Data[,!names(Air_Num_Data) %in% 'ArrivalDelayinMinutes']                           
Air_Num_Data =Air_Num_Data[,!names(Air_Num_Data) %in% 'Cleanliness']      

head(Air_Num_Data)
str(Air_Num_Data)
View(Air_Num_Data)

# Correlation plot of each variable in housing dataset
corrgram(Air_Num_Data, lower.panel=panel.shade, upper.panel=NULL, test.panel=panel.txt, main ="Correlation between each varaible")

#----------------------------Implementing KNN Classification Model -------------

set.seed(3033)
intrain <- createDataPartition(y = Air_Num_Data$satisfaction, p= 0.7, list = FALSE)
training <- Air_Num_Data[intrain,]
testing <-  Air_Num_Data[-intrain,]

#Viewing dimention of data frame train and test 
dim(training)
dim(testing)

#Creating separate data frame for 'satisfaction' feature which is our target.
training_labels <- Air_Num_Data[intrain,1]
testing_labels  <-Air_Num_Data[-intrain,1]

#knn.5 holds the results of prediction
knn.5 <- knn(train=training, test=testing, cl=training_labels, k=5)


ACC.5 <- 100 * sum(testing_labels == knn.5)/NROW(testing_labels)
ACC.5

table(knn.5 ,testing_labels)

#viewing the prediction: Knn 5 is predicted values and testing_labels are provided test data
#The result of classification we make in a data frame between result of prediction and the data testing.

View(data.frame(knn.5, testing_labels))

confusionMatrix(table(knn.5 ,testing_labels))

CrossTable(x = testing_labels, y = knn.5,
           prop.chisq = FALSE)

#Calculation Recall
Recall = (7858)/(7858+743)
Recall

#Calculating
Precision = (7858)/(7858+1732)
Precision

#Calculating F-Measure
F.measure = (2*Recall*Precision)/(Recall+Precision)
F.measure

#finding optimum k value
i=1                         #initialization of variable                
k.optm=1                     #initialization of variable 
for (i in 1:28){ 
  knn.mod <-  knn(train=training, test=testing, cl=training_labels, k=i)
  k.optm[i] <- 100 * sum(testing_labels == knn.mod)/NROW(testing_labels)
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}
#=> maximum accuracy  92.20671 was achieved for k=3 


# To plot the % accuracy  to k-value
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level", col=c("magenta2")) 















