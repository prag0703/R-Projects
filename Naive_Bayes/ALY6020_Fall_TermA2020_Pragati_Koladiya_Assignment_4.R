# Assignment 4 
#Student name: Pragati Koladiya
#NUID: 001029445
# File name: ALY6020_Fall_TermA2020_Pragati_Koladiya
# Goal: To predict weather the student will be placed or not based on given features. 
# Dataset Information: Data set is from kaggle with file name "Placement.csv"

#--------------------------------------Importing the packages-------------------
library(caTools)
library(ggplot2)
library(stats)
library(dplyr)
library(gridExtra)
library(GGally)
library(readr)
library(seriation) 
library(moderndive)
library(corrgram)
library(RColorBrewer)
library(lattice)
library(caret)
library(gmodels)
library(gdata)
library(class)
library(reprex)
library(caTools)
library(e1071)
#--------------------------------------Importing CSV ---------------------------
placement <- read_csv("/Users/pragatikoladiya/OneDrive - Northeastern University/Q3-First/Predictive_Analytics/week4/Placement.csv")

#Data exploration
View(placement) 
str(placement)
head(placement)
glimpse(placement)
placement %>% dim()

#Data Dictionary
# 1.	sl_no = Serial Number
# 2.	gender = Gender: Male='M', Female='F'
# 3.	ssc_p = Secondary Education percentage- 10th Grade
# 4.	ssc_b = Board of Education- Central/ Others
# 5.	hsc_p = Higher Secondary Education percentage- 12th Grade
# 6.	hsc_b = Board of Education- Central/ Others
# 7.	hsc_s = Specialization in Higher Secondary Education
# 8.	degree_p = Degree Percentage
# 9.	degree_t = Under Graduation(Degree type)- Field of degree education
#10.	workex = Work Experience
#11.	etest_p = Employability test percentage ( conducted by college)
#12.	specialisation = Post Graduation(MBA)- Specialization
#13.	mba_p = MBA percentage
#14.	status = Status of placement- Placed/Not placed
#15.	salary = Salary offered by corporate to candidates


#----------------------------Exploratory Data Analysis(EDA)---------------------

#Student count by gender
placement %>% 
  ggplot(aes(x = gender, fill = gender)) +  geom_bar(aes(fill = gender))+
  labs(title = 'Student count by gender') + 
  scale_fill_brewer(palette = "Set3")

#Student count grouped by status
placement %>% 
    ggplot(aes(x = gender, fill = status)) +  geom_bar(aes(fill = status))+
      labs(title = 'Student count grouped by status') + 
      scale_fill_brewer(palette = 14)

#Student count by gender grouped by work experience
placement %>% 
    ggplot(aes(x = workex)) +  geom_bar(aes(fill = status))+
    labs(title = 'Student count by gender grouped by work experience') + 
    scale_fill_brewer(palette = 4)

#Placement Vs Under graduate degree type
placement <- ggplot(placement, aes(degree_t))
placement+ geom_bar(aes(fill = status), position = "dodge")+
    labs(title = 'Placement Vs Under graduate degree type') + 
    scale_fill_brewer(palette = 11)

#SSC Vs HSC percentage by status
qplot(ssc_p, hsc_p, data = placement, geom = c("point"), xlab = "SSC Percentage", ylab = "HSC Percentage" , size = factor(status), color = factor(status)) + 
  scale_fill_manual(values =rainbow(2, start = .8, end = .88))

#E-test percentage distribution using density plot
ggplot(placement) + geom_density(aes(etest_p, fill = degree_t), alpha = 0.5)+
  scale_fill_manual(values =rainbow(3, start =0.48 , end = 0.64 ))

#Salary Vs status
ggplot(placement,aes(factor(status),salary,fill=factor(status)))+
  geom_boxplot(alpha=0.6)+
  scale_fill_manual(values =rainbow(2, start = .92, end = .44))+
  theme(legend.position="none")+
  ggtitle("Salary Vs Status")+
  labs(x="Student status")

#Heat map
colnames(placement)
#Selecting only numeric columns for heat map
placement_numeric <- placement[, c(3, 5, 8, 11)]
colnames(placement_numeric)

corr_matrix <- round(cor(placement_numeric),2)
df_corr_value <- reshape2::melt(corr_matrix)

ggplot(data = df_corr_value) + 
  geom_tile(mapping = aes(x=Var1, y=Var2, fill=value))

#------------------------------------Data Preparation------------------------------
#Viewing Null count
placement[placement==""] <- NA
sapply(placement,function(x) sum(is.na(x)))
sapply(placement, function(x) length(unique(x)))

#Dropping Salary column
placement =placement[,!names(placement) %in% 'salary'] 

#Converting categorical columns to numerical columns
placement$gender <- factor(placement$gender, levels=c('M','F'),labels=c(0,1))
table(placement$gender)
typeof(placement$gender)
------------------------------------------------------------------
placement$ssc_b <- factor(placement$ssc_b, levels=c('Others','Central'),labels=c(0,1))
table(placement$ssc_b)
typeof(placement$ssc_b)
------------------------------------------------------------------
placement$hsc_b <- factor(placement$hsc_b, levels=c('Others','Central'),labels=c(0,1))
table(placement$hsc_b)
typeof(placement$hsc_b)
------------------------------------------------------------------
placement$hsc_s <- factor(placement$hsc_s, levels=c('Arts','Commerce','Science'), labels=c(0,1,2))
table(placement$hsc_s)
typeof(placement$hsc_s)
------------------------------------------------------------------
placement$degree_t <- factor(placement$degree_t, levels=c('Sci&Tech','Comm&Mgmt','Others'), labels=c(0,1,2))
table(placement$degree_t)
typeof(placement$degree_t)
------------------------------------------------------------------
placement$workex <- factor(placement$workex, levels=c('No','Yes'), labels=c(0,1))
table(placement$workex)
typeof(placement$workex)
------------------------------------------------------------------
placement$specialisation<- factor(placement$specialisation, levels=c('Mkt&HR','Mkt&Fin'), labels=c(0,1))
table(placement$specialisation)
typeof(placement$specialisation)
----------------------------------------------------------------------------------------------------------------
placement$status<- factor(placement$status, levels=c('Placed','Not Placed'), labels=c(0,1))
table(placement$status)
typeof(placement$status)

str(placement)
placement_Num_Data <- as.data.frame(apply(placement, 2, as.numeric))

# Encoding the target feature as factor
placement_Num_Data$status = factor(placement_Num_Data$status, levels = c(0, 1))

head(placement_Num_Data)
str(placement_Num_Data)

# Correlation plot of each variable in housing dataset
corrgram(placement_Num_Data, lower.panel=panel.shade, upper.panel=NULL, test.panel=panel.txt, main ="Correlation between each varaible")

ggcorr(placement_Num_Data)

#Droping lower corelation values
placement_Num_Data =placement_Num_Data[,!names(placement_Num_Data) %in% 'sl_no']
placement_Num_Data =placement_Num_Data[,!names(placement_Num_Data) %in% 'hsc_b']
placement_Num_Data =placement_Num_Data[,!names(placement_Num_Data) %in% 'degree_t']

#Let's plot histogram to analyze the correlation after dropping few columns
ggcorr(placement_Num_Data)

#----------------------------Implementing Naive Bayes Model---------------------
# Splitting the data set into the Training set and Test set
set.seed(123)
split = sample.split(placement_Num_Data$status, SplitRatio = 0.75)
training_set = subset(placement_Num_Data, split == TRUE)
test_set = subset(placement_Num_Data, split == FALSE)

#Viewing the dimention of train and test set
dim(training_set)
dim(test_set)

#implementing naivebayes model
classifier = naiveBayes(status ~ . ,data = training_set)
classifier

# Predicting the Test set results
prediction <-predict(classifier, test_set)
prediction

#Model evaluation using Confusion Matrix
ConfusionMatrix = table(test_set[,11], prediction)
ConfusionMatrix

CrossTable(prediction, test_set[,11],
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

Accuracy = (35+10)/(35+2+7+10)
Accuracy

Sentivity = (10)/(10+7)
Sentivity

Specificity = (35)/(35+2)
Specificity

Precision = (10)/(10+2)
Precision
