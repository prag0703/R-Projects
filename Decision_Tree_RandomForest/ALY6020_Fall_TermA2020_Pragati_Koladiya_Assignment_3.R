
#Fine name: ALY6020_Fall_TermA2020_Pragati_Koladiya_Assignment_3
#Tittle: Assignment-3 
#Course: ALY6020 Predictive Analytics

#Models implemented:
#* Decision Tree
#         o	Decision tree without splitting data 
#               o	Good Vs alcohol, sulphates (Selected this two features because the most discriminating attributes we can observe are sulphates and alcohol level of the red wine).
#                o	Good Vs alcohol, volatile acidity, citric acidity, and sulphates
#          o	Decision tree with splitting data
#                o	Quality Vs all independent variable
#* Random Forest
#           o Quality Vs all independent variable             
                              
#--------------------------------------Importing the packages-------------------

library(caTools)
library(ggplot2)
library(ggthemes)
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
library(randomForest)
library(FSelector)
library(caret)
library(rpart)
library(rpart.plot)
library(dplyr)
library(data.tree)
library(caTools)
library(corrplot)
library(devtools)
require(tree)
library(gmodels)


#--------------------Reading the data file 
wine_data <- read_csv("/Users/pragatikoladiya/OneDrive - Northeastern University/Q3-First/Predictive_Analytics/week3/winequality-red.csv")

View(wine_data) 
#--------------------Data Dictionary------------------------

##Red Wine Quality

#fixed acidity - Most acids involved with wine or fixed or nonvolatile(do not evaporate readily)
#volatile acidity - The amount of acetic acid in wine, which at too high of levels can lead to an unplesant, vinegar taste
#citric acid - Found in small quantities, citric acid can add 'freshness' and flavour to wines
#residual sugar - The amount of sugar remaining after fermentation stops, it's rare to find wines with less than 1 gram/liter. Wines with greater than 45 grams/litter are considered sweet
#chlorides - The amount of salt in the wine
#free sulfur dioxide - The free form of SO2 exists in equilibrium between molecular SO2 (as a dissolved gas) and bisulfite ion; it prevents microbial growth and the oxidation of wine
#total sulfur dioxide- Amount of free and bound form of SO2; in low concentrations, SO2 is mostly undetectable in wine,but at free SO2 concentrations over 50 ppm, SO2 becomes evident in the nose and taste of wine
#density - The wine density
#pH - Describes how acidic or basic a wine is on a scale from 0 (very acidic) to 14 (very basic); most wines are between 3-4 on the pH scale
#sulphates - A wine additive which can contribute to sulfur dioxide gas (S02) levels, which acts as an antimicrobial and antioxidant
#alcohol - The percent alcohol content of the wine
#quality - output variable (based on sensory data, score between 0 and 10)

#All the measurements are (in grams) per decimeter cubed of wine (dm^3) [g/dm^3] accept pH, alcohol and quality columns


#--------------------Data Exploration-------------------------------
str(wine_data)
head(wine_data)
sum(is.na(wine_data))

summary(wine_data)
#---------------------------EXPLORATERY DATA ANALYSIS(EDA)----------------------

#Red wine quality count 

#Distribution of red wine quality ratings bar plot and density plot
table(wine_data$quality)
wine_data %>% group_by(qui=factor(quality))%>%summarise(floor_cnt=n())%>%
  ggplot(aes(x=qui,floor_cnt,fill=qui))+
  geom_bar(stat="identity",alpha=0.5)+
  scale_fill_manual(values =rainbow(8, start= .2, end = .96))+
  theme(legend.position="none")+
  labs(x="Quality (raiting on the scale 1 to 10)",y="Count")+
  ggtitle("Red wine quality raiting")

#density plot
ggplot(wine_data,aes(x=quality, fill=factor(quality)))+
  geom_density(alpha=0.50)+
  scale_fill_manual(values =rainbow(6, start= .76, end = 1))+
  ggtitle("Density plot to show 'Quality' distribution")
  theme_classic()
  
#Distribution of good/bad red wine
#-- Create a variable indicating if a wine is good or bad
wine_data$good <- ifelse(wine_data$quality > 6,1,0)

#Good and bad wine distribution using bar plot
ggplot(wine_data,aes(x=good,fill=factor(good)))+geom_bar(stat = "count",position = "dodge", alpha = 0.5)+
  scale_x_continuous(breaks = seq(0,1,1))+
  scale_fill_manual(values =rainbow(2, start =0.48 , end = 0.64 ))+
  ggtitle("Distribution of Good/Bad Red Wine")+
  theme_classic()

#Alcohol Vs Wine quality
ggplot(wine_data,aes(x=alcohol,fill=factor(good)))+geom_density(alpha=0.50)+
  geom_vline(aes(xintercept=mean(alcohol[good==0],na.rm=T)),color="yellow",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(alcohol[good==1],na.rm=T)),color="yellow",linetype="dashed",lwd=1)+
  scale_fill_manual(values =rainbow(2, start = .72, end = .92))+
  scale_x_continuous(breaks = seq(5,15,1))+
  xlab(label = "Alcohol Level")+
  ggtitle("Distribution of Alcohol Levels")+
  theme_classic()

#Alcohol Vs Quality box plot 
ggplot(wine_data,aes(factor(good),alcohol,fill=factor(good)))+
  geom_boxplot(alpha=0.6)+
  scale_fill_manual(values =rainbow(2, start = .92, end = .44))+
  theme(legend.position="none")+
  ggtitle("Alcohol Vs Quality")+
  labs(x="Quality of wine")


#Alcohol Vs Density grouped by quality of wine
qplot(alcohol, density, data = wine_data, geom = c("point"), xlab = "Alcohol", ylab = "Density" , size = factor(good), color = factor(good)) + 
  scale_size_manual(values = c(3, 7), name="good") + 
  scale_colour_manual(values = c("yellowgreen","yellow"), name="good")

#Sulfates Vs quality reviews
ggplot(wine_data,aes(factor(good),sulphates,fill=factor(good)))+
  geom_boxplot(alpha=0.6)+
  scale_fill_manual(values =rainbow(2, start = .8, end = .88))+
  theme(legend.position="none")+
  ggtitle("Sulphates Vs Quality")+
  labs(x="Quality of wine")

#pH Vs reviews
ggplot(wine_data,aes(factor(good),pH,fill=factor(good)))+
  geom_boxplot(alpha=0.6)+
  scale_fill_manual(values =rainbow(2, start = .48, end = .16))+
  theme(legend.position="none")+
  ggtitle("pH Vs Quality")+
  labs(x="Quality of wine")

#Citric acid Vs reviews
ggplot(wine_data,aes(factor(good),citric.acid,fill=factor(good)))+
  geom_boxplot(alpha=0.6)+
  scale_fill_manual(values =rainbow(2, start = .76, end = .96))+
  theme(legend.position="none")+
  ggtitle("Citric Acid Vs Quality")+
  labs(x="Quality of wine")

#Fixed acidity Vs quality
ggplot(wine_data,aes(factor(good),fixed.acidity,fill=factor(good)))+
  geom_boxplot(alpha=0.6)+
  scale_fill_manual(values =rainbow(2, start = .32, end = .6))+
  theme(legend.position="none")+
  ggtitle("Fixed Acidity Vs Quality")+
  labs(x="Quality of wine")

#Residual sugar Vs red wine quality
ggplot(wine_data,aes(factor(good),residual.sugar,fill=factor(good)))+
  geom_boxplot(alpha=0.6)+
  scale_fill_manual(values =rainbow(2, start = .16, end = .52))+
  theme(legend.position="none")+
  ggtitle("Residual sugar Vs Quality")+
  labs(x="Quality of wine")

#Distribution of free and total supher dioxide
qplot(free.sulfur.dioxide,total.sulfur.dioxide, data = wine_data, col=c(rep("turquoise3"))) 

#Chlorides Vs quality
ggplot(wine_data,aes(factor(quality),chlorides, fill=factor(quality)))+
  geom_boxplot(alpha=0.6)+
  scale_fill_manual(values =rainbow(6, start = .4, end = .6))+
  theme(legend.position="none")+
  ggtitle("Chlorides Vs Quality")+
  labs(x="Quality of wine")

# Correlation plot of each variable in housing dataset
corrgram(wine_data, order=TRUE, lower.panel=panel.shade, upper.panel=NULL, test.panel=panel.txt, main ="Correlation between each varaible")

par(mfrow=c(1,1))
#--------------DECISION TREE  without splitting data--------------

tree1 <- rpart(good ~ alcohol + sulphates, data = wine_data, method="class")
summary(tree1)
rpart.plot(tree1)
pred1 <- predict(tree1,newdata=wine_data,type="class")

tree2 <- rpart(good ~ alcohol + volatile.acidity + citric.acid + sulphates, data = wine_data, method="class")
summary(tree2)
rpart.plot(tree2)
pred2 <- predict(tree2,newdata=wine_data,type="class")

table(wine_data$good,pred1)
table(wine_data$good,pred2)

Accuracy_t1 = ((1343+75)/(1343+39+142+75))
Accuracy_t1
#==> Accracy = 88% 

Accuracy_t2=((1334+111)/(1334+48+106+111))
Accuracy_t2
#==> Accracy = 90% 
#----------------------------DECISION TREE with split data-----------------

#load file again as we have converted quality in to three category so 

wine_data$quality = as.factor(wine_data$quality)

set.seed(123)
sample = sample.split(wine_data$quality, SplitRatio = .80)
train = subset(wine_data, sample == TRUE)
test = subset(wine_data, sample == FALSE)


#view dimention of data frame train and test 
dim(train)
dim(test)

#View(test)
#Training the decision tree classifier 
tree <- rpart(quality ~ ., data = train, method = 'class')
rpart.plot(tree)
#predictions
tree.quality.predicted <- predict(tree, newdata = test, type = 'class')

#View(tree.quality.predicted)

View(data.frame(tree.quality.predicted, test$quality))

#confusion matrix for evaluating the model
confusionMatrix(table(tree.quality.predicted ,test$quality))


CrossTable(x = tree.quality.predicted, y = test$quality,
           prop.chisq = FALSE)

#==> Accuracy = 77%
---------------------------------------------------------------------------------
  
#----------------------------Random Forest with split data-----------------
  
#--------------------Reading the data file again as we are not using good column for further models
wine_data <- read_csv("/Users/pragatikoladiya/OneDrive - Northeastern University/Q3-First/Predictive_Analytics/week3/winequality-red.csv")

wine_data$quality <- as.factor(wine_data$quality)
set.seed(123)
index <- sample(1:nrow(wine_data),size = 0.8*nrow(wine_data))
train <- wine_data[index,]
test <- wine_data[-index,]

# simplest random forest
rf_model <- randomForest(quality ~., data = train, mtry=4, ntree=2000, importance=TRUE)
plot(rf_model)

rf_prediction <- predict(rf_model, test)
result <- data.frame(test$quality, predict(rf_model, test[,1:11], type="response"))
View(data.frame(rf_prediction, test$quality))

#confusion matrix for evaluating the model
rf_matrix <- confusionMatrix(rf_prediction, test$quality)
rf_matrix

#==> Accracy 80%












