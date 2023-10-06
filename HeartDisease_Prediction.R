# MOHAMED MAAZ REHAN - PGDA 16

#Import loan_default File

HD<- read.csv("HeartDisease.csv")

View(HD)

str(HD)

dim(HD) # 303 14

sum(is.na(HD)) # To count NA in a dataset

sum(duplicated(HD)) # To check if there are any duplicates - 1

library(dplyr)

HD <- distinct(HD)

sum(duplicated(HD)) # It has removed the duplicates

colnames(HD) # Variable names

head(HD) # To View the top 6 rows

tail(HD) #To View the bottom 6 rows

class(HD) #data.frame

#Number of target column

table(HD$target)

#Percentage of target in a dataset

prop.table(table(HD$target))*100

#To convert into categorical data

HD$gender <- as.factor(HD$gender)

HD$chest_pain <- as.factor(HD$chest_pain)

HD$fasting_blood_sugar <- as.factor(HD$fasting_blood_sugar)

HD$rest_ecg <- as.factor(HD$rest_ecg)

HD$exer_angina <- as.factor(HD$exer_angina)

HD$slope <- as.factor(HD$slope)

HD$ca <- as.factor(HD$ca)

HD$thalassemia <- as.factor(HD$thalassemia)

HD$target <- as.factor(HD$target)


#Summary of the data frame

library(dplyr)

sapply(HD, FUN = summary) # It gives the summary of entire data set

tapply(HD$ï..age, HD$target, mean)

tapply(HD$ï..age, HD$target, range)

tapply(HD$rest_bps, HD$target, mean) # It gives the average in each category

tapply(HD$rest_bps, HD$target, range) # It gives the min and max value 

tapply(HD$cholestrol, HD$target, mean)

tapply(HD$cholestrol, HD$target, range)

tapply(HD$thalach, HD$target, mean)

tapply(HD$thalach, HD$target, range)

tapply(HD$old_peak, HD$target, mean)

tapply(HD$old_peak, HD$target, range)


# Descriptive statistics of data frame

library(psych)

describe(HD)

summary(HD)

#Cross tabulation with two categorical Variable

library(gmodels)

CrossTable(HD$gender, HD$target)

CrossTable(HD$chest_pain, HD$target)

CrossTable(HD$fasting_blood_sugar, HD$target)

CrossTable(HD$rest_ecg, HD$target)

CrossTable(HD$exer_angina, HD$target)

CrossTable(HD$slope, HD$target)

CrossTable(HD$ca, HD$target)

CrossTable(HD$thalassemia, HD$target)


# Co-relations

cor(HD$ï..age, HD$rest_bps) #0.283

cor(HD$ï..age, HD$cholestrol) # 0.207

cor(HD$ï..age, HD$thalach) #-0.395

cor(HD$ï..age, HD$old_peak) #0.206

cor(HD$rest_bps, HD$cholestrol) #0.125

cor(HD$rest_bps, HD$thalach) #-0.04

cor(HD$rest_bps, HD$old_peak) #0.194

cor(HD$cholestrol, HD$thalach)# -0.005


 #Pairs Panel

cor_HD <- HD[, c(1,4,5,8,10)]

str(cor_HD)

pairs.panels(cor_HD)


#For categorical, we have to use bar plot

#Barplot of Gender

text(barplot(table(HD$gender), col = c('blue', 'darkgreen'),
             xlab = "Gender", ylab = "count",
             main = 'Bar plot of Gender'), 0,
     table(HD$gender), cex = 2, pos = 3)


#Barplot of chest_pain

text(barplot(table(HD$chest_pain), col = c('blue', 'darkgreen'),
             xlab = "chest_pain", ylab = "count",
             main = 'Bar plot of chest_pain'), 0,
     table(HD$chest_pain), cex = 2, pos = 3)


#Barplot of fasting_blood_sugar

text(barplot(table(HD$fasting_blood_sugar), col = c('blue', 'darkgreen'),
             xlab = "fasting_blood_sugar", ylab = "count",
             main = 'Bar plot of fasting_blood_sugar'), 0,
     table(HD$fasting_blood_sugar), cex = 2, pos = 3)



#Barplot of rest_ecg

text(barplot(table(HD$rest_ecg), col = c('blue', 'darkgreen'),
             xlab = "rest_ecg", ylab = "count",
             main = 'Bar plot of rest_ecg'), 0,
     table(HD$rest_ecg), cex = 2, pos = 3)


#Barplot of exer_angina

text(barplot(table(HD$exer_angina), col = c('blue', 'darkgreen'),
             xlab = "exer_angina", ylab = "count",
             main = 'Bar plot of exer_angina'), 0,
     table(HD$exer_angina), cex = 2, pos = 3)


#Barplot of slope

text(barplot(table(HD$slope), col = c('blue', 'darkgreen'),
             xlab = "slope", ylab = "count",
             main = 'Bar plot of slope'), 0,
     table(HD$slope), cex = 2, pos = 3)


#Barplot of Number of major vessels - ca

text(barplot(table(HD$ca), col = c('blue', 'darkgreen'),
             xlab = "Number of major vessels", ylab = "count",
             main = 'Bar plot of Number of major vessels'), 0,
     table(HD$ca), cex = 2, pos = 3)



#Barplot of Number of thalassemia 

text(barplot(table(HD$thalassemia), col = c('blue', 'darkgreen'),
             xlab = "thalassemia", ylab = "count",
             main = 'Bar plot of thalassemia'), 0,
     table(HD$thalassemia), cex = 2, pos = 3)


#Barplot of Number of target- 164 - Heart Disease, 138 - No Heart Disease

text(barplot(table(HD$target), col = c('blue', 'darkgreen'),
             xlab = "target", ylab = "count",
             main = 'Bar plot of target'), 0,
     table(HD$target), cex = 2, pos = 3)


##histogram and boxplot for age

par(mfrow = c(2,1)) # par = partitioning, mfrow, margin from row, c(2,1) = 2 rows and 1 column

hist(HD$ï..age, xlab = "Age", 
     main = "Histogram of Age", 
     ylab = "counts", col = "blue") 


boxplot(HD$ï..age, main = 'boxplot of age', col = 'blue', horizontal = T)

summary(HD$ï..age)


##histogram and boxplot for Blood pressure of the patient while resting

hist(HD$rest_bps, xlab = "Blood pressure of the patient while resting", 
     main = "Histogram of Blood pressure of the patient while resting", 
     ylab = "counts", col = "red") 


boxplot(HD$rest_bps, main = 'boxplot of Blood pressure of the patient while resting', col = 'red', horizontal = T)

summary(HD$rest_bps)


##histogram and boxplot for cholestrol

hist(HD$cholestrol, xlab = "cholestrol", 
     main = "Histogram of cholestrol", 
     ylab = "counts", col = "green") 


boxplot(HD$cholestrol, main = 'boxplot of cholestrol', col = 'green', horizontal = T)

summary(HD$cholestrol)


##histogram and boxplot for The patient's maximum heart rate

hist(HD$thalach, xlab = "The patient's maximum heart rate", 
     main = "Histogram of The patient's maximum heart rate", 
     ylab = "counts", col = "violet") 


boxplot(HD$thalach, main = 'boxplot of The patients maximum heart rate', col = 'violet', horizontal = T)

summary(HD$thalach)



##histogram and boxplot for position on ECG plots

hist(HD$old_peak, xlab = "position on ECG plots", 
     main = "Histogram of position on ECG plots", 
     ylab = "counts", col = "darkblue") 


boxplot(HD$old_peak, main = 'boxplot of position on ECG plots', col = 'darkblue', horizontal = T)

summary(HD$old_peak)

dev.off()



#ggplot

library(ggplot2)

#ggplot of two categorical variable - STACK

ggplot(HD,aes(x = target, fill = gender)) + geom_bar()

ggplot(HD,aes(x = target, fill = chest_pain)) + geom_bar()

ggplot(HD,aes(x = target, fill = fasting_blood_sugar)) + geom_bar()

ggplot(HD,aes(x = target, fill = rest_ecg)) + geom_bar()

ggplot(HD,aes(x = target, fill = exer_angina)) + geom_bar()

ggplot(HD,aes(x = target, fill = slope)) + geom_bar()

ggplot(HD,aes(x = target, fill = ca)) + geom_bar()

ggplot(HD,aes(x = target, fill = thalassemia)) + geom_bar()


#ggplot of two categorical variable - CLUSTER

ggplot(HD, aes(gender, fill = target))+ geom_bar(stat= 'count', position = position_dodge())

ggplot(HD, aes(chest_pain, fill = target))+ geom_bar(stat= 'count', position = position_dodge())

ggplot(HD, aes(fasting_blood_sugar, fill = target))+ geom_bar(stat= 'count', position = position_dodge())

ggplot(HD, aes(rest_ecg, fill = target))+ geom_bar(stat= 'count', position = position_dodge())

ggplot(HD, aes(exer_angina, fill = target))+ geom_bar(stat= 'count', position = position_dodge())

ggplot(HD, aes(slope, fill = target))+ geom_bar(stat= 'count', position = position_dodge())

ggplot(HD, aes(ca, fill = target))+ geom_bar(stat= 'count', position = position_dodge())

ggplot(HD, aes(thalassemia, fill = target))+ geom_bar(stat= 'count', position = position_dodge())



#Data Preparation

# Creating Random Sample for Training and Testing Data

# install.packages("caTools")

library(caTools)
# use set.seed to use the same random number sequence
set.seed(123)

# creating the data partition
split <- sample.split(HD$target, SplitRatio = 0.70)

# create 70% training data
HDTrain <- subset(HD, split == TRUE)

# dimensions of training data
dim(HDTrain) # 212 14
 
# create 30% testing data
HDTest <- subset(HD, split == FALSE)

# dimensions of testing data
dim(HDTest) #90 14

#count of target in loan data set
table(HD$target)

#count of target in Training data set
table(HDTrain$target)

#count of target in Testing data set
table(HDTest$target)

#Percentage of target in Testing data set
prop.table(table(HDTest$target))


# Build Model

library(tree) #Install tree library

modelClassTree <- tree(as.factor(target) ~. ,  data = HDTrain)

summary(modelClassTree)

plot(modelClassTree)

text(modelClassTree, pretty = 0, cex = 0.75)


# MOdel Prediction

pred <- predict(modelClassTree, newdata = HDTest, type = "class")

conf <- table(HDTest$target, pred)

conf #Confusion Matrix


OAA <- (conf[1,1]+conf[2,2])/sum(conf)

OAA #72.22 (Accuarcy)


# AUC AND ROC

library(ROCR) #Import ROCR library

test.pred.tree.roc <- predict(modelClassTree, 
                              newdata = HDTest, 
                              type = 'vector') 

perf.tree <-  prediction(test.pred.tree.roc[,2], HDTest$target)

auc.tree <- performance(perf.tree, "auc")

auc.tree@y.values #0.8111

pred.tree = performance(perf.tree, "tpr","fpr")

roc.tree <- plot(pred.tree, main="ROC Curve",col = "violet", lwd = 2)

abline(a=0, b=1,lwd=2, lty=2, col ="grey")


## "Improvement Models of Decision Tree - Pruning, Bagging and Random Forest"


library(tree) #Import tree library

tree.HD <- tree(as.factor(target) ~., HD) 

summary(tree.HD)

# Plot

plot(tree.HD) # 20 terminal nodes

text(tree.HD, pretty = 0) #Add text to a plot


#Train and Test

set.seed(2) #Random Number Generator

train <- sample(1:nrow(HD), 212) #Selecting 70% of data for training

HD.test <- HD[-train, ] #Leaving train set, remaining are Test data

target.test <- HD$target[-train]

# Build Tree -Unprune

tree.HD1 <- tree(as.factor(HD$target) ~.,
                   HD, subset = train)
summary(tree.HD1) #Misclassification error rate: 0.089

plot(tree.HD1) # 18 terminal Nodes

text(tree.HD1, pretty = 0)

# Test Model - Unprune

tree.pred <- predict(tree.HD1,
                     HD.test, type = 'class')


table(tree.pred, target.test)

(36+32)/90  # 75.55

## AUC and ROC Curve - Unprune

library(ROCR)

test.pred.unprune.roc <- predict(tree.HD1, 
                                 newdata = HD[-train, ], 
                                 type = 'vector') 

perf.unprune <-  prediction(test.pred.unprune.roc[,2], target.test)

auc.unprune <- performance(perf.unprune, "auc")

auc.unprune@y.values #0.836

pred.unprune = performance(perf.unprune, "tpr","fpr")

roc.unprune <- plot(pred.unprune, main="ROC Curve for UnPruned",col = "violet", lwd = 2)

abline(a=0, b=1,lwd=2, lty=2, col ="grey")


# Pruning Tree

set.seed(3) # We can write any number, This is only fixing the random Process
cv.HD <- cv.tree(tree.HD1, FUN = prune.misclass)

#When to prune the tree

names(cv.HD) 
cv.HD

#Size (number of terminal nodes) and cost complexity factor
#Plotting error

par(mfrow = c(1,2))

plot(cv.HD$size, cv.HD$dev, 
     type = 'b', col = 'red', lwd = 2) #lwd - line width, Y axis is the deviance,
# It shows the minimum deviance corresponds to terminal node

plot(cv.HD$k, cv.HD$dev,
     type = 'b', col = 'blue', lwd = 2)

dev.off()


#Build tree with 4 terminals Nodes - Prune

prune.HD <- prune.misclass(tree.HD1, best = 4)

plot(prune.HD)

text(prune.HD, pretty = 0) #Adding text to plot

# Test Model - Prune

tree.pred1 <- predict(prune.HD, HD.test, type = 'class')

table(tree.pred1, target.test)

(32+39)/90 #78.88, (Accuracy - Pruned)

# AUC and ROC Curve - Prune 


test.pred.prune.roc <- predict(prune.HD, 
                               newdata = HD[-train, ], 
                               type = 'vector') 

perf.prune <-  prediction(test.pred.prune.roc[,2], target.test)

auc.prune <- performance(perf.prune, "auc")

auc.prune@y.values #0.787

pred.prune = performance(perf.prune, "tpr","fpr")

roc.prune <- plot(pred.prune, main="ROC Curve for Pruning",col="red", lwd = 2)

abline(a=0, b=1,lwd=2, lty=2, col ="black")



#Bagging - Building and Testing the Model

#Bagging is random forest if all predictors are used for building trees

library(randomForest) #Import randomForest library

set.seed(1)

bag.HD <- randomForest(as.factor(target)~., HD, subset = train, 
                         mtry = 13, importance = TRUE)
#mtry - maximum try  with predictors, 13 predictors were there, 
#we are telling try with all 13 predictors

dim(HD)

# Which variables are important?

importance(bag.HD) # In our Analysis, chest_pain, thalassemia and ca are 
#top 3 predictors

varImpPlot(bag.HD, col = 'red', 
           pch = 10, cex = 1.25 )

# Trained Model (bagging)

bag.HD # Out of bag(OOB) estimate of  error rate: 18.87%
#500 trees were built, No. of variables tried at each split: 13 (In Bagging all predictors are used)
# OOB estimate of  error rate: 18.87% (Misclassification Error), (Classification Error - 81.13)

#Test Results

test.pred.bag <- predict(bag.HD, newdata = HD[-train, ], 
                         type = 'class') #newdata - Testing data

table(test.pred.bag, target.test)

(34+36)/90 #77.77 - Accuracy (Bagging)


# AUC and ROC Curve - Bagging

test.pred.bag.roc <- predict(bag.HD, 
                             newdata = HD[-train, ], 
                             type = 'prob') 

perf.bag <-  prediction(test.pred.bag.roc[,2], target.test)


auc.bag <- performance(perf.bag, "auc")

auc.bag@y.values #0.875

pred.bag = performance(perf.bag, "tpr","fpr")

roc.bag <- plot(pred.bag, main="ROC Curve for bagging",col="green", lwd = 2)

abline(a=0, b=1,lwd=2, lty=2, col ="black")


#Random Forest - Building and Testing the Model

#random forest SQRT16 = mtry = 4 

set.seed(1)
rf.HD <- randomForest(as.factor(target)~.,
                        HD, subset = train,
                        mtry = 4, importance = TRUE)

# mtry = 4, because SQRT(13) = 3.6, meaning it will pick 4 random predictors

dim(HD)

importance(rf.HD)

varImpPlot(rf.HD, col = 'blue',
           pch = 20, cex = 1.25)

#RF Trained Model 

rf.HD # OOB estimate of  error rate: 15.09%


#RF Tested Results

test.pred.rf <- predict(rf.HD, 
                        newdata = HD[-train, ], 
                        type = 'class')

table(test.pred.rf, target.test)

(36+37)/90 # 81.11 - Accuracy (Random Forest)


# AUC and ROC Curve - Random Forest


library(ROCR)

test.pred.rf.roc <- predict(rf.HD, 
                            newdata = HD[-train, ], 
                            type = 'prob') 

perf.rf <-  prediction(test.pred.rf.roc[,2], target.test)


auc.rf <- performance(perf.rf, "auc")

auc.rf@y.values #0.891

pred.rf = performance(perf.rf, "tpr","fpr")

roc.rf <- plot(pred.rf,main="ROC Curve for Random Forest",col= "blue",lwd=2)

abline(a=0,b=1,lwd=2,lty=2,col="black")


# From the above models, It is observed that random forest is a good model for the given data set 
# having accuracy 81.11% and AUC- 0.891

# The top 5 predictors for heart disease are as follows: 

# 1. Chest_Pain

# 2. thalassemia 

# 3. thalach

# 4. ca

# 5. old_peak 
































