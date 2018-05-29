#Load Datasets

setwd("E:/RStudio/R projects/BigMartSalesPrediction")
train <- read.csv("Train_UWu5bXk.csv")
test <- read.csv("Test_u94Q5KV.csv")

#check dimesions ( number of row & columns) in data set
dim(train)
dim(test)

#check the variables and their types in train
str(train)

#check missing values
table(is.na(train))

#locate missing values, Item_weight has 1463 missing values
colSums(is.na(train))

summary(train)

library(ggplot2)

#Visualization
ggplot(train, aes(x= Item_Visibility, y= Item_Outlet_Sales))+
  geom_point(size= 1.5, color='navy')+
  xlab('Item Visibility')+
  ylab('Item Outlet Sales')+
  ggtitle('Item Visibility vs Item Outlet Sales')

# Most sales are the products with less than 0.2 visibility.
# Suggest Item_Visibility < 2 must be an important factor in determining sales.

ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales))+
  geom_bar(stat = "identity", color = "purple")+
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))+
  ggtitle("Outlets vs Total Sales")+ 
  theme_bw()

# Out027 contributed to most of the sales, followed by Out035
# Out010 and Out 019 have the least outlet sales.

ggplot(train, aes(Item_Type, Item_Outlet_Sales))+ 
  geom_bar( stat = "identity") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + 
  xlab("Item Type") + 
  ylab("Item Outlet Sales")+
  ggtitle("Item Type vs Sales")

# Fruits and Vegetables contributed the highest ammount of outlet sales
# followed by snacks and household products.

ggplot(train, aes(Item_Type, Item_MRP)) +
  geom_boxplot() +ggtitle("Box Plot") + 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) +
  xlab("Item Type") + 
  ylab("Item MRP") + 
  ggtitle("Item Type vs Item MRP")

# Impute the missing NA values

dim(train)
dim(test)

# test has 11 columns, need to add placeholder column Item_Outlet_Sales

test$Item_Outlet_Sales <-  1
combi <- rbind(train, test)

# Impute using median, RMSE is highly affected by outliers, use median to remove some outliers.

combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
table(is.na(combi$Item_Weight))

# some of the Item_Visibility have zero values, not practically feasible
# consider as missing value and impute

combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0,
                                median(combi$Item_Visibility), combi$Item_Visibility) 


levels(combi$Outlet_Size)[1] <- "Other"
library(plyr)

# renamed various levels of Item_Fat_Content

combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,
                                  c("LF" = "Low Fat", "reg" = "Regular"))

combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, 
                                  c("low fat" = "Low Fat"))

table(combi$Item_Fat_Content)

# Feature Engineering
# What else (factors) could influence Item_Outlet_Sales? 

# 10 unique outlets in data.
# The higher the count on outlets, the chances are more contributed with sales.

library(dplyr)

a <- combi%>%
  group_by(Outlet_Identifier)%>%
  tally()

names(a)[2] <- "Outlet_Count"      # renaming second col

combi <- full_join(a, combi, by = 'Outlet_Identifier')

# Count of Item Identifiers

b <- combi%>%
  group_by(Item_Identifier)%>%
  tally()

names(b)[2] <- "Item_Count"

combi <- merge(b, combi, by = 'Item_Identifier')

# Outlet Years
# hypothesis, older the outlet, large base of loyal customers and larger the outlet sales

c <- combi%>%
  select(Outlet_Establishment_Year)%>% 
  mutate(Outlet_Year = 2013 - combi$Outlet_Establishment_Year)

combi <- full_join(c, combi)

# Item Type
# DR ~ eatables, FD ~ drinks, NC ~ cannot be consumed

q <- substr(combi$Item_Identifier,1,2)
q <- gsub("FD","Food",q)
q <- gsub("DR","Drinks",q)
q <- gsub("NC","Non-Consumable",q)
table(q)

combi$Item_Type_New <- q

# Label Encoding and One Hot Encoding

combi$Item_Fat_Content <- ifelse(combi$Item_Fat_Content == "Regular",1,0)

sample <- select(combi, Outlet_Location_Type)
demo_sample <- data.frame(model.matrix(~.-1,sample))
head(demo_sample)

library(dummies)

combi <- dummy.data.frame(combi, names = c('Outlet_Size',
                                           'Outlet_Location_Type',
                                           'Outlet_Type', 
                                           'Item_Type_New'),  sep='_')
str(combi)











# lin analysis

linear_model <- lm(Item_Outlet_Sales ~ ., data = combi)
summary(linear_model)

# R^2 value is bad, soem of the variables that isnt helping 


# Robust reg model


setwd("E:/RStudio/R projects/BigMartSalesPrediction")
train <- read.csv("Train_UWu5bXk.csv")
test <- read.csv("Test_u94Q5KV.csv")

#create a new variable in test file
test$Item_Outlet_Sales <- 1

#combine train and test data
combi <- rbind(train, test)

#impute missing value in Item_Weight
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)

#impute 0 in item_visibility

combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),
                                combi$Item_Visibility)

# first index of Outlet_size is blank, need to replace with a name
View(levels(combi$Outlet_Size))

#rename level in Outlet_Size
levels(combi$Outlet_Size)[1] <- "Other"

#rename levels of Item_Fat_Content
library(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat", "reg" =                                   "Regular"))
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
table(combi$Item_Fat_Content)

#create a new column 2013 - Year
combi$Year <- 2013 - combi$Outlet_Establishment_Year

#drop variables not required in modeling, -c( , , , ) drops index in combi
library(dplyr)
combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year))

#divide data set
new_train <- combi[1:nrow(train),]
new_test <- combi[-(1:nrow(train)),]

#linear regression, analysis from new_train data set with Item_Outlet_Sales as dependent variable
linear_model <- lm(Item_Outlet_Sales ~ ., data = new_train)
summary(linear_model)

# regression plot

par(mfrow=c(2,2))
plot(linear_model)

# most important plot is Residuals vs Fitted Graph
# residual values are the difference between actual and predicted, fitted values are predicted values
# model suffering from unequal variance in error terms. 
# if model has constant variance, there would be no pattern visible in the graph

# use log of response variable in attempt to remove unequal variance

linear_model <- lm(log(Item_Outlet_Sales) ~ ., data = new_train)
summary(linear_model)

# R^2 is improved

par(mfrow=c(2,2))
plot(linear_model)

# residual vs fitted has no longer a trend.

# model can further improved by detecting outliers and high leverage points

# check RMSE and compare with other algorithms

#install.packages("Metrics")
library(Metrics)
rmse(new_train$Item_Outlet_Sales, exp(linear_model$fitted.values))

# rmse = 1140, improve this by using other algo

# Decision Trees

#loading required libraries
library(ggplot2)
library(lattice)
library(rpart)
library(e1071)
library(rpart.plot)
library(caret)     # package for cross validation

#setting the tree control parameters
fitControl <- trainControl(method = "cv", number = 5)
cartGrid <- expand.grid(.cp=(1:50)*0.01)

#decision tree
tree_model <- train(Item_Outlet_Sales ~ ., data = new_train, 
                    method = "rpart", trControl = fitControl, 
                    tuneGrid = cartGrid)
print(tree_model)

# best cp (complexity paramters) is 0.01 with RMSE 1115.619

# decision tree with cp = 0.01

main_tree <- rpart(Item_Outlet_Sales ~ .,
                   data = new_train, control = rpart.control(cp=0.01))
prp(main_tree)

# item_MRP as been marked as the most important variable (being the root node)
# check RMSE of this model to see if its better than regression

pre_score <- predict(main_tree, type = "vector")
rmse(new_train$Item_Outlet_Sales, pre_score)

# RMSE improved from 1140 to 1102.774 with decision trees.

# random forest

#load randomForest library
library(randomForest)

#set tuning parameters
control <- trainControl(method = "cv", number = 5)

#random forest model
rf_model <- train(Item_Outlet_Sales ~ ., data = new_train, 
                  method = "parRF", trControl = control, 
                  prox = TRUE, allowParallel = TRUE)

#check optimal parameters
print(rf_model)

# output best one is mtry = 15, at RMSE = 1120.297

#random forest model
forest_model <- randomForest(Item_Outlet_Sales ~ ., data = new_train, 
                               mtry = 15, ntree = 1000)
print(forest_model)
varImpPlot(forest_model)

# can only varImpPlot with randomForest
# Item_MRP is the most important variable
# we get Mean of SE = 1282586, RMSE = 1132.513

main_predict <- predict(main_tree, newdata = new_test, type = "vector")
