install.packages("rlang")
library(readr)
Network_Intrusion_Train_data <- read.csv("C:/Users/Shaily/Desktop/Imarticus/R/Log_Reg/DT/Network_Intrusion_Train_data.csv",header=T,sep= ",", stringsAsFactors = T)
Network_Intrusion_Test_data <- read.csv("C:/Users/Shaily/Desktop/Imarticus/R/Log_Reg/DT/Network_Intrusion_Test_data.csv",header=T,sep= ",", stringsAsFactors = T)

Network_Intrusion_Train_data_X <- Network_Intrusion_Train_data[,1:41]
Network_Intrusion_Train_data_Y <- as.data.frame(Network_Intrusion_Train_data[,42])
colnames(Network_Intrusion_Train_data_Y) <- "class"

Network_Intrusion_Test_data_X <- Network_Intrusion_Test_data[,1:41]

#merge data for creating dummies using factors present in both test and train data sets

Network_Intrusion_data_X_merged <- rbind(Network_Intrusion_Train_data_X,Network_Intrusion_Test_data_X)
Network_Intrusion_data_X_merged
names(Network_Intrusion_data_X_merged)

#some transformations

transformed_data_X_merged <- as.data.frame(model.matrix(~.-1,data=Network_Intrusion_data_X_merged))
names(transformed_data_X_merged)

#After transformation again splitting the data into train and test data

Network_Intrusion_Train_data_X_dummy <- transformed_data_X_merged[(1:nrow(Network_Intrusion_Train_data_X)),]
Network_Intrusion_Test_data_X_dummy <- transformed_data_X_merged[nrow(Network_Intrusion_Train_data_X)+1:nrow(Network_Intrusion_data_X_merged),]

#begin modelling using basic decision tree

install.packages("tree")
library(tree)

model_tree <- tree(Network_Intrusion_Train_data_Y$class~.,data = Network_Intrusion_Train_data_X_dummy)

#Visualize the tree

x11(10,8) 
plot(model_tree)
text(model_tree)

#Explore the tree

model_tree

#confusion matrix
Network_Intrusion_Train_data_Y_Predicted <- predict(model_tree,Network_Intrusion_Train_data_X_dummy,type = "class")
conf <- table(Network_Intrusion_Train_data_Y$class,Network_Intrusion_Train_data_Y_Predicted)

#accuracy
acc <- sum(diag(conf)/sum(conf))
acc

#USING RANDOM FOREST

install.packages("gmodels")
library(gmodels)
CrossTable(Network_Intrusion_Train_data$class,Network_Intrusion_Train_data_Y_Predicted)

install.packages("pROC")
library("pROC")

plot(roc(Network_Intrusion_Train_data_Y$class~as.numeric(Network_Intrusion_Train_data_Y_Predicted)))

#use basic models for predictions and validations

Network_Intrusion_Test_data_Y_Predicted <- predict(model_tree,Network_Intrusion_Test_data_X_dummy,type = "class")
summary(Network_Intrusion_Test_data_Y_Predicted)

install.packages("randomForest")
library(randomForest)

#modelling with default number of trees
model_random <- randomForest(Network_Intrusion_Train_data_Y$class~.,Network_Intrusion_Train_data_X_dummy)
model_random
summary(model_random)

Network_Intrusion_Train_data_Y_Predicted <- predict(model_random,Network_Intrusion_Train_data_X_dummy)
summary(Network_Intrusion_Test_data_Y_Predicted)
plot(roc(Network_Intrusion_Train_data_Y$class~as.numeric(Network_Intrusion_Train_data_Y_Predicted)))

Network_Intrusion_Test_data_Y_Predicted <- predict(model_random,Network_Intrusion_Test_data_X_dummy)
summary(Network_Intrusion_Test_data_Y_Predicted)

#modelling with 50 number of trees
model_random <- randomForest(Network_Intrusion_Train_data_Y$class~.,Network_Intrusion_Train_data_X_dummy,ntree=50)
model_random
summary(model_random)

Network_Intrusion_Train_data_Y_Predicted <- predict(model_random,Network_Intrusion_Train_data_X_dummy)
summary(Network_Intrusion_Test_data_Y_Predicted)
plot(roc(Network_Intrusion_Train_data_Y$class~as.numeric(Network_Intrusion_Train_data_Y_Predicted)))

Network_Intrusion_Test_data_Y_Predicted <- predict(model_random,Network_Intrusion_Test_data_X_dummy)
summary(Network_Intrusion_Test_data_Y_Predicted)



