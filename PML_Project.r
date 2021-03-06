
plm_train<-read.csv("D:/My Stuff/Practical Machine learning/pml-training.csv")
plm_test<-read.csv("D:/My Stuff/Practical Machine learning/pml-testing.csv")


for(i in seq(1:160))
{
  print(paste("Column ",i))
  print(table(is.na(plm_train[,i])))
  
}

complete_train<-plm_train[complete.cases(plm_train),]

---------------------------------------------------------------------
  
# Random Forest
library(caret)
modFit<-train(plm_train$classe ~.,data=plm_train,method="rf", prox=TRUE)
modFit
getTree(modFit$finalModel,k=2)
rf_predict<-predict(modFit,newdata = plm_test)
imp_var <- varImp(modFit, scale = FALSE) 

# GLM
model <- train(plm_train$classe~.,data=plm_train,method="glm",family="binomial")
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(trainSA$chd, predict(model, trainSA))
missClass(testSA$chd, predict(model, testSA))

# SVM
with(plm_train, table(plm_train$classe, plm_train$user_name))

----------------------------------------------------------------------

library(caret)
gbmmodel <- (train(plm_train$classe~., data=plm_train, method="gbm"))
gbmImp <- varImp(gbmmodel, scale = FALSE)
gbmImp
 
# gbm variable importance
# only 20 most important variables shown (out of 6952)
# Overall
# X                              400.433
# avg_roll_dumbbell               22.035
# min_roll_forearm                 9.534
# var_accel_dumbbell               5.020
# max_roll_dumbbell                4.111
# kurtosis_yaw_arm-1.18751         0.000
# kurtosis_yaw_arm-1.00968         0.000
# kurtosis_picth_arm-0.98893       0.000
# kurtosis_picth_arm-1.26305       0.000
# skewness_pitch_dumbbell0.0169    0.000
# skewness_pitch_arm0.96238        0.000
# kurtosis_roll_belt-1.629928      0.000
# skewness_roll_belt.11.069879     0.000
# skewness_roll_forearm0.3081      0.000
# skewness_roll_dumbbell-0.0563    0.000
# skewness_roll_arm0.93328         0.000
# kurtosis_roll_arm-1.04555        0.000
# kurtosis_picth_dumbbell-0.5850   0.000
# kurtosis_picth_arm-1.23878       0.000
# skewness_pitch_forearm1.2207     0.000

plm_train_imp<-plm_train[,c(1,12,69,70,71,72,73,88,90,91,93,103,104,128,129,134)]
x<-summary(gbmmodel)
x

gbm.result <- predict(gbmmodel, plm_test)
confusionMatrix(plm_test$classe, gbm.result)$overall['Accuracy']
-------------------------------------------------------------------------
  
rpart_model <- train(plm_train$classe~.,data=plm_train,method="rpart")
summary(rpart_model)

# Variable importance
# X                       stddev_roll_belt 
# 38                              7 
# var_roll_belt           var_total_accel_belt 
# 7                              7 
# amplitude_pitch_belt    avg_roll_belt 
# 7                              7 
# min_roll_forearm        avg_pitch_forearm 
# 5                              4 
# pitch_forearm           amplitude_yaw_arm 
# 3                              3 
# var_accel_arm           cvtd_timestamp02/12/2011 14:57 
# 3                              2 
# cvtd_timestamp05/12/2011 11:24          
# avg_pitch_belt 
# 2                                              2 
# pitch_belt           raw_timestamp_part_1 
# 2                              1 

# ***************************************************************************

library(caret)
modFit<-train(complete_train$classe ~.,data=complete_train,method="rf", prox=TRUE)
modFit
getTree(modFit$finalModel,k=2)
rf_predict<-predict(modFit,newdata = plm_test)
imp_var <- varImp(modFit, scale = FALSE) 

# GLM
model <- train(complete_train$classe~.,data=complete_train,method="glm",family="binomial")
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(trainSA$chd, predict(model, trainSA))
missClass(testSA$chd, predict(model, testSA))

# SVM
with(plm_train, table(complete_train$classe, complete_train$user_name))

----------------------------------------------------------------------
  
library(caret)
gbmmodel <- (train(complete_train$classe~., data=complete_train, method="gbm"))
gbmImp <- varImp(gbmmodel, scale = FALSE)
gbmImp

install.packages("nnet")
library(nnet)
mod <- multinom(plm_reduced$classe~., data = plm_reduce)


plm_reduced<-plm_train[,-c(18,19,21,22,24,25,27:36,50:59,75:83,93,94,96,97,99,100,103:112,131,132,134,135,137:138,141:150)]


