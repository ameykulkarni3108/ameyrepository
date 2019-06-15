#=======================RIDGE REGRESSION========================
  
install.packages("caret")
install.packages("glmnet")

library(caret)
library(glmnet)
library(ISLR)

set.seed(123) 

tc = trainControl(method = "cv", number = 10,verboseIter = T)

A = read.csv(file.choose())

A = data.frame(Hitters)

A1 = na.omit(A)

model1 = train(PROFIT ~ RND + MKT ,method = "glmnet",data = A, trControl = tc,tuneGrid = expand.grid(alpha = 0,lambda = seq(0,1,length=20)))

model1 = train(Salary ~ AtBat + Hits + League ,method = "glmnet",data = A1, trControl = tc,tuneGrid = expand.grid(alpha = 0,lambda = seq(40,50,length=20)))

model1$results
varImp(model1)

model1$bestTune




model1(LOOK AT FINAL MODEL)

plot(model1)   (PLOT IT)

attributes(model1)  (SEE ITS ATTRIBUTES)

model1$finalModel  (LOOK AT THE SELECTED MODEL COEFFICIENTS)

plot(model1$finalModel,xvar="lambda")  (PLOT FINAL MODEL AGAINST PENALTY)

plot(model1$finalModel,xvar="dev")  (PLOT FINAL MODEL AGAINST PENALTY)

plot(varImp(model1,scale = FALSE))  (SEE VARIABLE SIGNIFICANCE...FEATURE SELECTION)

=======================LASSO REGRESSION========================
  
  library(caret)
library(glmnet)

set.seed(123) 

tc = trainControl(method = "cv", number = 10,verboseIter = T)

A = read.csv(file.choose())

model2 = train(PROFIT ~ . ,method = "glmnet",data = A, trControl = tc,tuneGrid = expand.grid(alpha = 1,lambda = seq(0.1,1,length=7)))

model2(LOOK AT FINAL MODEL)

plot(model1)   (PLOT IT)

attributes(model1)  (SEE ITS ATTRIBUTES)

model2$finalModel  (LOOK AT THE SELECTED MODEL COEFFICIENTS)

plot(model2$finalModel,xvar="lambda")  (PLOT FINAL MODEL AGAINST PENALTY)

#plot(model2$finalModel,xvar="dev")  (PLOT FINAL MODEL AGAINST PENALTY)

#plot(varImp(model2,scale = FALSE))  (SEE VARIABLE SIGNIFICANCE...FEATURE SELECTION)

#=================ELASTIC NET REGRESSION=================
  
  library(caret)
library(glmnet)

set.seed(123) 

tc = trainControl(method = "cv", number = 10,verboseIter = T)

A = read.csv(file.choose())

model3 = train(PROFIT ~ . ,method = "glmnet",data = A, trControl = tc,tuneGrid = expand.grid(alpha = seq(0,1,length=),lambda = seq(0.1,1,length=7)))

model3(LOOK AT FINAL MODEL)

plot(model3)   (PLOT IT)

attributes(model3)  (SEE ITS ATTRIBUTES)

model3$finalModel  (LOOK AT THE SELECTED MODEL COEFFICIENTS)