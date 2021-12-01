library(psych)
library(ROCR)
set.seed(20)
dim(diabetes)
summary(diabetes)
boxplot(diabetes$Pregnancies)
boxplot(diabetes$Glucose)
boxplot(diabetes$SkinThickness)

id=sample(2,nrow(diabetes),prob = c(0.7,0.3),replace = TRUE)
id
diab_train=diabetes[id==1,]
diab_test=diabetes[id==2,]
View(diab_test)
View(diab_train)

sum(is.na(diabetes))
summary(diab_train)

library(rpart)
colnames(diabetes)
diab_model=rpart(Outcome~.,data=diab_train)
diab_model

plot(diab_model,margin=0.1)
text(diab_model,use.n = TRUE, pretty = TRUE,cex=0.8)

library(ROCR)
diab_P<-predict(diab_model,diab_train)
diab_P
diab_Pred <-prediction(diab_P,diab_train$Outcome)
ROCRPref<-performance(diab_Pred,"tpr","fpr")
plot(ROCRPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

pred_diab<-predict(diab_model, newdata= diab_test, type = "vector")>0.4

table(ActualValue=diab_test$Outcome,PredictedValue=pred_diab>0.4)
accuracy_score <-(134+55)/(134+17+44+55)
accuracy_score

library(caret)
library(e1071)
confusionMatrix(table(pred_diab,diab_test$Outcome))
confusionMatrix(table(pred_diab,diab_test$Outcome>0.5))

install.packages("randomForest")
library(randomForest)
diab_model<-randomForest(Outcome~.,data=diab_train)   

diab_model