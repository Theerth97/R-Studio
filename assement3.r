cs<-read.csv("D:\\SKILL ENEBLE\\R CLASS\\data sets\\train.csv")
View(cs)

ds<-read.csv("D:\\SKILL ENEBLE\\R CLASS\\data sets\\test.csv")
View(ds)

es<-read.csv("D:\\SKILL ENEBLE\\R CLASS\\data sets\\gender_submission.csv")
View(es)

library(psych)
describe(cs)
summary(cs)
head(cs)
dim(cs)
str(cs)
csmatrix<-data.matrix(cs)
View(csmatrix)
dim(csmatrix)

model=lm(cs$Age~cs$Survived)
model
summary(model)

prd=predict(lm(cs$Age~cs$Survived))
prd
cs$prediction=prd
View(cs)

error=residuals(lm(cs$Age~cs$Survived))
error               
cs$error=error
View(cs)
hist(error)

model1=glm(cs$Survived~cs$Sex+cs$Pclass+cs$PassengerId,cs,family="binomial")
model1
summary(model1)
model1$coefficients
exp(coef(model1))

resthr=predict(model1,cs,type = "response")
library(ROCR)
rocrprd=prediction(resthr,cs$Survived)
rocrprf=performance(rocrprd,"tpr","fpr")
plot(rocrprf,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
res=predict(model1,ds,type = "response")
res2=predict.glm(model1,ds,type = "response")
table(Actualval=cs$Survived,predictval=res>0.3)
table(Actualval=cs$Survived,predictval=res2>0.3)

prval=as.data.frame(res)
View(prval)

error1=residuals(lm(cs$Age~cs$Survived))
error1               
cs$error1=error1
View(cs)
hist(error1)

cs$prediction=model1
View(cs)

sum(is.na(csmatrix))
summary(csmatrix)
dim(csmatrix)
csmatrix<-na.omit(csmatrix)
dim(csmatrix)
summary(csmatrix)


sample<- csmatrix[sample(nrow(csmatrix),500),]
View(sample)
dim(sample)

library(psych)
describe(sample)

sample_short<-sample[,c(1,5,3)]
View(sample_short)
sample_matrix<- data.matrix(sample_short)


wss=NA
for (i in 2:15) wss[i]<-sum(kmeans(sample_matrix,centers=i)$withinss)
plot(1:15,wss,type="b",xlab="Number of Clusters", ylab = "within Sum of Squares")

#k means clustering for k=3
cl <- kmeans(sample_matrix,3,nstart=25)
cl

plot(sample_matrix,col=(cl$cluster+1),
     main="k~means reslt with 2 clster",pch=1,cex=1,las=1)
points(cl$centers,col="red",pch=8,cex=2)

cl$centers
dim(cl)

aggregate(data=sample,sex~cl$cluster,mean)




