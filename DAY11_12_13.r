cs<-read.csv("D:\\SKILL ENEBLE\\R CLASS\\data sets\\movie_metadata.csv")
View(cs)
library(psych)
describe(cs)
summary(cs)
head(cs)
dim(cs)

csmatrix<-data.matrix(cs)
View(csmatrix)
dim(csmatrix)

#is.na(csmatrix)
sum(is.na(csmatrix))
summary(csmatrix)
dim(csmatrix)
csmatrix<-na.omit(csmatrix)
dim(csmatrix)
summary(csmatrix)

#nrow(csmatrix)
# selecting sample
sample<- csmatrix[sample(nrow(csmatrix),500),]
View(sample)
dim(sample)

library(psych)
describe(sample)  #to observe the column location of budget and gross

#selecting facebook likes(c9,c23)for cluster analysis
sample_short<-sample[,c(9,23)]
View(sample_short)
sample_matrix<- data.matrix(sample_short)

# Elbow curve
#wss <- (nrow(sample_matrix)-1)*sum(apply(sample_matrix,2,var))
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

aggregate(data=sample,gross~cl$cluster,mean)




