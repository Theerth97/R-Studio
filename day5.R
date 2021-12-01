library(readr)
cs <- read_csv("D:\\SKILL ENEBLE\\R CLASS\\data sets\\movie_metadata.csv")
View(cs)
View(cs)

csmatrix<-data.matrix(cs)
View(csmatrix)
dim(csmatrix)

is.na(csmatrix)
sum(is.na(csmatrix))

sample=csmatrix[sample(nrow(csmatrix),500),]
View(sample)
dim(sample)
describe(sample)

wss=NA
for (i in 2:15) wss[i]<-sum(kmeans(sample_matrix,centers=i)$withinss)
plot(1:15,wss,type="b",xlab="Number of Clusters", ylab = "within Sum of Squares")



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


plotarima
cl=kmeans(sample_matrix,3,nstart = 25)
cl