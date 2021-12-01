library(readxl)
Redwine <- read_excel("D:/SKILL ENEBLE/R CLASS/data sets/Redwine.xlsx")
View(Redwine)

set.seed(20)
x=sample(c(0:100),replace = TRUE,1000)
y=x+sample(c(-10:10),replace=TRUE,1000)
plot(y~x)

pca.sample=prcomp(matrix(c(x,y),ncol=2))
summary(pca.sample)
pca.sample$rotation
rotation.matrix=-pca.sample$rotation
rotated.data=matrix(c(x,y),ncol=2)%%rotation.matrix
plot(rotated.data[,1],rotated.data[,2],xlim=c(0,150))

summary(Redwine)
dim(Redwine)
library(psych)
describe(Redwine)
range(Redwine)
cov(Redwine[,-12])
cor(Redwine[,-12])
egcov=eigen(cov(Redwine[,-12]))
egcor=eigen(cor(Redwine[,-12]))
egcov$values/sum(egcov$values)
egcor$values/sum(egcor$values)

View(red)
Redwinepr=prcomp(Redwine[,-12])
Redwineprscl=prcomp(Redwine[,-12],scale=TRUE)
summary(Redwinepr)
summary(Redwineprscl)
screeplot(Redwineprscl,type="barplot",col="Green")
screeplot(Redwineprscl,type="line",col="Green")
biplot(Redwineprscl)
cor(Redwine)
wine.pca <- pca(Redwine, quanti.sup = 12)
summary(wine.pca)

plot(wine.pca$eig$eigenvalue, type = 'b', xlab = 'PrincipalComponent', ylab = 'Eigenvalue', main = 'Eigenvalues of Principal
Components')
summary(wine.pca, ncp = 4)
