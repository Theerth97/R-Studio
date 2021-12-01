dim(cs2m)
length(cs2m$Prgnt)
median(cs2m$BP)

set.seed(2)
id=1:10
salary=sample(20:50,10,replace = TRUE )
df=data.frame(id,salary)
print(df)

library(Hmisc)
df$salary_group=as.numeric(cut2(df$salary,g=5))
print(df)

myadd=function(x,y)
{print(paste(x+y))}
myadd(2,3)
myadd(8,2)
head(cs2m,2)
tail(cs2m)
which(cs2m$BP>=170)
table(cs2m$AnxtyLH)
summary(cs2m)

library(psych)
describe(cs2m)

hist(cs2m$BP,col = "red")
stem(cs2m$BP)
boxplot(cs2m$BP)
plot(cs2m$BP,cs2m$Chlstrl,col="red")
plot(cs2m)
cor(cs2m)
pie(cs2m,col = rainbow)

dose=c(20,30,40,50,60)
druga=c(12,34,56,76,45)
drugb=c(12,13,17,18,19)

par(pin=c(2,3))
par(lwd=3, cex= 1.5)
par(cex.axis=0.75,font.axis=3)
plot(dose,druga,type="b",pch=23,lty=2,col="blue",bg="green")