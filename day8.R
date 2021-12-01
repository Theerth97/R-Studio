t.test(cs2m$Age,mu=49,conf.level = 0.9)
t.test(x=grades$quiz1,y=grades$quiz2,alternative = "two.sided",mu=0,paired = TRUE)
t.test(cs2m$BP=cs2m$AnxtyLH)

x1=rnorm(20,0,5)
x2=rpois(20,2)
x3=rpois(20,5)
x4=rpois(20,2)
x5=rnorm(20,2)
df=data.frame(x1,x2,x3,x4,x5)
df

df[df$x4==0,]
df[df$x4==7,]
df[df$x4==4,]

x1<-rnorm(20,0.5)
x2<-rpois(20,2)
x3<-rpois(20,5)
x4<-rpois(20,10)
x5<-rnorm(20,2)
df<-data.frame(x1,x2,x3,x4,x5)
df
df[df$x4==7,]



describe(grades)

library(Hmisc)
hist(grades$gpa)

stem(grades$gpa)
plot(grades$gpa,grades$final)
boxplot(grades$gpa)

cor(grades$gpa,grades$final)

corr.test(grades$gpa,grades$final)


sample_means = rep(NA, 1000)
for(i in 1:1000){
  sample_means[i] = mean(rexp(40,0.2))
}

sample_means = rep(NA, 1000)
for(i in 1:1000){
  sample_means[i] = mean(rexp(40,0.2))
}
var(sample_means)
hist(sample_means)
mean(sample_means)

hist(sample_means, main = "", xlab = "Sample Means", prob = T, col = "darkred")
lines(density(sample_means), col = "darkblue", lwd = 2)



y<-rnorm(1000)
z <- rep(NA,1000)
mynewdata= sample(c(y,z),10)
mynewdata

y<-rnorm(1000)
z <- rep(NA,1000)
mynewdata= sample(c(y,z),10)
mynewdata

boxplot(mtcars$disp~mtcars$gear, subset = (mtcars$am == 0), 
        xlab = "gear", ylab = "disp", main = "Automatic")
boxplot(mtcars$disp~mtcars$gear, subset = (mtcars$am == 1),
        xlab = "gear", ylab = "disp", main = "Manual")
boxplot(mtcars$disp~mtcars$gear, subset = (mtcars$am == 0), 
        xlab = "gear", ylab = "disp", main = "Automatic")
boxplot(mtcars$disp~mtcars$gear, subset = (mtcars$am == 1),
        xlab = "gear", ylab = "disp", main = "Manual")


mtcars_aov2 <- aov(mtcars$disp~factor(mtcars$gear) * 
                     factor(mtcars$am))
summary(mtcars_aov2)


