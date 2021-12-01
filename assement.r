#create a data frame
setwd("D:/SKILL ENEBLE/R CLASS/assesments")
mydf.data=data.frame(name=c("A","B","C","D","E","F"),
                     age=c(1,2,3,4,5,6),
                     salary=c(10,20,30,40,50,60),
                     hobby=c("q","w","r","t","y","u"))
print(mydf.data)

colnames(mydf.data)=c('empname','empage','empsalary','emphobby')
print(mydf.data)

getwd()

write.table(mydf.data,file = "assesmet.csv",sep="\t",row.names = FALSE)

library(rio)
export(mydf.data,"assement.xlsx")

list.files()

min(grades$percent)
sd(grades$total)
median(grades$quiz4)
var(grades$percent)
range(grades$grade)
