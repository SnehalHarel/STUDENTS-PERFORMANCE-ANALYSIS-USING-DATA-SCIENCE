install.packages('anomalize')
install.packages('outliers')
library(anomalize)
library(ggplot2)
library(outliers)


data<-read.csv("C:/Users/lenovo/Desktop/ProjectF/Multiple.csv")
View(data)

#check Diamentions of Data
dim(data)

#Check Null Values
anyNA(data) 

set.seed(0123)

str(data)
data.hi<-subset(data,Placement<0.30)
data.hi
boxplot(Placement~Gradepred,data=data.hi)
df<-data.frame(data.hi)
write.csv(df,"C:/Users/lenovo/Desktop/ProjectF/Anomaly.csv", row.names = FALSE)