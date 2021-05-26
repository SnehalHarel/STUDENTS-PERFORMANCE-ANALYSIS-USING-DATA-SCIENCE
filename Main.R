######### Decision Tree Algorithm #############

#Adding Libraries
library(rpart)
library(rpart.plot)
library(e1071)
library(dplyr)
library(caret)
library(ggplot2)
library(caTools)
library(rattle)
library(RColorBrewer)

#Data Preprocessing 

#Import Dataset
data <- read.csv("C:/Users/lenovo/Desktop/ProjectF/RealDataset.csv")

#Exploring data

#check Diamentions of Data
dim(data)

#Check Null Values
anyNA(data)

#Class of data object
class(data)

#Summary of data
summary(data)

#preview of data with helpful details
str(data)

#Top 1 rows of a dataset  
head(data, n = 5) 



# Encoding categorical data
data$Grades= factor(data$Grades,
                    levels = c('Advanced', 'Average', 'Poor'),
                    labels = c(1, 2, 3))


#For Identical Result
set.seed(0123)

#Spliting records
train.rows<-sample(rownames(data), dim(data)[1]*0.60)
#setdiff means it will look for records not in train set
valid.rows<-sample(setdiff(rownames(data),train.rows), dim(data)[1]*0.10)
#setdiff means it will look for records not in train set and also valid set
test.rows<-setdiff(rownames(data),union(train.rows,valid.rows))


#See it's exactly 1000 records
#remove the row number
train.data<-data[train.rows, ]

valid.data<-data[valid.rows, ]

test.data<-data[test.rows, ]


#check Diamentions of Data
dim(train.data)
dim(valid.data)
dim(test.data)

#Check Null Values
anyNA(train.data)
anyNA(valid.data)
anyNA(test.data) 

#Preview of data with helpful details
str(train.data)
str(test.data)


#Run Classification tree model on train set
classtree <- rpart(formula = Grades~Result+ClassTest+Attendance+SSC+HSC, data = train.data, method = 'class', control = rpart.control(cp = 0))


#Plot the decision Tree
rpart.plot(classtree, box.palette="RdBu")


#Predict value at any point
test.data$pred <- predict(classtree,test.data, type = "class")



#Calculating accuracy.

# calculate overall accuracy
base_accuracy <-mean(test.data$pred == test.data$Grades)
base_accuracy

#Train Data Evaluate model - Missclassification error
p1<-predict(classtree,train.data,type = "class")
confusionMatrix(table(Predicted = p1,Actual= train.data$Grades))


#New Accurcy on Test Data
confusionMatrix(table(test.data$Grades,test.data$pred))



# Trees prepared with prepruning and Postpruning

#Prepruning
# prepare model
model_preprun <- rpart(formula = Grades~Result+ClassTest+Attendance+SSC+HSC,data = train.data, method = 'class', control = rpart.control(cp = 0, minsplit = 100, maxdepth = 8))

# prepare prediction
test.data$Predp <- predict(model_preprun,test.data, type = 'class')

# calculate overall accuracy
accuracy_preprun <-mean(test.data$Predp == test.data$Grades)
accuracy_preprun

# prepare confusion matrix
confusionMatrix(table(test.data$Predp,test.data$Grades))

# plot tree
rpart.plot(model_preprun,box.palette = "RdBu")



#Postpruning

# Prune the hr_base_model based on the optimal cp value
model_pruned <- prune(classtree, cp = 0.0084 )

# Compute the accuracy of the pruned tree
test.data$predpost <- predict(model_pruned, test.data, type = "class")

accuracy_postprun <- mean(test.data$predpost == test.data$Grades)
accuracy_postprun

# prepare confusion matrix
confusionMatrix(table(test.data$predpost,test.data$Grades))

# plot tree
rpart.plot(model_pruned,box.palette = "RdBu")

data.frame(base_accuracy, accuracy_preprun, accuracy_postprun)




#Output.csv
df <- data.frame(Sr.No = c(test.data$Sr.No),Name = test.data$Name,Gender = test.data$Gender,DOB = test.data$DOB,Result = c(test.data$Result),Grades=test.data$Grades,PPTotal =test.data$PPTotal,MRTotal =test.data$MRTotal,Gradepred = c(test.data$pred),Placement =test.data$Placement)
write.csv(df,"C:/Users/lenovo/Desktop/ProjectF/Doutput.csv", row.names = FALSE)


######### Multiple linear Regression Algorithm #############


library(ggplot2)
library(caTools)

# Multiple Linear Regression
#Multiple Regression Algorithm For classifying students Get Job or Not

#Data Preprocessing 
#import dataset
data <- read.csv("C:/Users/lenovo/Desktop/ProjectF/Doutput.csv")


#Exploring data
#check Diamentions of Data
dim(data)

#Check Null Values
anyNA(data) 

set.seed(0123)

#Spliting records
train.rows<-sample(rownames(data), dim(data)[1]*0.60)
#setdiff means it will look for records not in train set
valid.rows<-sample(setdiff(rownames(data),train.rows), dim(data)[1]*0.10)
#setdiff means it will look for records not in train set and also valid set
test.rows<-setdiff(rownames(data),union(train.rows,valid.rows))


#See it's exactly 900 records
#remove the row number
data_train<-data[train.rows, ]

data_valid<-data[valid.rows, ]

data_test<-data[test.rows, ]

#Check Null Values
anyNA(data_train)
anyNA(data_valid)
anyNA(data_test)

#check Diamentions of Data
dim(data_train)
dim(data_valid)
dim(data_test)

#Run Regression model on train set
Model<-lm(Placement~Gradepred+PPTotal+MRTotal,data =data_train)
summary(Model)

#Graph of Placement

G2<-ggplot(data = data_train,aes(x=Gradepred,fill=factor(Placement)))+geom_bar(stat ="count")+ggtitle("Number of placements: 0-Not Placed,1-Placed")+
  geom_text(stat = 'count',aes(label=..count..),vjust=2)+
  xlab("Gradepred = 1-Advanced, 2-Average,3-poor")+ylab("Total Number of Students")+facet_grid(. ~Placement)
G2

#Plaement Prediction
pred<-round(predict(Model,newdata = data_test))
data_test$pred<-round(predict(Model,newdata =data_test))
sigma(Model)/mean(data_test$pred)

#Calculating accuracy
confusionMatrix(table(predicted_class=data_test$pred,Actual=data_test$Placement))
cm<- confusionMatrix(table(predicted_class=data_test$pred,Actual=data_test$Placement))

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, '0', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, '1', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, '0', cex=1.2, srt=90)
  text(140, 335, '1', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  
draw_confusion_matrix(cm)


#Output.csv
df <- data.frame(Sr.No = c(data_test$Sr.No),Name = data_test$Name,Gender = data_test$Gender,DOB = data_test$DOB,Result = c(data_test$Result),Gradepred =c(data_test$Gradepred),Placement=c(data_test$Placement),Placepred =data_test$pred)
write.csv(df,"C:/Users/lenovo/Desktop/ProjectF/Multiple.csv", row.names = FALSE)

############### Anomaly Detection ##################

library(anomalize)
library(ggplot2)
library(outliers)


data<-read.csv("C:/Users/lenovo/Desktop/ProjectF/Multiple.csv")

#check Diamentions of Data
dim(data)

#Check Null Values
anyNA(data) 

set.seed(0123)

str(data)
data.hi<-subset(data,Placement<0.30)
data.hi
df<-data.frame(data.hi)
write.csv(df,"C:/Users/lenovo/Desktop/ProjectF/Anomaly.csv", row.names = FALSE)











