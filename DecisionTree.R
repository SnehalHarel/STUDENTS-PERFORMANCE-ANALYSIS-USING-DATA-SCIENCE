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



