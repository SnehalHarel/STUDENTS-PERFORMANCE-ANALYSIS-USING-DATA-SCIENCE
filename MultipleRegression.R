
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

