setwd("C:\\Users\\Wild\\Desktop\\R Programming Course\\Machine Learning\\project1\\code")
library(caret)
library(kernlab)
library(ggplot2)
data<-read.csv("C:\\Users\\Wild\\Desktop\\R Programming Course\\Machine Learning\\project1\\data\\pml-training.csv")




inTrain<-createDataPartition(y=data$classe, p=0.60, list=FALSE)
training<-data[inTrain,]
testing<-data[-inTrain,]

#taken from http://mlarocca.github.io/07-23-2014/analysis.html since I found it is a great idea to refine data
treshold <- dim(training)[1] * 0.95
#Remove columns with more than 95% of NA or "" values
goodColumns <- !apply(training, 2, function(x) sum(is.na(x)) > treshold  || sum(x=="") > treshold)
#end of taken part

training <- training[, goodColumns]

testing <- testing[, goodColumns]



#retrieve the names of columns
names = colnames(testing)
names<-names[8:59]

#generate graphs for each variable to select the best ones
path="C:\\Users\\Wild\\Desktop\\R Programming Course\\Machine Learning\\project1\\materials\\features\\"
for (name in names) {
  fn<-paste(path,name,".jpg", sep = "")
  p<-ggplot(training, aes(training[,name], fill = classe)) + geom_histogram(alpha = 0.5, aes(y = ..count..), position = 'identity',binwidth = 5)
  p<-p+xlab(name) + ylab("Count")
  ggsave(filename=fn, plot=p,width=10, height=6)
}

path="C:\\Users\\Wild\\Desktop\\R Programming Course\\Machine Learning\\project1\\materials\\plottedTogether\\"
for (name1 in names){ 
  for (name2 in names){
    if(name1!=name2){
      fn<-paste(path,name1,"-",name2,".jpg", sep = "")
      strHead<-paste("Features ", name1, " and ", name2," plotted against each other")
      p1 <- 
        ggplot(training, aes(x=training[,name1], y=training[,name2], colour=classe)) +
        geom_point(alpha=.3) +
        ggtitle(strHead)
      p1<-p1+xlab(name1) + ylab(name2)
      ggsave(filename=fn, plot=p1,width=10, height=6) 
    }   
  }
}



#plt<-featurePlot(x=training[,c("accel_arm_x","accel_arm_y","accel_arm_z")], y=training[,c("accel_belt_x","accel_belt_y","accel_belt_z")],plot="pairs")
#png(filename="C:\\Users\\Wild\\Desktop\\R Programming Course\\Machine Learning\\project1\\materials\\features\\plot.png")
#plot(plt)
#dev.off()

#selected features

features=c("accel_arm_x","accel_arm_y","accel_arm_z",
           "accel_belt_z","accel_dumbbell_x","accel_dumbbell_y","accel_dumbbell_z",
           "accel_forearm_x","accel_forearm_y","accel_forearm_z",
           "magnet_forearm_x","magnet_forearm_y","magnet_forearm_z",
           "pitch_belt","pitch_dumbbell","pitch_forearm","roll_arm","roll_belt",
           "roll_dumbbell","roll_forearm","yaw_arm","yaw_belt","yaw_dumbbell",
           "yaw_forearm","classe")



trimmedTrain<-subset(training, select=features)
trimmedTest<-subset(testing, select=features)


mod1 <- train(classe ~ ., data=trimmedTrain, method="lda")
pred1 <- predict(mod1, trimmedTest)
confusionMatrix(pred1, trimmedTest$classe)

mod2 <- train(classe ~ ., data=trimmedTrain, method="gbm")
pred2 <- predict(mod2, trimmedTest)
confusionMatrix(pred2, trimmedTest$classe)



testData<-read.csv("C:\\Users\\Wild\\Desktop\\R Programming Course\\Machine Learning\\project1\\data\\pml-testing.csv")
#taken from http://mlarocca.github.io/07-23-2014/analysis.html since I found it is a great idea to refine data
treshold <- dim(testData)[1] * 0.95
#Remove columns with more than 95% of NA or "" values
goodColumns <- !apply(testData, 2, function(x) sum(is.na(x)) > treshold  || sum(x=="") > treshold)
#end of taken part


testData <- testData[, goodColumns]

trimmedTestData<-subset(testData, select=features[1:24])
answers <- predict(mod2, trimmedTestData)

answers<-as.character(answers)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("C:\\Users\\Wild\\Desktop\\R Programming Course\\Machine Learning\\project1\\results\\problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)


