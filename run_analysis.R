
#1.Merge the training and the test sets to create one data set.

# loading the required datasets

actlab<-read.table("activity_labels.txt")
features<-read.table("features.txt")
xtrain<-read.table("X_train.txt")
ytrain<-read.table("y_train.txt")
trainsub<-read.table("subject_train.txt")
names(xtrain)<-as.character(features[,2])


## converting the activity number label to activity name
actlab$V2<-as.character(actlab$V2)
appy<-vector()
for(i in 1:nrow(ytrain))
{
        for(j in 1:nrow(actlab)){
         if(ytrain[i,1]==actlab[j,1])
         {
                appy<-append(appy,actlab[j,2]) 
         }
        }

        
}
ytrain$activity<-appy


##mearging all datasets related to train
newtrain<-cbind(trainsub,ytrain$activity,xtrain)
library(plyr)
newtrain<-rename(newtrain,c("V1"="subjects"))



#loading the test dataset
testsub<-read.table("subject_test.txt")
xtest<-read.table("X_test.txt")
ytest<-read.table("y_test.txt")
names(xtest)<-as.character(features[,2])

actlab$V2<-as.character(actlab$V2)
app<-vector()
for(i in 1:nrow(ytest))
{
        for(j in 1:nrow(actlab)){
                if(ytest[i,1]==actlab[j,1])
                {
                        app<-append(app,actlab[j,2]) 
                }
        }
        
        
}
ytest$activity<-app

##mearging all datasets related to test
newtest<-cbind(testsub,ytest$activity,xtest)

newtest<-rename(newtest,c("V1"="subjects"))

newtrain<-rename(newtrain,c("ytrain$activity"="activity"))
newtest<-rename(newtest,c("ytest$activity"="activity"))


#Merging train and test dataset
newtraintest<-rbind(newtrain,newtest)




## 2. Extract only the measurements on the mean and standard deviation for each measurement.


filter<-grep("mean|std",names(newtraintest))

newtraintest<-newtraintest[,c(1,2,filter)]

# 4. Appropriately label the data set with descriptive variable names.
names(newtraintest)
names(newtraintest)<-gsub("mean"," Mean ",names(newtraintest))
names(newtraintest)<-gsub("std"," Std ",names(newtraintest))
names(newtraintest)<-gsub('[()]',' ',names(newtraintest))

#5. From the data set in step 4, creates a second, independent tidy data set
#with the average of each variable for each activity and each subject.

newtraintest$subjects<-as.factor(newtraintest$subjects)
newtraintest$activity<-as.factor(newtraintest$activity)

library(reshape2)
melted <- melt(newtraintest, id = c("subjects", "activity"))

newmelted <- dcast(melted, subjects + activity ~ variable, mean)

write.csv(newmelted,"tidy.csv")




