Enter file contents here## Coursera Getting and Cleaning Data Course Project

# runAnalysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names. 
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##########################################################################################################

# 1. Merge the training and the test sets to create one data set.

#set working directory to the UCI HAR Dataset
setwd('C:/Users/Stephen Iaquaniello/Course3_Project/UCI HAR Dataset/')

# Read in the data 
features  <-  read.table('./features.txt',header=FALSE) #imports features.txt
activity_labels  <-  read.table('./activity_labels.txt',header=FALSE) #imports activity_labels.txt
subject_train  <-  read.table('./train/subject_train.txt',header=FALSE) #imports subject_train.txt
x_train  <-  read.table('./train/x_train.txt',header=FALSE) #imports x_train.txt
y_train  <-  read.table('./train/y_train.txt',header=FALSE) #imports y_train.txt

# Assigin column names to the data 
colnames(activity_labels) <- c('activityId','activityType')
colnames(subject_train) <- "subjectId"
colnames(x_train) <- features[,2]
colnames(y_train) <- "activityId"

# Create the final training set by merging y_train, subject_train, and x_train
trainingData <- cbind(y_train,subject_train,x_train)

# Read in the test data
subject_test <- read.table('./test/subject_test.txt',header=FALSE) #imports subject_test.txt
x_test <- read.table('./test/x_test.txt',header=FALSE) #imports x_test.txt
y_test <- read.table('./test/y_test.txt',header=FALSE) #imports y_test.txt

# Assign column names to the test data imported above
colnames(subject_test) <- "subjectId"
colnames(x_test) <- features[,2]
colnames(y_test) <- "activityId"

# Create the final test set by merging the xTest, yTest and subjectTest data
testData <- cbind(y_test,subject_test,x_test)

# Combine training and test data to create a final data set
FinalData  <-  rbind(trainingData,testData)


# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

library(dplyr)
FinalData <- FinalData[ , !duplicated(colnames(FinalData))]

FinalDataAct <- select(FinalData, contains("activityId"))
FinalDataSub <- select(FinalData, contains("subject"))
FinalDataMean <- select(FinalData, contains("mean"))
  FinalDataMean <- select(FinalDataMean, -contains("angle"))
  FinalDataMean <- select(FinalDataMean, -contains("meanFreq"))
FinalDataStd<- select(FinalData, contains("std"))

FinalData2 <- cbind(FinalDataAct,FinalDataSub,FinalDataMean,FinalDataStd)

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
FinalData2  <-  merge(FinalData2,activity_labels,by='activityId',all.x=TRUE)


# 4. Appropriately label the data set with descriptive activity names. 

names(FinalData2)<-gsub("^t", "Time", names(FinalData2))
names(FinalData2)<-gsub("^f", "Frequency", names(FinalData2))
names(FinalData2)<-gsub("Acc", "Accelerometer", names(FinalData2))
names(FinalData2)<-gsub("Gyro", "Gyroscope", names(FinalData2))
names(FinalData2)<-gsub("Mag", "Magnitude", names(FinalData2))
names(FinalData2)<-gsub("BodyBody", "Body", names(FinalData2))
names(FinalData2)


# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

library(plyr)
Average<-aggregate(. ~subjectId + activityType, FinalData2, mean)
Average<-Average[order(Average$subjectId,Average$activityType),]
write.table(Average, file = "tidydata.txt",row.name=FALSE)
