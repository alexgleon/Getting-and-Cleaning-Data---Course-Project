#26/03/2017 Oliver González
#Peer-graded Assignment: Getting and Cleaning Data Course Project

#Content and Objectives: 
#Obj 1.Merges the training and the test sets to create one data set.
#Obj 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#Obj 3.Uses descriptive activity names to name the activities in the data set
#Obj 4.Appropriately labels the data set with descriptive variable names. 
#Obj 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Obj 1: Merges the training and the test sets to create one data set.
#1.0 Preparing environment, Getting data
  rm(list=ls())
  #1.0.1 First set the working directory to simplify the code
  setwd("~/RWkngdrctry/Week 4/UCI HAR Dataset")

  #1.0.2 Reading training sets
  xtrain <- read.table("./train/X_train.txt",header=FALSE)
  ytrain <- read.table("./train/y_train.txt",header=FALSE)
  subjecttrain <- read.table("./train/subject_train.txt",header=FALSE)
  
  #1.0.3 Reading testing sets
  xtest <- read.table("./test/X_test.txt",header=FALSE)
  ytest <- read.table("./test/y_test.txt",header=FALSE)
  subjecttest <- read.table("./test/subject_test.txt",header=FALSE)
  
  #1.0.4 Reading feature and activity sets
  features     = read.table('./features.txt')
  activity_labels = read.table('./activity_labels.txt') 
  
#1.1 Assign column names to the data imported above train, test and Activity
  colnames(xtrain) <- features[,2] 
  colnames(ytrain) <-"activityId"
  colnames(subjecttrain) <- "subjectId"

  colnames(xtest) <- features[,2] 
  colnames(ytest) <- "activityId"
  colnames(subjecttest) <- "subjectId"
  
  colnames(activity_labels) <- c('activityId','activityType')

#1.2 Merging all togheter
  mrg_train <- cbind(ytrain, subjecttrain, xtrain)
  mrg_test <- cbind(ytest, subjecttest, xtest)
  fnl_mrg <-rbind(mrg_train, mrg_test)
  
#Obj 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
  #2.1 get column names from final merge
  colNames  = colnames(fnl_mrg)
  
  #2.2 vector for mean stardard deviation 
  mean_std <- (grepl("activity.." , colNames) | grepl("subject.." , colNames) | 
               grepl("mean.." , colNames) | grepl("std.." , colNames)
              )
  
  #2.3 creating subsets from fnl_mrg 
  finalset_mean_std <- fnl_mrg[ , mean_std == TRUE]

#Obj 3.Uses descriptive activity names to name the activities in the data set
  finalset_ActivityNames <- merge(finalset_mean_std, activity_labels, by='activityId',
                                all.x=TRUE)
  finalset_ActivityNames$activityId <-activity_labels[,2][match(finalset_ActivityNames$activityId, activity_labels[,1])]   

  columns <- colnames(finalset_ActivityNames)  
  
#Obj 4.Appropriately labels the data set with descriptive variable names.   
  for (i in 1:length(columns)) 
  {
    columns[i] <- gsub("\\()","",columns[i])
    columns[i] <- gsub("-std$","StdDev",columns[i])
    columns[i] <- gsub("-mean","Mean",columns[i])
    columns[i] <- gsub("^(t)","time",columns[i])
    columns[i] <- gsub("^(f)","freq",columns[i])
    columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
    columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
    columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
    columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
    columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
    columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
    columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
  }
  
  colnames(finalset_ActivityNames)<-columns;
  
  finalset_ActivityNames <- finalset_ActivityNames[,names(finalset_ActivityNames) != 'activityType'];
  
#Obj 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.  
  
  # 5.1 Making second tidy data set 
  scnd_TidySet <- aggregate(. ~subjectId + activityId, finalset_ActivityNames, mean)
  scnd_TidySet <- scnd_TidySet[order(scnd_TidySet$subjectId, scnd_TidySet$activityId),]
  
  # 5.2 Writing second tidy data set in txt file
  write.table(scnd_TidySet, "~/RWkngdrctry/Week 4/scnd_TidySet.txt", row.names=FALSE)  
  