  #Set working directory
  setwd("./Course_3")
  #list files to view the existing files
  list.files()
  #Unzip the downloaded dataset an assign a new name for user convenience
  HARData<- "HARDataset.zip"
  unzip(HARData)
  #Check the files after unziping
  list.files()
  list.files("UCI HAR Dataset")
  
  ## 1. Merge the training and the test sets to create one data set.
  
  #imports features.txt
  features<- read.table('./UCI HAR Dataset/features.txt',header=FALSE)
  #imports activity_labels.txt
  actlab<- read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE)
  #imports subject_train.txt
  subtrain<- read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE)
  #imports x_train.txt
  xTrain<- read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE)
  #imports y_train.txt
  yTrain<- read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE)
  
  # Assigin column names 
  colnames(actlab) = c('activityId','activityType')
  colnames(subtrain) = "subjectId"
  colnames(xTrain) = features[,2]
  colnames(yTrain) = "activityId"
  
  #combine all training data
  
  train<- cbind(yTrain, subtrain, xTrain)
  
  # Reading test data
  #imports subject_test.txt
  subtest<- read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE)
  #imports x_test.txt
  xTest<- read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE)
  #imports y_test.txt
  yTest<- read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE)
  
  # Assign column names to the test data imported above
  colnames(subtest) = "subjectId";
  colnames(xTest) = features[,2]; 
  colnames(yTest) = "activityId";
  
  #combine all test data
  test<- cbind(yTest,subtest,xTest)
  #Combine training data and test data
  
  final<- rbind(train, test)
  
  #Create a vector for the column names from the finalData, which will be used
  # to select the desired mean() & stddev() columns
  colNames<-colnames(final)
  
  # 2. Extract only the measurements on the mean and standard deviation for each measurement. 
  
  # Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
  logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
  
  # Subset finalData table based on the logicalVector to keep only desired columns
  final<- final[logicalVector==TRUE]
  
  # 3. Use descriptive activity names to name the activities in the data set
  
  # Merge the finalData set with the acitivityType table to include descriptive activity names
  final<- merge(final,actlab,by='activityId',all.x=TRUE)
  
  # Updating the colNames vector to include the new column names after merge
  colNames<- colnames(final)
  
  # 4. Appropriately label the data set with descriptive activity names. 
  
  # Cleaning up the variable names
  for (i in 1:length(colNames)) 
  {
    colNames[i] = gsub("\\()","",colNames[i])
    colNames[i] = gsub("-std$","StdDev",colNames[i])
    colNames[i] = gsub("-mean","Mean",colNames[i])
    colNames[i] = gsub("^(t)","time",colNames[i])
    colNames[i] = gsub("^(f)","freq",colNames[i])
    colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
    colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
    colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
    colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
    colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
    colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
    colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
  }
  
  # Reassigning the new descriptive column names to the finalData set
  colnames(final) = colNames
  
  # 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
  
  # Create a new table, finalDataNoActivityType without the activityType column
  finalNoactlab  = final[,names(final) != 'actlab']
  
  # Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
  tidyData    = aggregate(finalNoactlab[,names(finalNoactlab) != c('activityId','subjectId')],by=list(activityId=finalNoactlab$activityId,subjectId = finalNoactlab$subjectId),mean)
  
  # Merging the tidyData with activityType to include descriptive acitvity names
  tidyData    = merge(tidyData,actlab,by='activityId',all.x=TRUE)
  
  # Export the tidyData set 
  write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')
