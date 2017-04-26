# Coursera Getting and Cleaning Data Course Project
# Filename : run_analysis.R
# Description:
# This script will perform the following:
#   1. Merges the training and the test sets to create one data set.
#   2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#   3. Uses descriptive activity names to name the activities in the data set
#   4. Appropriately labels the data set with descriptive activity names. 
#   5. From the data set in #4, creates a second, independent tidy data set with the average of 
#      each variable for each activity and each subject. 

###### 1. Merges the training and the test sets to create one data set. #####

# Read in the data from files
activityType <- read.table('./activity_labels.txt',header=FALSE); 
features <- read.table('./features.txt',header=FALSE); 

# Read training files
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE); 
xTrain <- read.table('./train/X_train.txt',header=FALSE); 
yTrain <- read.table('./train/y_train.txt',header=FALSE); 

# Assign descriptive column names 
colnames(activityType) <- c('activityId','activityType');
colnames(subjectTrain) <- "subjectId";
colnames(xTrain) <- features[,2]; 
colnames(yTrain) <- "activityId";

# merging subjectTrain, xTrain and yTrain
trainingData <- cbind(subjectTrain,xTrain,yTrain);

# Read test files
subjectTest <- read.table('./test/subject_test.txt',header=FALSE); 
xTest <- read.table('./test/X_test.txt',header=FALSE); 
yTest <- read.table('./test/y_test.txt',header=FALSE); 

# Assign descriptive column names
colnames(subjectTest) <- "subjectId";
colnames(xTest) <- features[,2]; 
colnames(yTest) <- "activityId";


# merging subjectTest,xTest and yTest 
testData <- cbind(subjectTest,xTest,yTest);

# merging training and test data to create ONE data set
finalData <- rbind(trainingData,testData);

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames <- colnames(finalData); 

##### 2. Extract only the measurements on the mean and standard deviation for each measurement. #####

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector to keep only desired columns
finalData <- finalData[logicalVector==TRUE];

##### 3. Use descriptive activity names to name the activities in the data set #####

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData <- merge(finalData,activityType,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames <- colnames(finalData); 

##### 4. Appropriately label the data set with descriptive activity names. #####

for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("-std$","StdDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("^(t)","time",colNames[i])
  colNames[i] <- gsub("^(f)","freq",colNames[i])
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) <- colNames;

##### 5. Create a second, independent tidy data set with the          #####
#####    average of each variable for each activity and each subject. #####

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType <- finalData[,names(finalData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData <- merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
