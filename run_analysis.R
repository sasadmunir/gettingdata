run_analysis <- function (directory="UCI HAR Dataset"){
  
  # Check if the data directory exits
  if( !file.exists(directory) )
    return("Directory Not Found. Please check your current working directory.") 
  
  # Define substructure in the data directory
  trainUri <- "train"
  testUri <- "test"
  
  # Load feature labels and filter those of interest
  featureLabels <- "features.txt"
  fLabels <- read.table( paste(directory,featureLabels,sep="/") )
  myFLabels <- grepl("mean()",as.character(fLabels[,2]),fixed=TRUE) | grepl("std()",as.character(fLabels[,2]),fixed=TRUE)  # Filter features containing mean() and std() in them
  
  # Load Activity Labels
  activityLables <- "activity_labels.txt"
  aLabels <- read.table( paste(directory,activityLables,sep="/") )
  
  # Load training data and add back columns
  x_train <- read.table( paste(directory,trainUri,"x_train.txt",sep="/") )
  trainingData <- x_train[,myFLabels]  # Filter features
  colnames(trainingData) <- as.character(fLabels[myFLabels,2])  # Incroporate feature names
  y_train <- read.table( paste(directory,trainUri,"y_train.txt",sep="/") )
  subject_train <- read.table( paste(directory,trainUri,"subject_train.txt",sep="/") )
  y_trainDesc <- merge(y_train,aLabels)
  colnames(y_trainDesc) <- c("activity_no","activity_label")
  trainingData <- cbind(trainingData,y_trainDesc)
  trainingData <- cbind(trainingData,subject_train)
  colnames(trainingData)[length(trainingData)] <- "subject_id"
  
  # Load test data and add back columns
  x_test <- read.table( paste(directory,testUri,"x_test.txt",sep="/") )
  testData <- x_test[,myFLabels]
  colnames(testData) <- as.character(fLabels[myFLabels,2])
  y_test <- read.table( paste(directory,testUri,"y_test.txt",sep="/") )
  subject_test <- read.table( paste(directory,testUri,"subject_test.txt",sep="/") )
  y_testDesc <- merge(y_test,aLabels)
  colnames(y_testDesc) <- c("activity_no","activity_label")
  testData <- cbind(testData,y_testDesc)
  testData <- cbind(testData,subject_test)
  colnames(testData)[length(testData)] <- "subject_id"
  
  # Merge training dataset and test dataset
  tidyDataSet <- rbind(testData,trainingData)
  
  # Find average of features by Activity Labels
  actSplit <- split(tidyDataSet,tidyDataSet$activity_label)
  lbls <- as.character(fLabels[myFLabels,2])
  tidyActivities <- lapply(actSplit,function(x) colMeans(x[,lbls]))
  
  # Find average of features by Subjects
  tidyDataSet$subject_id <- factor(tidyDataSet$subject_id)
  subSplit <- split(tidyDataSet,tidyDataSet$subject_id)
  tidySubjects <- lapply(subSplit,function(x) colMeans(x[,lbls]))
  
  # Create list containing feature averages by feature and subject
  finalTidyData <- c(tidyActivities,tidySubjects)
  
  # Convert tidy data list to data frame for easy storage and manipulation
  tidyDataFrame <- data.frame(matrix(unlist(finalTidyData),nrow=length(finalTidyData),byrow=T))
  colnames(tidyDataFrame) <- lbls
  tidyDataFrame <- cbind(names(finalTidyData),tidyDataFrame)
}