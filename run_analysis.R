library(dplyr)
library(tidyverse)

filename <- "Coursera_DS3_Final.zip"

if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
  
  if (!file.exists("UCI HAR Dataset")) { 
      unzip(filename) 
  }
}

#1 Merges the training and the test sets to create one data set.

columns_names <- read_file('./UCI HAR Dataset/features.txt')
columns_names <- unlist(strsplit(columns_names, " "))

dataX_test <- read_delim('./UCI HAR Dataset/test/X_test.txt', delim = "  ",
                        col_names = columns_names[-1])
dataX_train <- read_delim('./UCI HAR Dataset/train/X_train.txt', delim = "  ",
                         col_names = columns_names[-1])


dataY_test <- read_file('./UCI HAR Dataset/test/y_test.txt')
dataY_train <- read_file('./UCI HAR Dataset/train/y_train.txt')

dataY_binded <- paste(dataY_test,dataY_train)
dataY_binded <- unlist(strsplit(dataY_binded, "[\\\\]|[^[:print:]]"))

data_binded <- rbind.data.frame(dataX_test, dataX_train)
#data_binded <- sapply(data_binded, as.double)
#data_binded <- lapply(data_binded, function(x) as.numeric(as.character(x)))
data_binded[1:dim(data_binded)[2]] <- sapply(data_binded[c(1:dim(data_binded)[2])],as.numeric)

data_binded$Activities <- dataY_binded

#2 Extracts only the measurements on the mean and standard deviation for each measurement.

std_and_mean_variables <- grepl("mean|(std)", columns_names)
data_binded <- data_binded[std_and_mean_variables]

#3 Uses descriptive activity names to name the activities in the data set
#based on activity labels:
    #1 WALKING
    #2 WALKING_UPSTAIRS
    #3 WALKING_DOWNSTAIRS
    #4 SITTING
    #5 STANDING
    #6 LAYING
for( i in c(1:length(dataY_binded))){
  if(dataY_binded[i] == "1"){
    dataY_binded[i] <- "WALKING"
  }else if(dataY_binded[i] == "2"){
    dataY_binded[i] <- "WALKING_UPSTAIRS"
  }else if(dataY_binded[i] == "3"){
    dataY_binded[i] <- "WALKING_DOWNSTAIRS"
  }else if(dataY_binded[i] == "4"){
    dataY_binded[i] <- "SITTING"
  }else if(dataY_binded[i] == "5"){
    dataY_binded[i] <- "STANDING"
  }else if(dataY_binded[i] == "6"){
    dataY_binded[i] <- "LAYING"
  } else{ dataY_binded[i] <- "NAN"}
  
}

data_binded$Activities <- dataY_binded

#4 Appropriately labels the data set with descriptive variable names.

columns_names <- names(data_binded)
columns_names <- gsub("[^ -~]","", columns_names)
columns_names <- gsub("[0-9]{1,3}$", "", columns_names)
columns_names <- gsub("tBodyAcc-", "Accelerometer_Body_Time_Signal-", columns_names)
columns_names <- gsub("tGravityAcc", "Accelerometer_Gravity_Vector_Time_Signal", columns_names)
columns_names <- gsub("tBodyAccJerk", "Accelerometer_Body_Jerk_Signal-", columns_names)
columns_names <- gsub("tBodyGyro-", "Gyroscope_Body_Time_Signal-", columns_names)
columns_names <- gsub("tBodyGyroJerk-", "Gyroscope_Body_Jerk_Signal-", columns_names)
columns_names <- gsub("fBodyAcc-", "Accelerometer_Body_Frequency_Signal-", columns_names)
columns_names <- gsub("fBodyAcc-", "Accelerometer_Body_Frequency_Signal-", columns_names)
columns_names <- gsub("fGravityAcc", "Accelerometer_Gravity_Vector_Frequency_Signal", columns_names)
columns_names <- gsub("fBodyAccJerk", "Accelerometer_Body_JerkFrequency_Signal-", columns_names)
columns_names <- gsub("fBodyGyro-", "Gyroscope_Body_Frequency_Signal-", columns_names)
columns_names <- gsub("fBodyGyroJerk-", "Gyroscope_Body_JerkFrequency_Signal-", columns_names)
columns_names <- gsub("fBodyAcc-", "Accelerometer_Body_Frequency_Signal-", columns_names)

names(data_binded) <- columns_names

write.csv(data_binded, "data_binded.csv")

#5 From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable for each activity and each subject.

data_test_subjects <- read_delim('./UCI HAR Dataset/test/subject_test.txt', delim = " ", col_names = c("Subjects"))
data_train_subjects <- read_delim('./UCI HAR Dataset/train/subject_train.txt', delim = " ", col_names = c("Subjects"))
data_subjects_binded <- rbind(data_test_subjects, data_train_subjects)
data_task5 <- data_binded
data_task5$subjects <- as.vector(unlist(data_subjects_binded))

data_task5_summarised <-data_task5 %>% 
      group_by(subjects, Activities) %>%
      summarise_each(funs(mean))

write.csv(data_task5_summarised, "data_task5_summarised.csv")