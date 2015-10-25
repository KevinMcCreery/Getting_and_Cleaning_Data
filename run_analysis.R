#Purpose: to clean, arrange and otherwisse prepare datasets for analysis,
#based on instruction provided with the Coursera Course "Getting and Cleaning Data" Project

#"You should create one R script called run_analysis.R that does the following:

#1)Merges the training and the test sets to create one data set.
#2)Extracts only the measurements on the mean and standard deviation for each measurement. 
#3)Uses descriptive activity names to name the activities in the data set
#4)Appropriately labels the data set with descriptive variable names. 
#5)From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject."


#Created by: Kevin McCreery for a Coursera Course from Johns Hopikins, October 2015



#setup, as needed.

#Download of material required to recreate: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#set the working directory to my desired local file path
#setwd("C:/Users/Kevin/Documents/R/Coursera Files/GettingCleaningData/UCI HAR Dataset/")
library(dplyr)
library(reshape2)
#library(RMySQL)
#library(DBI)

#get the IDs of the subjects of the observations
subtraining <- read.table("./train/subject_train.txt")
#get the features for col names
features <- read.table("./features.txt")
#read in the training data with features as col names
train <- read.table("./train/X_train.txt", col.names = features[,2])
#Get the activity labels of each of the observations for the training set
ytrain <- read.table("./train/y_train.txt")
#col bind the subtraining as the row / subject ID col to the train data
train2 <- cbind(subtraining, ytrain, train)


#get the IDs of the subjects of the observations
subtesting <- read.table("./test/subject_test.txt")
#read in the testing data with features as col names
test <- read.table("./test/X_test.txt", col.names = features[,2])
#Get the activity labels of each of the observations for the testing set
ytest <- read.table("./test/y_test.txt")
#col bind the subtraining as the row / subject ID col to the test data
test2 <- cbind(subtesting, ytest, test)


#Append the two data frames to have one df with all subjects and observations
combined <- rbind(test2, train2)



#Identify the columns that contain mean or std measurements, 
#and create a combined logical vector with the desired columns.
#Make sure I carry through the necessary first two columns (subject and activity), 
#that don't have mean or std in name, by creating a vector and putting the yes value in these index locations
idcol <- c(1,1, rep.int(0,561))
#Break out the desired column positions into vectors and combine, in order to subset
meancol <- as.numeric(grepl("mean", colnames(combined)))
stdcol <- as.numeric(grepl("std", colnames(combined)))
desiredcol <- as.vector(meancol + stdcol + idcol)

#Get the desired columns out of the original dataset
df_with_desiredcol <- combined[,which(as.logical(desiredcol))]


#Rplace all values of activity label with names
temp <- df_with_desiredcol[,2]
temp <- replace(temp, temp == 1, "walking")
temp <- replace(temp, temp == 2, "walking upstairs")
temp <- replace(temp, temp == 3, "walking downstairs")
temp <- replace(temp, temp == 4, "sitting")
temp <- replace(temp, temp == 5, "standing")
temp <- replace(temp, temp == 6, "laying")

#now replace the column in the dataframe with the new vector
df_with_desiredcol[,2] <- temp


#renaming column headers where necessary. Moving all hearders to lowercase, per instructions
colnames(df_with_desiredcol)[1] <- "SubjectID"
colnames(df_with_desiredcol)[2] <- "ActivityName"
colnames(df_with_desiredcol) <- tolower(colnames(df_with_desiredcol))


#group and summarize into the intended wide format tidy output  for the dataframe
grpd <- group_by(df_with_desiredcol, subjectid, activityname)
widetidy <- summarise_each(grpd, funs(mean), -subjectid, -activityname)


#then output the file with write file to a text file for upload
write.table(widetidy, file = "KCM_ProjectOutput.txt",row.names = FALSE)