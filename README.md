# Getting_and_Cleaning_Data
Content created for the Johns Hopkins Data Science Specialization on Coursera  

##Summary of pertinent code from run_analysis.R file. Descriptions of functionality appear above the code chunks.


Read in the two sample groups (test and training), assign the desired column names, add a column to identify the subject, and then combine both dataframes for a complete dataset of all subjects and observations.
```{r}
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
```

Prepare to remove the desired columns (the subjectid, activityname, and all mean or std columns). This uses vectors with a length equal to the column dimension of the dataframe, and indexing, in order to accomplish the task.
```{r}
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
```


Replace the activity label integers with the desired human-readable text, and rename columns to fit standards.
```{r}
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

```

Finally, group, summarize and output the resulting data as specified in the course project instructions.
```{r}
#group and summarize into the intended wide format tidy output  for the dataframe
grpd <- group_by(df_with_desiredcol, subjectid, activityname)
widetidy <- summarise_each(grpd, funs(mean), -subjectid, -activityname)


#then output the file with write file to a text file for upload
write.table(widetidy, file = "KCM_ProjectOutput.txt",row.names = FALSE)
```
