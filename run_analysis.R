# load the necessary packages
library(tidyverse)
library(data.table)

# download and read the data into R
courseProjUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
courseProjPath <- "./data/Getting-Cleaning-Data-Course-Project"

download.file(courseProjUrl, courseProjPath)
unzip(courseProjPath)

# assign all needed data sets to variables (while setting column
# names to understandable names that match where they need to)
features <- fread("UCI HAR Dataset/features.txt", col.names = c("n", "functions"))
activity <- fread("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subjectTest <- fread("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
xTest <- fread("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
yTest <- fread("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subjectTrain <- fread("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
xTrain <- fread("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
yTrain <- fread("UCI HAR Dataset/train/y_train.txt", col.names = "code")


# merge the training and test sets to create one data set
X <- rbind(xTrain, xTest)
Y <- rbind(yTrain, yTest)
subjectID <- rbind(subjectTrain, subjectTest)

mergedData <- cbind(subjectID, Y, X)

# extract only the measurements for the mean and standard deviation
# for each measurement
imp_data <- mergedData %>% select(subject, code, contains("mean"), contains("std"))

# use descriptive activity names to name the activities
imp_data$code <- activity[imp_data$code, 2]

# appropriately label the data set with descriptive variable names
names(imp_data)[2] = "activity"
names(imp_data) <- gsub("Acc", "Accelerometer", names(imp_data))
names(imp_data) <- gsub("tBody", "TimeBody", names(imp_data))
names(imp_data) <- gsub("-mean()", "Mean", names(imp_data), ignore.case = TRUE)
names(imp_data) <- gsub("-std()", "STD", names(imp_data), ignore.case = TRUE)
names(imp_data) <- gsub("Gyro", "Gyroscope", names(imp_data))
names(imp_data) <- gsub("Mag", "Magnitude", names(imp_data))
names(imp_data) <- gsub("^t", "Time", names(imp_data))
names(imp_data) <- gsub("^f", "Frequency", names(imp_data))
names(imp_data) <- gsub("BodyBody", "Body", names(imp_data))
names(imp_data) <- gsub("-freq()", "Frequency", names(imp_data), ignore.case = TRUE)
names(imp_data) <- gsub("angle", "Angle", names(imp_data))
names(imp_data) <- gsub("gravity", "Gravity", names(imp_data))

# from the data set created above, create a second, independent tidy
# data set with the average of each variable for each activity and
# each subject
final_data <- imp_data %>% 
        group_by(subject, activity) %>% 
        summarize(across(3:86, mean))

write.table(final_data, "final_data.txt", row.names = FALSE)