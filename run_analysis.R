# Marc Vaglio-Laurin
# run_analysis.R
# Coursera Getting and Cleaning Data
# 24JAN2015
#
#=================================================================
# 1. Merges the training and the test sets to create one data set.
#=================================================================
#
X.test <- read.table("UCI HAR Dataset/test/X_test.txt")
y.test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject.test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X.train <- read.table("UCI HAR Dataset/train/X_train.txt")
y.train <- read.table("UCI HAR Dataset/train/y_train.txt")
subject.train <- read.table("UCI HAR Dataset/train/subject_train.txt")
test <- cbind(X.test, y.test, subject.test)
train <- cbind(X.train, y.train, subject.train)
data <- rbind(test, train)
#
#===========================================================================================
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#===========================================================================================
# 
features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
means.index <- grep("mean\\(\\)", features[,2])
std.index <- grep("std\\(\\)", features[,2])
features <- rbind(features[means.index,], features[std.index,])
features <- features[order(features[1]),]
features <- rbind(list(c(ncol(data)-1, ncol(data)), c("activity", "subject")), features)
data <- data[, features$V1]
#
#===========================================================================
# 3. Uses descriptive activity names to name the activities in the data set.
#===========================================================================
# 
activity.label <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)
activity.label$V2 <- tolower(activity.label$V2)
data <- merge(data, activity.label, by.x = "activity", by.y = "V1")
data$activity <- data$V2
data$V2 <- NULL
#
#======================================================================
# 4. Appropriately labels the data set with descriptive variable names.
#======================================================================
#
colnames(data) <- features$V2
#
#============================================================================
# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
#============================================================================
# 
data.melt <- melt(data, id=c("activity", "subject"))
data.cast <- dcast(data.melt, subject + activity ~ variable, mean)
write.table(data.cast, file = "tidy_data_set_marc_vaglio_laurin.txt", row.names = FALSE)
