# Load necessary packages
install.packages("data.table")
install.packages("reshape2")
install.packages("dplyr")

# 1. Merge the training and the test sets to create one data set.
path <- getwd()
testpath <- eval(paste(path, "/test/", sep=""))
trainpath <- eval(paste(path, "/train/", sep=""))

#load test and train data
test_X <- read.table(eval(paste(testpath, "X_test.txt", sep="")))
train_X <- read.table(eval(paste(trainpath, "X_train.txt", sep="")))
subject_test_X <- read.table(eval(paste(testpath, "subject_test.txt", sep="")))
subject_train_X <- read.table(eval(paste(trainpath, "subject_train.txt", sep="")))
#load activities data
activity_test_X <- read.table(eval(paste(testpath, "y_test.txt", sep="")))
activity_train_X <- read.table(eval(paste(trainpath, "y_train.txt", sep="")))

#merge subjects with the respective data
test_dat <- cbind(subject_test_X, test_X)
train_dat <- cbind(subject_train_X, train_X)
#merge activities with the respective data
test_dat_act <- cbind(activity_test_X, test_dat)
train_dat_act <- cbind(activity_train_X, train_dat)
#merge test data with train data
dat_full <- rbind(train_dat_act, test_dat_act)

#add features as labels to the merged data set
features_df <- read.table("features.txt")
features_factor <- as.character(features_df$V2)
features <- as.character(features_factor)
colnames(dat_full) <- c("activity", "subject", features)


# 2. Extract only measurements of the mean and standard deviation for each measurement.
extract_features <- grepl("mean\\(\\)|std\\(\\)", features)
dat <- dat_full[, extract_features]


# 3. Use descriptive activity names to name the activities in the data set
activity_labels_df <- read.table("activity_labels.txt")
activity_labels_factor <- activity_labels_df$V2
activity_labels <- as.character(activity_labels_factor)
#factorize variale "activity" in the data frame "dat" using activity IDs and activity_labels
dat$activity <- factor(dat$activity, labels = activity_labels)


# 4. Appropriately label the data set with descriptive variable names. 
# replace prefix t with time
names(dat) <- gsub("^t", "time", names(dat))
# replace Acc with Accelerometer
names(dat) <- gsub("Acc", "Accelerometer", names(dat))
# replace Gyro with Gyroscope
names(dat) <- gsub("^Gyro", "Gyroscope", names(dat))
# replace prefix f with frequency
names(dat) <- gsub("^f", "frequency", names(dat))
# replace Mag with Magnitude
names(dat) <- gsub("Mag", "Magnitude", names(dat))
# replace BodyBody with Body
names(dat) <- gsub("BodyBody", "Body", names(dat))


# 5. From the data set in previous step, create a second, independent tidy data set with
# the mean of each variable for each activity and each subject.
dat2 <- aggregate(. ~subject + activity, dat, mean)
dat2 <- dat2[order(dat2$subject,dat2$activity),]
write.table(dat2, file = "tidydata.txt", row.name = FALSE)
