GETTING AND CLEANING DATA

###COURSE PROJECT DESCRIPTION###

You should create one R script called run_analysis.R that does the following:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names.
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

###END OF COURSE PROJECT DESCRIPTION###

##This file describes how run_analysis.R script works##

1. Unzip the data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip in your working directory.
2. Source "run_analysis.R" in RStudio.
3. The script will create tidydata.txt file in the working directory.
4. There are 6 activities and 30 subjects in total, which result in 180 rows and 66 variables, which are calculated as mean values of original variables for one subject / activity pair.

#Dependencies#

run_analysis.R will install the dependencies automatically. It depends on reshape2, data.table, and dplyr packages.
