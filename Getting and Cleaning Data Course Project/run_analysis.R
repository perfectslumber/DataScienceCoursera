library(dplyr)

#Read data
x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")
subject_test <- read.table("test/subject_test.txt")
x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")
features <- read.table("features.txt", stringsAsFactors = FALSE)
labels <- read.table("activity_labels.txt")

#1 Merges the training and the test sets to create one data set.
train_data <- cbind(subject_train, y_train, x_train)
test_data <- cbind(subject_test, y_test, x_test)
total_data <- rbind(test_data, train_data)
colnames(total_data) <- c("subject", "activity", features[, 2]) # This has covered step 4

#2 Extracts only the measurements on the mean and standard deviation
#  for each measurement.
mean_index <- grep("mean", colnames(total_data))
std_index <- grep("std", colnames(total_data))
extracted_data <- total_data[, sort(c(1, 2, mean_index, std_index))]

#3 Uses descriptive activity names to name the activities in the data set
extracted_data[, 2] <- as.factor(extracted_data[, 2])
levels(extracted_data[, 2]) <- tolower(labels[, 2])

#4 Appropriately labels the data set with descriptive variable names.

## THIS IS ALREADY DONE IN STEP 1

#5 From the data set in step 4, creates a second, independent tidy data set
#   with the average of each variable for each activity and each subject.
extracted_data[, 1] <- as.factor(extracted_data[, 1])
colnames(extracted_data) <- gsub("-", "", colnames(extracted_data))
colnames(extracted_data) <- gsub("\\(\\)", "", colnames(extracted_data))
    # remove minus signs and brackets in coloumn names which would be confusing for analysis
extracted_data <- tbl_df(extracted_data)
extracted_data <- group_by(extracted_data, subject, activity)
tidy_data <- summarize_all(extracted_data, mean)

#Final results of the tidy data are stored in tidy_data
write.table(tidy_data, file = "tidy_dataset.txt", row.names = FALSE)
