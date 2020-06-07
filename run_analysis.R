# Getting the Data from the Web
data_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zip_file <- "UCI HAR Dataset.zip"
folder_path <- "UCI HAR Dataset"

# Downlod and unzip the file
if(!file.exists(zip_file)){
    download.file(data_url, zip_file, mode="wb")
    unzip(zip_file)
}

# Loading/Reading the Data into R

# Reading training and test data
training_subjects <- read.table(file.path(folder_path, "train", "subject_train.txt"))
training_values <- read.table(file.path(folder_path, "train", "X_train.txt"))
training_activity <- read.table(file.path(folder_path, "train", "y_train.txt"))
test_subjects <- read.table(file.path(folder_path, "test", "subject_test.txt"))
test_values <- read.table(file.path(folder_path, "test", "X_test.txt"))
test_activity <- read.table(file.path(folder_path, "test", "y_test.txt"))

# Reading Features and Activity Labels
features <- read.table(file.path(folder_path, "features.txt"), as.is = TRUE)
activities <- read.table(file.path(folder_path, "activity_labels.txt"))

## Step 1 - Merging the Datasets
training_data <- cbind(training_subjects, training_values, training_activity)
test_data <- cbind(test_subjects, test_values, test_activity)

final_data <- rbind(training_data, test_data)

# Properly naming the columns
col_names <- c("subjects", features[, 2], "activity")
names(final_data) <- col_names

## Step 2 - Extract only the columns with mean and standard deviation for each measurement
required_columns <- grepl("mean|std|subject|activity", names(final_data))
final_data <- final_data[, required_columns]

## Step 3 - Use descriptive activity names to name activities in the dataset
final_data$activity <- factor(final_data$activity, 
                                  levels = activities[, 1], 
                                  labels = activities[, 2])

## Step 4 - Label the dataset with descriptive variable names

new_colnames <- gsub("[\\(\\)-]", "", names(final_data))

new_colnames <- gsub("^f", "frequencyDomain", new_colnames)
new_colnames <- gsub("^t", "timeDomain", new_colnames)
new_colnames <- gsub("Acc", "Accelerometer", new_colnames)
new_colnames <- gsub("Gyro", "Gyroscope", new_colnames)
new_colnames <- gsub("Mag", "Magnitude", new_colnames)
new_colnames <- gsub("Freq", "Frequency", new_colnames)
new_colnames <- gsub("mean", "Mean", new_colnames)
new_colnames <- gsub("std", "StandardDeviation", new_colnames)

names(final_data) <- new_colnames # Assign the cleaned variable names

# Step 5 - Create a second tidy dataset containing means of each variable

final_data_means <- aggregate(. ~ subjects + activity, final_data, mean)

# Saving the data in tidy format
write.table(final_data_means, "tidy.txt", row.names = FALSE, quote = FALSE)
