# Pat Molloy 16.Jan.2015
# Coursera Getting and Tidying Data Course Project
# run_analysis.R


require(plyr)

# set up main path - our working directory needs to contain a folder UCI_HAR_Dataset
# with the original folder structure contained in the downloaded zip archive

path <- "UCI_HAR_Dataset/"

# little function to stick the path and filename together

stick <- function(filename) {
  sprintf("%s%s", path, filename)
}

# create the file links 

x_train_file <- stick("train/X_train.txt")
x_test_file  <- stick("test/X_test.txt")

y_train_file <- stick("train/y_train.txt")
y_test_file  <- stick("test/y_test.txt")

subject_train_file <- stick("train/subject_train.txt")
subject_test_file <- stick("test/subject_test.txt")

features_file <- stick("features.txt")
activity_labels_file <- stick("activity_labels.txt")

# Load the data

message("... loading data")

x_train <- read.table(x_train_file)
x_test <- read.table(x_test_file)

y_train <- read.table(y_train_file)
y_test <- read.table(y_test_file)

subject_train <- read.table(subject_train_file)
subject_test <- read.table(subject_test_file)

features <- read.table(features_file, colClasses = c("character"))
activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))


# Merge the data

message ("... merging data")

training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)

# Combine all the training and test data into a single dataframe 

sensor_data <- rbind(training_sensor_data, test_sensor_data)

# Label the columns and put Subject and ActivityId at end - 562 and 563

sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels

# limit to cols that contain mean(), std() ; include Subject and ActivityID

pattern <- "mean()|std()|Subject|ActivityId"

message ("... selecting only required columns")

new_sensor_data <- sensor_data[,grep(pattern,  names(sensor_data), ignore.case=T)]

# pick up the Activity descriptive text from activity_labels, and use that. Lose ActivityID

new_sensor_data <- join(new_sensor_data, activity_labels, by = "ActivityId", match = "first")
new_sensor_data <- new_sensor_data[,-1] # get rid of ActivityId column

# Tidy up the variable/column names

message ("... tidying variable names")

names(new_sensor_data) <- gsub('\\(|\\)',"",names(new_sensor_data)) # get rid of the original parens in the names
names(new_sensor_data) <- gsub(",","-", names(new_sensor_data)) # change any commas to a -

names(new_sensor_data) <- tolower(names(new_sensor_data)) # everything to lower case as per recommendations

# Ceate the finaltidy data set with the mean of each variable by activity and subject

message ("... creating output in sensor_averages.txt")
sensor_avg <- ddply(new_sensor_data, c("subject","activity"), numcolwise(mean))
write.table(sensor_avg, file = "sensor_averages.txt", row.names=F)

## END