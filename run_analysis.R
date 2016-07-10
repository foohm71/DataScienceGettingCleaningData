library(dplyr)

# reads a space separated file and returns a dataframe
readData <- function(filename) {
  df <- read.csv(filename, header = FALSE, sep = "")
}

# reads a space separated file and returns a dataframe
readLabels <- function(filename) {
  df <- read.csv(filename, header = FALSE, sep = "")
}

# extracts out the mean and std dev based on the header label
extractMeanStdDev <- function(df) {
  df1 <- df[, grep("-mean\\(|-std\\(", names(df))]
}

# replace code with labels
replaceWithLabel <- function(df, labelarray) {
  df1 <- sapply(df, function(x) {labelarray[x,2]})
}


# Main script
# This assumes you have unzipped the zip file in the current dir

# This part reads all the files
activityLabels <- readLabels("UCI HAR Dataset/activity_labels.txt")
featureLabels <- readLabels("UCI HAR Dataset/features.txt")

testData <- readData("UCI HAR Dataset/test/X_test.txt")
testActivity <- readData("UCI HAR Dataset/test/y_test.txt")
testSubject <- readData("UCI HAR Dataset/test/subject_test.txt")

trainData <- readData("UCI HAR Dataset/train/X_train.txt")
trainActivity <- readData("UCI HAR Dataset/train/y_train.txt")
trainSubject <- readData("UCI HAR Dataset/train/subject_train.txt")

# This part puts the headers onto testData and trainData and extracts out only the cols that are mean and std
names(testData) <- featureLabels[,2]
names(trainData) <- featureLabels[,2]
testMeanStd <- extractMeanStdDev(testData)
trainMeanStd <- extractMeanStdDev(trainData)

# Remove testData, trainData from GlobalEnv for performance
rm(testData, envir = globalenv())
rm(trainData, envir = globalenv())

# Replace the testActivity and trainActivity codes with the corresponding activity labels
testActivityLables <- replaceWithLabel(testActivity, activityLabels)
trainActivityLables <- replaceWithLabel(trainActivity, activityLabels)

# Add columns "Activity" and "Subject" to testMeanStd and trainMeanStd
testMeanStd["Activity"] <- testActivityLables[,1]
trainMeanStd["Activity"] <- trainActivityLables[,1]
testMeanStd["Subject"] <- testSubject[,1]
trainMeanStd["Subject"] <- trainSubject[,1]

# Create 1 big table combining test and train dataframes 
# (part 1 of question)
combinedMeanStd <- rbind(testMeanStd, trainMeanStd)

# clean env for performance
rm(testMeanStd, envir = globalenv())
rm(trainMeanStd, envir = globalenv())

# This gets you the new tidy data frame with the mean for each data point for Subject, Activity combination
# (part 2 of question)
newDF <- combinedMeanStd %>% group_by(Subject, add = TRUE) %>% group_by(Activity, add = TRUE) %>% summarise_each(funs(mean))

# write to file
write.table(newDF, file = "./newDF.txt", row.names = FALSE)