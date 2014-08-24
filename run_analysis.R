library(reshape2)

clean.file <- "combined.txt"
extract.features <- c(1, 2, 3, 4, 5, 6, 41, 42, 43, 44, 45, 46, 81, 82, 83, 84, 85, 86, 121, 122, 123, 124, 125, 126, 161, 162, 163, 164, 165, 166, 201, 202, 214, 215, 227, 228, 240, 241, 253, 254, 266, 267, 268, 269, 270, 271, 345, 346, 347, 348, 349, 350, 424, 425, 426, 427, 428, 429, 503, 504, 516, 517, 529, 530, 542, 543)
extract.feature.names <- c("tBodyAcc-mean()-X", "tBodyAcc-mean()-Y", "tBodyAcc-mean()-Z", "tBodyAcc-std()-X", "tBodyAcc-std()-Y", "tBodyAcc-std()-Z", "tGravityAcc-mean()-X", "tGravityAcc-mean()-Y", "tGravityAcc-mean()-Z", "tGravityAcc-std()-X", "tGravityAcc-std()-Y", "tGravityAcc-std()-Z", "tBodyAccJerk-mean()-X", "tBodyAccJerk-mean()-Y", "tBodyAccJerk-mean()-Z", "tBodyAccJerk-std()-X", "tBodyAccJerk-std()-Y", "tBodyAccJerk-std()-Z", "tBodyGyro-mean()-X", "tBodyGyro-mean()-Y", "tBodyGyro-mean()-Z", "tBodyGyro-std()-X", "tBodyGyro-std()-Y", "tBodyGyro-std()-Z", "tBodyGyroJerk-mean()-X", "tBodyGyroJerk-mean()-Y", "tBodyGyroJerk-mean()-Z", "tBodyGyroJerk-std()-X", "tBodyGyroJerk-std()-Y", "tBodyGyroJerk-std()-Z", "tBodyAccMag-mean()", "tBodyAccMag-std()", "tGravityAccMag-mean()", "tGravityAccMag-std()", "tBodyAccJerkMag-mean()", "tBodyAccJerkMag-std()", "tBodyGyroMag-mean()", "tBodyGyroMag-std()", "tBodyGyroJerkMag-mean()", "tBodyGyroJerkMag-std()", "fBodyAcc-mean()-X", "fBodyAcc-mean()-Y", "fBodyAcc-mean()-Z", "fBodyAcc-std()-X", "fBodyAcc-std()-Y", "fBodyAcc-std()-Z", "fBodyAccJerk-mean()-X", "fBodyAccJerk-mean()-Y", "fBodyAccJerk-mean()-Z", "fBodyAccJerk-std()-X", "fBodyAccJerk-std()-Y", "fBodyAccJerk-std()-Z", "fBodyGyro-mean()-X", "fBodyGyro-mean()-Y", "fBodyGyro-mean()-Z", "fBodyGyro-std()-X", "fBodyGyro-std()-Y", "fBodyGyro-std()-Z", "fBodyAccMag-mean()", "fBodyAccMag-std()", "fBodyBodyAccJerkMag-mean()", "fBodyBodyAccJerkMag-std()", "fBodyBodyGyroMag-mean()", "fBodyBodyGyroMag-std()", "fBodyBodyGyroJerkMag-mean()", "fBodyBodyGyroJerkMag-std()")
activities <- c(1, 2, 3, 4, 5, 6)
activity.names <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

# A helper method for printing to the console.
print <- function(...) {
        cat("[GettingData RunAnalysis.R]", ..., "\n")
}


# Returns an interim dataframe for a single dataset.
get.data <- function(dir, name) {
        # Setup the file paths.
        features.filename <-  paste("X_", name, ".txt", sep = "")
        activities.filename <- paste("Y_", name, ".txt", sep = "")
        subject.filename <-  paste("subject_", name, ".txt", sep = "") 
        
        print("Feature ", features.filename)
        print("activities ", activities.filename )
        print("subject ", subject.filename)
        
        data.dir <- file.path(dir, name)
        features.name <- file.path(data.dir, features.filename)
        activities.name <- file.path(data.dir, activities.filename)
        subjects.name <- file.path(data.dir, subject.filename)
        
        print("Get data:", data.dir)
        
        # Read the features table.
        print(" Read  Features ")
        features.t <- read.table(features.name)[extract.features]
        names(features.t) <- extract.feature.names
        
        clean.data <- features.t
        
        # Read the activities list.
        print("  Read activities ")
        activities.t <- read.table(activities.name)
        names(activities.t) <- c("activity")
        activities.t$activity <- factor(activities.t$activity, levels = activities, labels = activity.names)
        clean.data <- cbind(clean.data, activity = activities.t$activity)
        
        # Read the subjects list.
        print(" Read  subjects ")
        subjects.t <- read.table(subjects.name)
        names(subjects.t) <- c("subject")
        clean.data <- cbind(clean.data, subject = subjects.t$subject)
        
        # Return the clean data
        clean.data
} 

# Performs the full analysis of both the test and train
# datasets. Writes a clean dataset to disk.
run.analysis <- function(dir) {
        print("Getting and Cleaning Data Project")
        print("Starting up.")
        print("Preparing to run analysis.")
        
        # Read the data.
        print("Reading datasets.")
        test <- get.data(dir, "test")
        train <- get.data(dir, "train")
        
        # Join the data.
        print("Join test and train data.")
        all.data <- rbind(test, train)
        
        # Reshape the data.
        print("Melting data.")
        all.data.long <- melt(all.data, id = c("subject", "activity"))
        print ("Dcasting. the data")
        all.data.wide <- dcast(all.data.long, subject + activity ~ variable, mean)
        
        # Set the clean data.
        all.data.clean <- all.data.wide
        
        # Save the clean data.
        clean.file.name <- file.path(dir, clean.file)
        print ("Saving cleaned data :", clean.file.name)
        write.table(all.data.clean, clean.file.name, row.names = FALSE, quote = FALSE)
}

# Run the analysis.
run.analysis("./data")