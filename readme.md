# Getting and Cleaning Data Project

## run_analysis.R

The cleanup script (run_analysis.R) does the following:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names. 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

## Running the script

To run the script, source `run_analysis.R`. After running, you will see the following output as the script works:

[GettingData RunAnalysis.R] Getting and Cleaning Data Project 
[GettingData RunAnalysis.R] Starting up. 
[GettingData RunAnalysis.R] Preparing to run analysis. 
[GettingData RunAnalysis.R] Reading datasets. 
[GettingData RunAnalysis.R] Feature  X_test.txt 
[GettingData RunAnalysis.R] activities  Y_test.txt 
[GettingData RunAnalysis.R] subject  subject_test.txt 
[GettingData RunAnalysis.R] Get data: ./data/test 
[GettingData RunAnalysis.R]  Read  Features  
[GettingData RunAnalysis.R]   Read activities  
[GettingData RunAnalysis.R]  Read  subjects  
[GettingData RunAnalysis.R] Feature  X_train.txt 
[GettingData RunAnalysis.R] activities  Y_train.txt 
[GettingData RunAnalysis.R] subject  subject_train.txt 
[GettingData RunAnalysis.R] Get data: ./data/train 
[GettingData RunAnalysis.R]  Read  Features  
[GettingData RunAnalysis.R]   Read activities  
[GettingData RunAnalysis.R]  Read  subjects  
[GettingData RunAnalysis.R] Join test and train data. 
[GettingData RunAnalysis.R] Melting data. 
[GettingData RunAnalysis.R] Dcasting. the data 
[GettingData RunAnalysis.R] Saving cleaned data : ./data/combined.txt 
 

## Process

1. For both the test and train datasets, produce an interim dataset:
    1. Extract the mean and standard deviation features (listed in CodeBook.md, section 'Extracted Features'). This is the `values` table.
    2. Get the list of activities.
    3. Put the activity *labels* (not numbers) into the `values` table.
    4. Get the list of subjects.
    5. Put the subject IDs into the `values` table.
2. Join the test and train interim datasets.
3. Put each variable on its own row.
4. Rejoin the entire table, keying on subject/acitivity pairs, applying the mean function to each vector of values in each subject/activity pair. This is the clean dataset.
5. Write the clean dataset to disk.

## Cleaned Data

The resulting clean dataset is in this repository at: `data/cleaned.txt`. It contains one row for each subject/activity pair and columns for subject, activity, and each feature that was a mean or standard deviation from the original dataset.

## Notes

X_* - feature values (one row of 561 features for a single activity)
Y_* - activity identifiers (for each row in X_*)
subject_* - subject identifiers for rows in X_*
