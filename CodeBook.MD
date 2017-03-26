 Getting and Cleaning Data Course Project
Oliver González Marzo 2017

### Overview
This file contains aditional info useful to understand the source, coding and output for the Getting and Cleaning Data Course Project 

## What is in the text files
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 
The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

## Attribute Information:
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment. 

## How is the solution achieved
## Part 1 Merges the training and the test sets to create one data set, asigning the column names 
  - reading the test files
      - X_test.txt
      - y_test.txt
      - subject_test.txt
  - reading the train files
    - X_train.txt
    - y_train.txt
    - subject_train.txt
  - reading features and activity  
    - features.txt
    - activity_labels.txt
      
## Part 2 Extract only the measurements on the mean and standard deviation for each measurement. 
Get column names for activity, Id, subject, mean, std, then create a subset "finalset_mean_std" that contains TRUE for mean and stdev columns and FALSE values for the other.

## Part 3. Use descriptive activity names to name the activities in the data set
Merge data subset "finalset_mean_std" with the "activity_labels" table to inlude the descriptive activity names.

## Part 4. Appropriately label the data set with descriptive activity names.
This was done using aplying gsub with the proper names

## Part 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
The final instructions were to create a second data set with the average of each veriable for each activity and subject      
 
### Source (if you need the original files)
A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
