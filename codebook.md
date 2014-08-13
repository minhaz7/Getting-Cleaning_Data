#Author: Minhazul Islam Sk
#Date:8/13/2014

# Coursera: Data Science Track: Getting and Cleaning Data Peer Assessment

CodeBook: Getting and Cleaning Data Course Project 
========================================================
A short description of the variables, the data, and any transformations or work that been performed to clean up the data.
# Data::

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
The data for the project can be found:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


# Analysis Performed using run_analysis.R script:

1. The analysis addresses the training and test data separately, first combining the feature vector with outcome variables and subject identifiers (e.g. X_train.txt, Y_train.txt, subject_train.txt), and then merges the training and test data into a single, larger data frame, joinData.

2. Read the features.txt file from the "/DATA" folder and store the data in a variable called features. We only extract the measurements on the mean and standard deviation. 

3. Read the activity_labels.txt file from the "./DATA"" folder and store the data in a variable called activity.

4. Transform the values of joinLabel according to the activity data frame.

5. Combine the joinSubject, joinLabel and joinData by column to get a new processed data frame, Processed_Data. The first two columns named "subject" and "activity". The "subject" column contains integers that range from 1 to 30 inclusive; the "activity" column contains 6 kinds of activity names; the last 66 columns contain measurements that range from -1 to 1 exclusive.

6. Generate a second independent tidy data set with the average of each measurement for each activity and each subject. We have 30 unique subjects and 6 unique activities, which result in a 180 combinations of the two. Then, for each combination, we calculate the mean of each measurement with the corresponding combination. So, after initializing the result data frame and performing the two for-loops, we get a 180x68 data frame.

7. Save the tidy data set as "Tidy_dataset_with_average_of_each_variable_for_each_activity_and_each_subject.txt" file.

