Coursera_Getting_and_Cleaning_Data_Peer_Assesment
=================================================

Getting and Cleaning Data Course Project.
Author: Minhazul Islam Sk
Date:8/13/2014

# Problem Description:

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

You should create one R script called run_analysis.R that does the following.

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set.
4. Appropriately labels the data set with descriptive activity names.
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Good luck!

# Steps to run this project:

1. Download the data from :
    https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
2. Unzip the data into a directory into "data".
4. Set the R working Directory to the directory which has the data folder (Parent directory of the data folder). Keep the run_analysis.R script in the 
   parent directory (not inside the data folder, but inside the parent directory). Then Open RStudio and run the command --> source("run_analysis.R") 
   in RStudio. Then run the command run_analysis()
5. The script will generate two files:
   I: "Data_after_merging.csv": Contains data frame named Processed_Data.
   II: "Tidy_dataset_with_average_of_each_variable_for_each_activity_and_each_subject.txt": Contains data frame named tidy_dataset.   
6. To reuse the tidy dataset use Finally, use 
   data <-read.table("Tidy_dataset_with_average_of_each_variable_for_each_activity_and_each_subject.txt",header=T) command in RStudio to read the file.  
7. We are supposed to get the average of each variable for each activity and each subject, and there are 6 activities in total and 30 subjects
   in total, we have 180 rows with all combinations for each of the 66 features.
