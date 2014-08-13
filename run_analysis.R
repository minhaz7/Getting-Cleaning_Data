run_analysis <- function() {
  
#Author: Sayan Maity
#Date:6/18/2014
####################################
# Coursera: Data Science Track: Getting and Cleaning Data Peer Assessment
####################################
# R- Script to perform Step-1 to Step-5::
###################################
#Step 1: Merges the training and the test sets to create one data set.

#Read Files
trainData <- read.table("data/train/X_train.txt")
trainLabel <- read.table("data/train/y_train.txt")
table(trainLabel)
trainSubject <- read.table("data/train/subject_train.txt")
testData <- read.table("data/test/X_test.txt")
testLabel <- read.table("data/test/y_test.txt") 
table(testLabel) 
testSubject <- read.table("data/test/subject_test.txt")
#Merge the training and the test sets
joinData <- rbind(trainData, testData)
joinLabel <- rbind(trainLabel, testLabel)
joinSubject <- rbind(trainSubject, testSubject)

print("Reading and Merging Complete")

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement. 

#Read the features.txt file.
features <- read.table("data/features.txt")
#Subset only measurements for the mean and standard deviation.
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) 
joinData <- joinData[, meanStdIndices]

print("Extracting Mean and STD Complete")

#Formatting data
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) 
names(joinData) <- gsub("mean", "Mean", names(joinData)) 
names(joinData) <- gsub("std", "Std", names(joinData))
names(joinData) <- gsub("-", "", names(joinData)) 

print("Formatting Complete")

#Step 3:Uses descriptive activity names to name the activities in the data set.
#Read activity_labels.txt file to add descriptive names to the activities.
activity <- read.table("data/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
#Merge activity labels.
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

print("Adding Descriptive Names Complete")

# Step 4: Appropriately labels the data set with descriptive activity names.
names(joinSubject) <- "subject"
Processed_Data <- cbind(joinSubject, joinLabel, joinData)
print("Processed Data with Subject and Activity Complete")

# Store the preliminary processed dataset
write.csv(Processed_Data, "Data_after_merging.csv") 

#Step 5:Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

subjectLen <- length(table(joinSubject)) 
activityLen <- dim(activity)[1] 
columnLen <- dim(Processed_Data)[2]
tidy_dataset <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
tidy_dataset <- as.data.frame(tidy_dataset)
colnames(tidy_dataset) <- colnames(Processed_Data)

neo_Processed_Data=Processed_Data[ order(Processed_Data[,1], Processed_Data[,2]), ]
row <- 1
prev_ind<-1
for(i in 2:dim(joinData)[1]) {
  if (neo_Processed_Data[i-1,2]!=neo_Processed_Data[i,2]){
    tidy_dataset[row,2]=neo_Processed_Data[i-1,2]      
    tidy_dataset[row,1]=neo_Processed_Data[i-1,1]
    tidy_dataset[row,3:columnLen]=colMeans(as.matrix(neo_Processed_Data[prev_ind:i-1,3:columnLen]))
    prev_ind=i
    row=row+1 
  }     
}

tidy_dataset[row,2]=neo_Processed_Data[dim(joinData)[1],2]      
tidy_dataset[row,1]=neo_Processed_Data[dim(joinData)[1],1]
tidy_dataset[row,3:columnLen]=colMeans(as.matrix(neo_Processed_Data[prev_ind:dim(joinData)[1],3:columnLen]))

write.table(tidy_dataset, "Tidy_dataset_with_average_of_each_variable_for_each_activity_and_each_subject.txt",row.name=FALSE)

print("Final Tidy_dataset Writing Complete")

}
