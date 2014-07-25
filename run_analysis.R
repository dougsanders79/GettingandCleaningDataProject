setwd("~/Coursera/GettingandCleaningData/project")
source("run_analysis_functions.R")
#source("run_analysis.R")

Name_existing_file <- "pc_data.zip"
destination <- "pc_data.zip"

# The logic in the if statement below saves from having the download the large data file each time the script 
# is loaded into R.

if (file.exists(Name_existing_file)){

} else {
  urlname <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  destination <- "pc_data.zip"
  download.file(url = urlname, destfile = destination, method = "curl")
}

file_info <- unzip(destination, list=TRUE)
unzip(destination)

# "file_info" is a data.frame that contains the paths/names to all of the downloaded files in unzipped form
# elements (character strings) are called directly using the read.delim function to import the data into R

file_info

DF_activity_labels <- read.delim(file_info[1,1], sep = "", skipNul = TRUE, header = FALSE)
DF_features <- read.delim(file_info[2,1], sep = "", skipNul = TRUE, header = FALSE)

subject_test <- read.delim(file_info[16,1], sep = "", skipNul = TRUE, header = FALSE)
DF_X_test <- read.delim(file_info[17,1], sep = "", skipNul = TRUE, header = FALSE)
DF_Y_test <- read.delim(file_info[18,1], sep = "", skipNul = TRUE, header = FALSE)


subject_train <- read.delim(file_info[30,1], sep = "", skipNul = TRUE, header = FALSE)
DF_X_train <- read.delim(file_info[31,1], sep = "", skipNul = TRUE, header = FALSE)
DF_Y_train <- read.delim(file_info[32,1], sep = "", skipNul = TRUE, header = FALSE)

# DF_features are the different measurements taken (561) these are set as the column names to the first Data frame 
# assembled. This is done for both the test and training data sets.

colnames(DF_X_test) <- DF_features[,2]
colnames(DF_X_train) <- DF_features[,2]

# The index_replace function uses the ativity labels index or key file (6x2) to replace the integer values
# (1-6) with the corresponding activity (WALKING, WALKING_UPSTAIRS...)


# index_replace function is used to create the Activity character vector for the test data set, then the 
# Training data set (it is simply overwritten for the second useage).  Using cbind the subject_ID and Activity 
# Vectors are added to the data.frames DF_X_test and DF_X_train.

Activity <- index_replace(as.vector(DF_Y_test[,1]), as.vector(DF_activity_labels[,2]) )
subject_ID <- as.vector(subject_test[,1])
DATA_TEST <- cbind(subject_ID, Activity,  DF_X_test)

Activity <- index_replace(as.vector(DF_Y_train[,1]), as.vector(DF_activity_labels[,2]) )
subject_ID <- as.vector(subject_train[,1])
DATA_TRAIN <- cbind(subject_ID, Activity,  DF_X_train)

namescols <- names(DATA_TRAIN)

# here some manipulations are done using grep to get a character vector of only the quantities needed per:
# "2. Extracts only the measurements on the mean and standard deviation for each measurement."
# 

namescols_std <- namescols[grep("std()", namescols)]
namescols_std

namescols_mean1 <- namescols[grep("mean()", namescols)]
namescols_mean <- namescols_mean1[-grep("Freq", namescols_mean1)]
namescols_mean

# Using the character vector obtained above, cbind is used to assemble a smaller data frame (less columns) of 
# the desired data for both the test and training data sets, in the last step the data sets are merged using 
# rbind.

DTrain <- cbind(DATA_TRAIN[c(1,2)],DATA_TRAIN[namescols_std],DATA_TRAIN[namescols_mean]) 
DTest <- cbind(DATA_TEST[c(1,2)],DATA_TEST[namescols_std],DATA_TEST[namescols_mean]) 
DMERGE <- rbind(DTrain,DTest)

colnames_tidy <- names(DMERGE)

strings_for_match <- c("tBody", "fBody", "tGravity", "Acc", "Gyro", "Jerk" ,"Mag", "std", "mean", "X", "Y", "Z")
DF_strings <- c("tB", "fB","tG", "A", "G", "J", "MG", "SD", "ME", "X", "Y", "Z")
DF_strings <- c("TimeDomainBody", "FrequencyDomainBody","TimeDomainGravity", "Accelleration", "Gyrometer", "Jerk", "Magnitude", "StandardDeviation", "Mean", "Xdir", "Ydir", "Zdir")

ML <- make_labels(colnames_tidy, strings_for_match, DF_strings) 

# here the DMERGE data frame is reordered row wise according to subject id and activity. An additional
# column is created where a character string of the subject id is pasted to the activity for use in step 5.

Dorder <- DMERGE[order(DMERGE$subject_ID, DMERGE$Activity),]
Dorder$subject_activity <- as.factor(paste(Dorder[,1],Dorder[,2]))

# FACTOR1 obtaines below contains 180 "levels", 30 subjects x 6 activities.
FACTOR1 <- unique(as.factor(paste(Dorder[,1],Dorder[,2])))

# the only way I could figure out how to calculate the mean was to it using the for loop founction below.
#  (I am sure there is a better way.. but I don't have the time right now)  considerable cpu time savings 
# was achieved by using the colMeans function such that the i index loops over the 180 levels of 
# subject_activity (so colMeans function is called 180 times) and the j index simply populates the DF data 
# frame with the appropriate vale from "temp".  Using the mean function 180x66 times was taking forever.

# "DF_Tidy is the second independent tidy data set created for step 5 and for submission.
DFtidy <- loop_over(Dorder, FACTOR1, cols = 3:68)

# here we "appropriately label the data set with descriptive variable (column) names"
names(DFtidy)[3:68] <- ML[3:68]

write.table(DFtidy, file= "tidy_data.txt")

#  The series of commands below were used to check to make sure that the loop function worked in
# the intended way.

DFtidy[8,4]
mean(Dorder[Dorder$subject_activity == FACTOR1[8] ,4])


dimnames(DFtidy)[[1]][8]
dimnames(DFtidy)[[2]][4]

FACTOR1[8]
dimnames(Dorder)[[2]][4]



