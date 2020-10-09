#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement.
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names.
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


require(data.table)
require(magrittr)
require(stringr)
require(dplyr)
require(plyr)

#Step 0: loops through train/test directories and loads all necessary files and gets variable names and activity labels. (Ignores Inertial folder as it is unececessary for the assignment)

datasets <-
  '/home/pedro/R-Resources/Getting and Cleaning Data/Course Project/UCI HAR Dataset/'
sets <- c('test', 'train')

for (set in sets) {
  folder <- paste0(datasets, set, sep = "")
  files <- dir(folder)[grep("txt", dir(folder))]
  
  for (file in files) {
    path <- paste0(folder, "/", file, sep = "")
    file <- str_remove_all(file, ".txt")
    assign(file, fread(path))
  }
}

features <-
  fread(
    '/home/pedro/R-Resources/Getting and Cleaning Data/Course Project/UCI HAR Dataset/features.txt'
  )
activity_labels <-
  fread(
    '/home/pedro/R-Resources/Getting and Cleaning Data/Course Project/UCI HAR Dataset/activity_labels.txt'
  )


#Step 1,3 & 4: MERGING and LABELING <<< make sure that the order of the merge is the same for all sets (test, train)

featureSET <- rbindlist(list(X_test, X_train))
colnames(featureSET) <- features[[2]]

activitySET <- rbindlist(list(y_test, y_train))
activitySET[['V1']] <-
  mapvalues(activitySET[['V1']], activity_labels[['V1']], activity_labels[['V2']])
colnames(activitySET) <- 'activity'

subjectsSET <- rbindlist(list(subject_test, subject_train))
colnames(subjectsSET) <- 'subject'

fullset <-
  cbind(subjectsSET, activitySET, featureSET) # this set is already properly labeled


#Step 2: Selecting just the columns with mean() and std() in them, the result here is the ITEM 4 requirement of the assignment

result <-
  fullset[, .SD, .SDcols = names(fullset) %like% "(subject|activity|mean\\(|std\\()"][, c('subject','activity') := lapply(.SD, factor), .SDcols = c('subject','activity')] #the last chained call transforms subject and activity columns into factor type



#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidyset <-
  aggregate(. ~ subject + activity, result, mean)    #-> symbolic formula syntax
tidyset <-
  tidyset[order(tidyset$subject, tidyset$activity), ]


#write output
write.table(tidyset, paste0(datasets,"tidyset.txt",sep=""), row.name = FALSE)
