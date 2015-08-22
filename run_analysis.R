run_analysis <- function() {
  # Merges the training and the test sets to create one data set.
  x_test_table = read.table("~/Documents/DataScience/UCI HAR Dataset/test/X_test.txt", header = FALSE)
  y_test_table = read.table("~/Documents/DataScience/UCI HAR Dataset/test/Y_test.txt", header = FALSE)
  sub_test_table = read.table("~/Documents/DataScience/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
  
  test_table = data.frame(x_test_table,y = y_test_table[["V1"]], sub = sub_test_table[["V1"]])
  
  x_train_table = read.table("~/Documents/DataScience/UCI HAR Dataset/train/X_train.txt", header = FALSE)
  y_train_table = read.table("~/Documents/DataScience/UCI HAR Dataset/train/Y_train.txt", header = FALSE)
  sub_train_table = read.table("~/Documents/DataScience/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
  
  train_table = data.frame(x_train_table,y = y_train_table[["V1"]], sub = sub_train_table[["V1"]])
  
  merger_table = merge(test_table, train_table, all = T)
  
  # Extracts only the measurements on the mean and standard deviation for each measurement. 
  
  ## Based on interpreting column names in the features.txt, we can infer the col number 
  ## which the mean and std been.i.e: include mean() and std() at the end, or include 
  ## entries with mean in an earlier part of the name
  ## the cols are as follows: 
  ##
  ## 1 2 3 4 5 6 41 42 43 44 45 46 81 82 83 84 85 86 121 122 123 124 125 126 161 162 163 164 165 166
  ## 201 202 214 215 227 228 240 241 253 254 266 267 268 269 270 271 345 346 347 348 349 350 424 425 
  ## 426 427 428 429 503 504 516 517 529 530 542 543 555 556 557 558 559 560 561 562 563

  mean_std_cols = c(1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,86,121,122,123,124,125,126,161,162,
                    163,164,165,166,201,202,214,215,227,228,240,241,253,254,266,267,268,269,270,271,
                    345,346,347,348,349,350,424,425,426,427,428,429,503,504,516,517,529,530,542,543,
                    555,556,557,558,559,560,561,562,563)
  mean_std_data = merger_table[,mean_std_cols]
  toNum <- function(x) {
    mode(x) = "numeric"
  }
  apply(mean_std_data, 2, toNum)
  
  # Uses descriptive activity names to name the activities in the data set
  ## the col 562("y") is the link of acvitiy labels
  mean_std_data[mean_std_data$y == 1, "y"] <- "WALKING"
  mean_std_data[mean_std_data$y == 2, "y"] <- "WALKING_UPSTAIRS"
  mean_std_data[mean_std_data$y == 3, "y"] <- "WALKING_DOWNSTAIRS"
  mean_std_data[mean_std_data$y == 4, "y"] <- "SITTING"
  mean_std_data[mean_std_data$y == 5, "y"] <- "STANDING"
  mean_std_data[mean_std_data$y == 6, "y"] <- "LAYING"
  
  # Appropriately labels the data set with descriptive variable names. 
  mean_std_names = c("tBodyAccMean-X","tBodyAccMean-Y","tBodyAccMean-Z","tBodyAccStd-X","tBodyAccStd-Y",
                     "tBodyAccStd-Z","tGravityAccMean-X","tGravityAccMean-Y","tGravityAccMean-Z",
                     "tGravityAccStd-X","tGravityAccStd-Y","tGravityAccStd-Z","tBodyAccJerkMean-X",
                     "tBodyAccJerkMean-Y","tBodyAccJerkMean-Z","tBodyAccJerkStd-X","tBodyAccJerkStd-Y",
                     "tBodyAccJerkStd-Z","tBodyGyroMean-X","tBodyGyroMean-Y","tBodyGyroMean-Z",
                     "tBodyGyroStd-X","tBodyGyroStd-Y","tBodyGyroStd-Z","tBodyGyroJerkMean-X",
                     "tBodyGyroJerkMean-Y","tBodyGyroJerkMean-Z","tBodyGyroJerkStd-X","tBodyGyroJerkStd-Y",
                     "tBodyGyroJerkStd-Z","tBodyAccMagMean","tBodyAccMagStd","tGravityAccMagMean",
                     "tGravityAccMagStd","tBodyAccJerkMagMean","tBodyAccJerkMagStd","tBodyGyroMagMean",
                     "tBodyGyroMagStd","tBodyGyroJerkMagMean","tBodyGyroJerkMagStd","fBodyAccMean-X",
                     "fBodyAccMean-Y","fBodyAccMean-Z","fBodyAccStd-X","fBodyAccStd-Y","fBodyAccStd-Z",
                     "fBodyAccJerkMean-X","fBodyAccJerkMean-Y","fBodyAccJerkMean-Z","fBodyAccJerkStd-X",
                     "fBodyAccJerkStd-Y","fBodyAccJerkStd-Z","fBodyGyroMean-X","fBodyGyroMean-Y","fBodyGyroMean-Z",
                     "fBodyGyroStd-X","fBodyGyroStd-Y","fBodyGyroStd-Z","fBodyAccMagMean","fBodyAccMagStd",
                     "fBodyBodyAccJerkMagMean","fBodyBodyAccJerkMagStd","fBodyBodyGyroMagMean","fBodyBodyGyroMagStd",
                     "fBodyBodyGyroJerkMagMean","fBodyBodyGyroJerkMagStd","angle(tBodyAccMean,gravity)",
                     "angle(tBodyAccJerkMean),gravityMean)","angle(tBodyGyroMean,gravityMean)",
                     "angle(tBodyGyroJerkMean,gravityMean)","angle(X,gravityMean)","angle(Y,gravityMean)",
                     "angle(Z,gravityMean)", "activity", "subject")
  colnames(mean_std_data) <- mean_std_names
  
  # From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
  # for each activity and each subject.
  activity_average = tapply(mean_std_data[["tBodyAccMean-X"]], mean_std_data$activity, mean)
  subject_average = tapply(mean_std_data[["tBodyAccMean-X"]], mean_std_data$subject, mean)
  mean_std_names1 = mean_std_names[!mean_std_names %in% c("tBodyAccMean-X","activity","subject")]
  mean_std_names2 = mean_std_names[!mean_std_names %in% c("activity","subject")]
  ## the average of each variable for each activity
  for (name in mean_std_names1) {
    act_data = tapply(mean_std_data[[name]], mean_std_data$activity, mean)
    activity_average = data.frame(activity_average,temp_data)
  }
  names(activity_average) <- mean_std_names2
  
  ## the average of each variable for each subject
  for (s_name in mean_std_names1) {
    data = tapply(mean_std_data[[s_name]], mean_std_data$subject, mean)
    subject_average = data.frame(subject_average, data)
  }
  names(subject_average) <- mean_std_names2
  
  ## merge the activity average and subject average, and rename the row names
  all_average = merge(activity_average, subject_average, all = T)
  row.names(all_average) <- c(row.names(activity_average), row.names(subject_average))
  
  ## created result file with write.table() using row.name=FALSE 
  write.table(all_average, "analysis_result.txt", row.names = FALSE)
}