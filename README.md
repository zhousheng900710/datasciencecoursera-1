# code script introduction
the script is coded as follows' steps：
* Merges the training and the test sets to create one data set.
* Extracts only the measurements on the mean and standard deviation for each measurement. 
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive variable names. 
* From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

* the "activity label" is in the file of "activity_label.txt" and linked the y_*.txt file.
* we can find the mean and std columns based on the "feature.txt".

there are details in the code, thanks.

# variable in result dataset(.txt file) introduction

* the column name of the set is the mean and std measure variables of each activity and subject
* the first six rows'name is "LAYING SITTING STANDING WALKING WALKING_DOWNSTAIRS WALKING_UPSTAIRS"
  and the rest of rows is the subject, which from "1" to "30"
