 #Description : for light reading  Script has been  shared by 3 parts
# The first part : script treatments folders "test"
# The second part : script treatments folders "train" , merge test and train
# The third part: create tidy data, independen data.frame
#  Script  using package 'reshape 2' , please install this befor start  script.

 URL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
 download.file(URL,destfile = './Dataset.zip', method ='curl')
library(reshape2)
X_test<- read.table("UCI HAR Dataset/test/X_test.txt",header=FALSE,sep="")#read file from directory and create X_test (data.frame)
 names<- read.table("UCI HAR Dataset/features.txt",header=FALSE,sep="",stringsAsFactors = FALSE) # read "features.txt" for creating  
 colnames(X_test) <- names$V2 #  variable names for X_test.
 
 X_test_mean <- X_test[,grepl("mean",colnames(X_test))]## find data.frame by name(mean)
 X_test_std <- X_test[,grepl("std",colnames(X_test))]#### find data.frame by name( std)
 X_test_mean_std  <-  cbind(X_test_mean, X_test_std )## merge 
 
 subject_test<- read.table("UCI HAR Dataset/test/subject_test.txt",header=FALSE,sep="")# read file subject_test and create subject_test(data.frame)
 y_test<- read.table("UCI HAR Dataset/test/y_test.txt",header=FALSE,sep="")## read file y_test and create y_test(data.frame)
 
  df <- cbind(subject_test,y_test ) # merge above  added data.frames together
  colnames(df) <- c("Subject","Activity")# gives names for variables 
  test_data <- cbind(df,X_test_mean_std)# merge a new data.frame  with earlier data.frame(X_test...)
 
   ## The same operation make for  a train folder
 
 X_train<- read.table("train/X_train.txt",header=FALSE,sep="")
   
  colnames(X_train) <- names$V2
 subject_train<- read.table("UCI HAR Dataset/train/subject_train.txt",header=FALSE,sep="")
 y_train<- read.table("UCI HAR Dataset/train/y_train.txt",header=FALSE,sep="")
 X_train_mean <- X_train[,grepl("mean",colnames(X_train))]
 X_train_std <- X_train[,grepl("std",colnames(X_train))]
 X_train_mean_std  <-  cbind(X_train_mean, X_train_std )
 
 df_train <- cbind(subject_train,y_train )
 colnames(df_train) <- c("Subject","Activity")
 train_data <- cbind(df_train,X_train_mean_std)
 train_test <- rbind(test_data,train_data)
# Third part
 clean_name <- gsub("[:():]","",colnames(train_test))## clean  the variables names  of bracket.
 colnames(train_test) <- clean_name ## assignment cleaned  names to column
 
 
 
 train_test$Activity <- as.character(train_test$Activity) ##  converted to character
 train_test$Activity[train_test$Activity == "1"]  <- "WALKING" ## assigment each number corresponding the nmaes of activity
 train_test$Activity[train_test$Activity == "2"]  <- "WALKING_UPSTAIRS"
 train_test$Activity[train_test$Activity == "3"]  <- "WALKING_DOWNSTAIRS"
 train_test$Activity[train_test$Activity == "4"]  <- "SITTING"
 train_test$Activity[train_test$Activity == "5"]  <- "STANDING"
 train_test$Activity[train_test$Activity == "6"]  <- "LAYING"
 
 by_Activ_Sub <- melt(train_test,id = c("Subject","Activity"),na.rm = TRUE) ## get molted d.f. 
 step <-  dcast(by_Activ_Sub, Subject+Activity ~ variable, mean) ## find avarege  value for each meaning 
 
