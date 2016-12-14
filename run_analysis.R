library(dplyr)
library(lubridate)
library(data.table)
library(plyr)
library(Hmisc)
library(tidyr)
library(readr)
library(stringi)

#################################################################################################################
## Manipulate and read input files
#################################################################################################################

# training input
setwd("C:/Users/leedo002/Documents/Education/Data Science by JHU/Course 3 - Getting and Cleaning Data/FinalAssignment/UCI HAR Dataset/train")

## Normalize delimiter before reading into data frame
  con<-file("X_train.txt")                    # open connection to file
  training_x <- readLines(con)                # read in the file to a character vector
  training_x2 <- sub("^ *","", training_x)    # remove leading blanks
  training_x3 <- gsub("  "," ", training_x2)  # change two contiguous spaces to a single space
  write_lines(training_x3,"training_x3.txt")  # write out to a new file
  close(con)                                  # close connection                               

  training <- read.delim("training_x3.txt",header=FALSE,sep=" ")      # read in modified input file 
  training_labels <- read.delim("y_train.txt",header=FALSE,sep=" ")   # read in corresponding labels

# test input  
setwd("C:/Users/leedo002/Documents/Education/Data Science by JHU/Course 3 - Getting and Cleaning Data/FinalAssignment/UCI HAR Dataset/test")
## Normalize delimiter before reading into data frame
  con<-file("x_test.txt")             # open connection to file
  test_x <- readLines(con)            # read in the file to a character vector
  test_x2 <- sub("^ *","", test_x)    # remove leading blanks
  test_x3 <- gsub("  "," ", test_x2)  # change two contiguous spaces to a single space
  write_lines(test_x3,"test_x3.txt")  # write out to a new file
  close(con)                          # close connection

  test <- read.delim("test_x3.txt",header=FALSE,sep=" ")        # read in modified input file 
  test_labels <- read.delim("y_test.txt",header=FALSE,sep=" ")  # read in corresponding labels

# activity input  
setwd("C:/Users/leedo002/Documents/Education/Data Science by JHU/Course 3 - Getting and Cleaning Data/FinalAssignment/UCI HAR Dataset")
  activity_labels <- read.delim("activity_labels.txt",header=FALSE,sep=" ")  # read in the file
  names(activity_labels) <- c("activityid", "activityname")                  # set variable names

# features input
  features <- read.delim("features.txt",header=FALSE,sep=" ")    # read in the file
  names(features) <- c("featureid","featurename")                # set variable names
  
# subject train 
setwd("C:/Users/leedo002/Documents/Education/Data Science by JHU/Course 3 - Getting and Cleaning Data/FinalAssignment/UCI HAR Dataset/train")
  subject_train <- read.delim("subject_train.txt",header=FALSE,sep=" ")  # read in the file
  names(subject_train) <- "subjectid"                                    # set variable names
# subject test 
setwd("C:/Users/leedo002/Documents/Education/Data Science by JHU/Course 3 - Getting and Cleaning Data/FinalAssignment/UCI HAR Dataset/test")
  subject_test <- read.delim("subject_test.txt",header=FALSE,sep=" ")   # read in the file
  names(subject_test) <- "subjectid"                                    # set variable names
  
#################################################################################################################
## Modify variable names, merge data from the various data frames, add new columns, and subset columns to 
## only the columns needed.
#################################################################################################################

#  rename training and test variables based on features.txt
  featurenames<- features$featurename
  names(training) <- featurenames
  names(test)<-featurenames

# merge training, training_labels, subject_train, and activity_labels to add new columns called activityname
#   and subjectid
  training_ext<-training %>% mutate(activityid=training_labels$V1, subjectid=subject_train$subjectid) %>% join(activity_labels, type="inner",by="activityid")

# merge test, test_labels, subject_test, and activity_labels to add new columns called activityname
#   and subjectid
  test_ext<-test %>% mutate(activityid=test_labels$V1, subjectid=subject_test$subjectid) %>% join(activity_labels, type="inner",by="activityid")

# union training and test into one data frame called completeSet
  completeSet<-rbind(training_ext, test_ext)

# subset completeSet to only the needed columns 
## NOTE: I have decided that only the mean() and std() variables are to be used 
  completeSetTb <- tbl_df(completeSet)
  subsetSet<-cbind(select(completeSetTb,activityname,subjectid),select(completeSetTb,ends_with("mean()")),select(completeSetTb,ends_with("std()")))

## tidy up the variable names per variable name guidelines
  names(subsetSet)<-gsub("\\(\\)","",names(subsetSet))    # remove "()" 
  names(subsetSet)<-tolower(names(subsetSet) )            # convert upper case characters to lower case
  names(subsetSet)<-gsub("-","",names(subsetSet))         # remove hyphens
  names(subsetSet)<-sub("std$","stddev",names(subsetSet)) # change "std" to "stddev" as "std" is common abbreviation for "standard"
  
# create a second data set with average of each variable for each activityname and subjectid
  secondSubsetSet <- arrange(subsetSet,activityname,subjectid)
  secondSubsetSet <- ddply(secondSubsetSet,c("activityname","subjectid"),summarise
                           , mean(tbodyaccmagmean   )        
                           , mean(tgravityaccmagmean )        
                           , mean(tbodyaccjerkmagmean)        
                           , mean(tbodygyromagmean   )       
                           , mean(tbodygyrojerkmagmean )      
                           , mean(fbodyaccmagmean     )       
                           , mean(fbodybodyaccjerkmagmean )  
                           , mean(fbodybodygyromagmean    )   
                           , mean(fbodybodygyrojerkmagmean )  
                           , mean(tbodyaccmagstddev       )  
                           , mean(tgravityaccmagstddev   )    
                           , mean(tbodyaccjerkmagstddev  )    
                           , mean(tbodygyromagstddev     )   
                           , mean(tbodygyrojerkmagstddev)     
                           , mean(fbodyaccmagstddev       )   
                           , mean(fbodybodyaccjerkmagstddev )
                           , mean(fbodybodygyromagstddev     )
                           , mean(fbodybodygyrojerkmagstddev)
                          )
  names(secondSubsetSet) <- names(subsetSet)   # preserve column names

###############################################################################################################
# Write out table to a file
###############################################################################################################
  setwd("C:/Users/leedo002/Documents/Education/Data Science by JHU/Course 3 - Getting and Cleaning Data/FinalAssignment")
  write.table(secondSubsetSet,file="tidysamsungaverages.txt",row.names=FALSE)
  