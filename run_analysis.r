# Clear variables and load r packages used
rm(list=ls(all=TRUE))
setwd("~/Data")
library(base)
library(dplyr)

# Read in the Activity labels and retain only the name of the Activity without the number in the front
la <- readLines("UCI HAR Dataset/activity_labels.txt")
ActivityLabels<- substring(la, 3)

# Read in the Features labels and retain only the name of the Feature without the number in the front
fe <- readLines("UCI HAR Dataset/features.txt")
for (i in 1:9){
  fe[i]<-substring(fe[i],3)
}
for (i in 10:99){
  fe[i]<-substring(fe[i],4)
}
for (i in 100:length(fe)){
  fe[i]<-substring(fe[i],5)
}

# Read in Test Dataset & its labels and combine the two; Add Activity column with the activity number
testData <- read.table("UCI HAR Dataset/test/X_test.txt")
testLabels<-readLines("UCI HAR Dataset/test/y_test.txt")
d<-tbl_df(testData)
d["Activity"]
d$Activity <- testLabels

# Read in the Train Dataset & its labels and combine the two; Add Activity column with the activity number
trainData <- read.table("UCI HAR Dataset/train/X_train.txt")
trainLabels<-readLines("UCI HAR Dataset/train/y_train.txt")
dt<-tbl_df(trainData)
dt["Activity"]
dt$Activity <- trainLabels

# Merge the two data sets into one data set (#1)
DataSet <-rbind(d,dt)

# Modify Activity column to use descriptive Activity names instead of numbers (#3)
for (i in 1:length(ActivityLabels)){
  j<-toString(i)
  DataSet$Activity[DataSet$Activity==j]<-ActivityLabels[i]
}

# Replace column names to use descriptive variable names for the Features. See Codebook for exact meaning. (#4)
n<- names(DataSet)
for (i in 1:(length(n)-1)){
  names(DataSet)[i]<- fe[i]
}

# Extract only the measurements on the mean and standard deviation for each measurement.See CodeBook for logic (#2)
cols<-c("mean()", "meanFreq()", "gravityMean", "tBodyAccMean", "tBodyAccJerkMean", "tBodyGyroMean", "tBodyGyroJerkMean", "std()") 
p<-paste(cols, collapse="|")
mean_and_std_col_index<- unique(grep(p,fe,ignore.case=TRUE)) # the index of the cols refering to mean or std
m <- c(562, mean_and_std_col_index) # Add Activity column back to the data frame
TidyData <- DataSet[m]

View(TidyData) # This is the Tidy dataset after steps 1 through 4

# NewTidyData that contains the average of each variable for each activity and each subject (#5)
NewTidyData <-(
  TidyData%>%
    group_by(Activity) %>%
      summarise_each(funs(mean))
  )
View(NewTidyData) 

# Writing the NewTidyData from #5 out to a text file
write.table(NewTidyData,"NewTidyData.txt",row.name=FALSE, sep="\t") #tab delimited text file with the new tidy data set 
# Check that the file is read in corectly
# data <- read.table("~/Data/NewTidyData.txt", header = TRUE) 
# View(data)








