## Project for the Coursera Course
## Getting and Cleaning Data J.T.Leek

## Read in the data, test and train subdirectories must be in current directory
testdf <- read.table("./test/X_test.txt", header=FALSE)
traindf <-read.table("./train/X_train.txt", header=FALSE)
rawdata <- rbind(testdf, traindf)
featureNames <- read.table("./features.txt", header=FALSE, colClasses=c("numeric", "character"))
colnames(rawdata) <- featureNames[,2]
## At this point the data frame is 10299 X 561
## Extract only the measurements on the mean and std dev for each measurement
## Choose data for further analysis - means and std deviations
neededCols <- c("tBodyAcc-mean()-X", "tBodyAcc-mean()-Y", "tBodyAcc-mean()-Z", 
                "tBodyAcc-std()-X", "tBodyAcc-std()-Y", "tBodyAcc-std()-Z",
                "tBodyGyro-mean()-X", "tBodyGyro-mean()-Y", "tBodyGyro-mean()-Z",
                "tBodyGyro-std()-X", "tBodyGyro-std()-Y", "tBodyGyro-std()-Z",
                "fBodyAcc-mean()-X", "fBodyAcc-mean()-Y", "fBodyAcc-mean()-Z",
                "fBodyAcc-std()-X", "fBodyAcc-std()-Y", "fBodyAcc-std()-Z")

selectdata <- rawdata[, neededCols]

## Read the y data files - these files contain the activities specifier for each row
## of the combined data file
to.read_train <- file("./train/y_train.txt", "rb")
trainActivity <- readBin(to.read_train, integer(), n=7352, size=2, endian="little")
close(to.read_train)
to.read_test <- file("./test/y_test.txt", "rb")
testActivity <- readBin(to.read_test, integer(), n=2947, size=2, endian="little")
close(to.read_test)
activity <- c(testActivity, trainActivity)

    
subjtst <- read.table("./test/subject_test.txt")
subjtrain <- read.table("./train/subject_train.txt")
## 10299 X 1 dataframe of subjects 1-30
subj <- rbind(subjtst, subjtrain)

newdf <- cbind(selectdata, data.frame(activity, subj))
colnames(newdf) <- c(neededCols, "activity", "subject")

## Step 5 Create an independent tidy data set with the average of each variable for
## each activity and each subject

## First create an empty data frame with the correct column types
motiondf <- data.frame(activity=integer(), subject=integer(),
            BodyAccMeanX=numeric(),BodyAccMeanY=numeric(),BodyAccMeanZ=numeric(),
            BodyStdMeanX=numeric(),BodyStdMeanY=numeric(),BodyStdMeanZ=numeric(),
            BodyGyroMeanX=numeric(),BodyGyroMeanY=numeric(),BodyGyroMeanZ=numeric(),
            BodyGyroStdX=numeric(),BodyGyroStdY=numeric(),BodyGyroStdZ=numeric(),
            FreeBodyAccMeanX=numeric(),FreeBodyAccMeanY=numeric(),FreeBodyAccMeanZ=numeric(),
            FreeBodyAccStdX=numeric(),FreeBodyAccStdY=numeric(),FreeBodyAccStdZ=numeric())

            
for(i in unique(newdf[,"activity"])){
    for(j in 1:30) {
        useidx <- newdf[,"activity"] == i & newdf[, "subject"] == j
        actcol1 <- i ## activity index
        actcol2 <- j ## subject index
        ## Extract and take the mean of each variables for one activity and one subject into a data frame row
        actcol3 <- mean(newdf[useidx, "tBodyAcc-mean()-X"])
        actcol4 <- mean(newdf[useidx, "tBodyAcc-mean()-Y"])
        actcol5 <- mean(newdf[useidx, "tBodyAcc-mean()-Z"])
        actcol6 <- sqrt(sum(newdf[, "tBodyAcc-std()-X"]^2))
        actcol7 <- sqrt(sum(newdf[, "tBodyAcc-std()-Y"]^2))
        actcol8 <- sqrt(sum(newdf[, "tBodyAcc-std()-Z"]^2))
        actcol9 <- mean(newdf[useidx, "tBodyGyro-mean()-X"])
        actcol10 <- mean(newdf[useidx, "tBodyGyro-mean()-Y"])
        actcol11 <- mean(newdf[useidx, "tBodyGyro-mean()-Z"])
        actcol12 <- sqrt(sum(newdf[, "tBodyGyro-std()-X"]^2))
        actcol13 <- sqrt(sum(newdf[, "tBodyGyro-std()-Y"]^2))
        actcol14 <- sqrt(sum(newdf[, "tBodyGyro-std()-Z"]^2))
        actcol15 <- mean(newdf[useidx, "fBodyAcc-mean()-X"])
        actcol16 <- mean(newdf[useidx, "fBodyAcc-mean()-Y"])
        actcol17 <- mean(newdf[useidx, "fBodyAcc-mean()-Z"])
        actcol18 <- sqrt(sum(newdf[, "fBodyAcc-std()-X"]^2))
        actcol19 <- sqrt(sum(newdf[, "fBodyAcc-std()-Y"]^2))
        actcol20 <- sqrt(sum(newdf[, "fBodyAcc-std()-Z"]^2))
        newrow <- c(actcol1, actcol2, actcol3, actcol4, actcol5, actcol6,
                    actcol7, actcol8, actcol9, actcol10, actcol11, actcol12,
                    actcol13, actcol14, actcol15, actcol16, actcol17, actcol18,
                    actcol19, actcol20)
        ## Add the row to the output data frame
        motiondf = rbind(motiondf, newrow)
    }
}
## rename the data frame columns
colnames(motiondf) <- c("activity", "subject", 
                       "Body Acceleration Mean X", "Body Acceleration Mean Y", "Body Acceleration Mean Z", 
                       "Body Acceleration Std X", "Body Acceleration Std Y", "Body Acceleration Std Z", 
                       "Body Gyro Mean X", "Body Gyro Mean Y", "Body Gyro Mean Z", 
                       "Body Gyro Std X", "Body Gyro Std Y", "Body Gyro Std Z", 
                       "Free Body Acc Mean X", "Free Body Acc Mean Y", "Free Body Acc Mean Z", 
                       "Free Body Acc Std X", "Free Body Acc Std Y", "Free Body Acc Std Z")
## convert the activity column to a factor column
motiondf[,1] <- factor(motiondf[,1], labels=c("WALKING", "WALKING_UPSTAIRS", 
                        "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))
write.table(motiondf, file="myTidyMotionData.txt", row.names=FALSE)