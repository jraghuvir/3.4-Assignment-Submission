## A. Merge the training and the test sets to create one data set.
## A0. Import all data and read into dataframes.

subjecttest <- read.csv("./UCI HAR Dataset/test/subject_test.txt", header=FALSE)
subjecttrain <- read.csv("./UCI HAR Dataset/train/subject_train.txt",header=FALSE)
ytest <- read.csv("./UCI HAR Dataset/test/y_test.txt", header=FALSE)
ytrain <- read.csv("./UCI HAR Dataset/train/y_train.txt",header=FALSE)

## A0.1 for x, spearator "" is specified to extract each variable in separate column
xtest <- read.csv("./UCI HAR Dataset/test/X_test.txt",sep ="", header=FALSE)
xtrain <- read.csv("./UCI HAR Dataset/train/X_train.txt",sep ="", header=FALSE)

## A1. rbind the subject, y, x data sets, test on top, train on bottom. 

y <- rbind(ytest,ytrain)
subject <- rbind(subjecttest,subjecttrain)

##A1.1 to ribind x, we will need to give columns names which are in features.txt file.
##A1.1.1 importing features.txt file
library(readr)
features <- read_lines("./UCI HAR Dataset/features.txt")

##A1.1.2 giving x files correct variable names from features.txt and then doing rbind
names(xtest) <- features
names(xtrain) <- features
x <- rbind(xtest,xtrain)

## A2. for x, select required columns:mean  and standard deviation for all signal types
stdmatch<-grep('std\\(\\)', features, value=TRUE)
meanmatch <-grep('mean\\(\\)', features, value=TRUE)
allmatch<- c(stdmatch, meanmatch)
x2 <- x[,allmatch]

## A3. cbind all 3 data sets: subject, y and x.
result<-cbind( subject, y, x2)

## A4. Give appropriate column names.
colnames(result)[1] <- "subject"
colnames(result)[2] <- "activity"

## B. Use descriptive activity names to name the activities in the data set.
## B1. pull activity labels
activitylabels <- read.csv("./UCI HAR Dataset/activity_labels.txt", sep=" ", header=FALSE, col.names = (c("activityid","activitydesc")))

## B2. merge them with result dataset
result2 <- (merge(activitylabels, result, by='activity'))
## C. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
## C1. group the result2 by activity and subject
result3 <- tbl_df(result2)
result3_group <- group_by(result3, activitydesc, subject)
## C2. summarize the table while calculating average of all columns by activitydesc and subject
result4_summ<- result3 %>%
     group_by(activitydesc,subject) %>%
     summarise_all(mean)
## C3. remove column "activity" which is an ID an not needed as we have activity description
result4_summ[3]<-NULL
## C4. write the data to a file
write.table(result4_summ, file="feature_mean_sd.txt", sep=",", row.name=FALSE)

## Note: numbers from the column names are not removed as it serves as a link with original file.