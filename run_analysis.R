## Load libraries
library(dplyr)
library(plyr)
library(stringr)

## Download file
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
temp<-tempfile() # create a temporary file
download.file(fileUrl,temp) #download file into temporary location

## Read in test and training data/labels
testData<-read.table(unz(temp,"UCI HAR Dataset/test/X_test.txt"))
testLabel<-read.table(unz(temp,"UCI HAR Dataset/test/y_test.txt"))##Activity labels for testing dataset
testSubj<-read.table(unz(temp,"UCI HAR Dataset/test/subject_test.txt"))##subject labels for testing dataset
trainData<-read.table(unz(temp,"UCI HAR Dataset/train/X_train.txt"))
trainLabel<-read.table(unz(temp,"UCI HAR Dataset/train/y_train.txt"))##Activity labels for training dataset
trainSubj<-read.table(unz(temp,"UCI HAR Dataset/train/subject_train.txt"))##subject labels for training dataset
colLabelOrig<-read.table(unz(temp,"UCI HAR Dataset/features.txt"))##Labels for column headings
actLabels<-read.table(unz(temp,"UCI HAR Dataset/activity_labels.txt"))##Labels for activities
unlink(temp)# remove temporary file


## Modify column label names for test and training data to be more comprehensive/descriptive
colLabel<-select(colLabelOrig,2) ## maintain only column with labels 
colLabel<-as.data.frame(sapply(colLabel,function (x) gsub("-","",x))) # remove dashes
colLabel<-as.data.frame(sapply(colLabel,function (x) gsub("\\(","",x))) # remove left parenthesis
colLabel<-as.character(sapply(colLabel,function (x) gsub("\\)","",x))) # remove right parenthesis


## Modify column label names for subject and activity ids
colnames(testLabel)<-"ActId" #rename testlabel column to Activity Id
colnames(trainLabel)<-"ActId" #rename trainlabel column to Activity Id
colnames(testSubj)<-"SubjId" #rename testSubj column to Sunject Id
colnames(trainSubj)<-"SubjId" #rename trainSubj column to Subject Id


## Create a vector to indicate if column has mean or std in text, apply col labels
## to test and training data, extract appropriate columns 
meanSDCols<-grep("mean|std",colLabel) # create vector of column indices that contain mean or std
colnames(testData)<-colLabel ## rename test data columns
colnames(trainData)<-colLabel ## rename training data columns
testDataMeanStd<-select(testData,meanSDCols) #select test data columns with only mean and std
trainDataMeanStd<-select(trainData,meanSDCols) #select train data columns with only mean and std

## Create a vector including activity descriptions
testActLab<-sapply(testLabel, function(x) actLabels[x,2]) 
trainActLab<-sapply(trainLabel, function(x) actLabels[x,2]) 
colnames(testActLab)<-"ActLab"
colnames(trainActLab)<-"ActLab"

## Include subject id, activity id and descriptions to test and training data and merge data sets
testDataFin<-cbind(testSubj,testLabel,testActLab,testDataMeanStd) 
trainDataFin<-cbind(trainSubj,trainLabel,trainActLab,trainDataMeanStd) 
testTrain<-rbind(testDataFin,trainDataFin)


## Create second dataset from testTrain with the average of each variable for
# each activity and each subject
subjActId<- paste(testTrain$SubjId,testTrain$ActId,sep="-") #create new id which includes subject and activity
names(subjActId)<-"SubjActId"
testTrainRev<-cbind(subjActId,testTrain) # include new id in data frame
testTrainRevFin<-testTrainRev %>% group_by(subjActId) %>% summarise_all(funs(mean)) ## new dataset by subject and activity
testTrainActLab<-sapply(testTrainRevFin$ActId, function(x) actLabels[x,2]) # identify labels for activities
testTrainRevFin$ActLab<-testTrainActLab # update labels of activities (NA returned after mean applied)


## Write final data set in textfile
write.table(testTrainRevFin,file="tidy_data.txt",row.names=FALSE)
