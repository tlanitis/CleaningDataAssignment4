---
title: "ReadME"
author: "T Lanitis"
date: "7/21/2020"
---


## Project Description
To create a tidy data set from raw data of human activity recognition using 
smartphones (Anguita et. al 2012) data set. Specifically to merge training and 
test data sets, extractonly measurements on the mean and standard deviation, 
use descriptive activity names and appropriately label the data set with 
descriptive variable names. An additional independent data set was also be 
created with average of each variablefor each activity and each subject.

## Study Design and Data Processing
The experiments have been carried out with a group of 30 volunteers within 
an age bracket of 19-48 years. 

## Collection of raw data
Each person performed six activities (WALKING, WALKING_UPSTAIRS, 
WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone 
(Samsung Galaxy S II) on the waist. Using its embedded accelerometer and 
gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity 
at a constant rate of 50Hz. The experiments have been video-recorded to label 
the data manually. The obtained dataset has been randomly partitioned into 
two sets, where 70% of the volunteers was selected for generating the training 
data and 30% the test data. 

## Processed data
Description has been modified based on "features_info.txt" file in original
codebook by Anguita et al. 2012. 

The variables for this database come from the accelerometer and gyroscope 
3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 
't' to denote time) were captured at a constant rate of 50 Hz. Then they were 
filtered using a median filter and a 3rd order low pass Butterworth filter with 
a corner frequency of 20 Hz to remove noise.  Similarly, the acceleration signal 
was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and 
tGravityAcc-XYZ) using another low pass Butterworth filter with a corner 
frequency of 0.3 Hz. Subsequently, the body linear acceleration and angular 
velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and 
tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were 
calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, 
tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). Finally a Fast Fourier 
Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, 
fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. 
(Note the 'f' to indicate frequency domain signals). These signals were used to
estimate variables of the feature vector for each pattern 

## Creating and cleaning tidy datafile
1. Data are downloaded into a temporary folder, with relevant tables read into
R memory using R code
2. Column labels for testing and training data sets are revised to be more 
comprehensive and additional variables are includes for subject and acitivity ids
3. Variables on mean and standard deviation are extracted from testing and training
datasets
4. Training and testing datasets are merged
5. An independent dataset providing means by subject id and activity is prepared


## Sources

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012..


