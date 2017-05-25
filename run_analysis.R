
#******Q1: Merges the training and the test sets to create one data set.

#----- download "downloader" package to download file from web

install.packages("downloader")
library("downloader")
#--------- download files into the directory

download("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",dest="zipfile.zip",mode="wb")

unzip("zipfile.zip",exdir="./Getting and Cleaning Data")

unzip(zipfile="zipfile.zip",exdir="./Getting and Cleaning Data")

#---- to see, the files in the directory
list.files("./Getting and Cleaning Data/UCI HAR Dataset")

#---- Read and row bind Subject files test and train

testsubjcet<-read.table("Getting and Cleaning Data/UCI HAR Dataset/test/subject_test.txt",header = F)
trainsubjcet<-read.table("Getting and Cleaning Data/UCI HAR Dataset/train/subject_train.txt",header = F)

test_train_subject<-rbind(testsubjcet,trainsubjcet)

colnames(test_train_subject)<-c("subjectid")
#-----Read and row bind test and training data set

testdata<-read.table("Getting and Cleaning Data/UCI HAR Dataset/test/X_test.txt",header = F)
traindata<-read.table("Getting and Cleaning Data/UCI HAR Dataset/train/X_train.txt",header = F)

test_train_data<-rbind(testdata,traindata)

#--------- read header file

Header_for_data_file<-read.table("Getting and Cleaning Data/UCI HAR Dataset/features.txt",header = F)

#-------- add header to the combined file
colnames(test_train_data)<- make.names(Header_for_data_file$V2,unique = T)

#---------- Read and row bind the test and train labels

testlabel<-read.table("Getting and Cleaning Data/UCI HAR Dataset/test/y_test.txt",header = F)
trainlabel<-read.table("Getting and Cleaning Data/UCI HAR Dataset/train/y_train.txt",header = F)

test_train_label<-rbind(testlabel,trainlabel)

#------- add header to the combined file

colnames(test_train_label)<-c("activityid")

#------------ Column bind all the data frames formed above

test_train <- cbind(test_train_subject,test_train_label,test_train_data)

dim(test_train) #--- to check the dimension of the data frame

#******Q2: Extracts only the measurements on the mean and standard deviation for each measurement.

install.packages("dplyr")
library("dplyr")


mean_std_data_by_measure<-select(test_train,matches("subjectid|activity|mean|std"))

#*******Q3: Uses descriptive activity names to name the activities in the data set

activitylabels=read.table("Getting and Cleaning Data/UCI HAR Dataset/activity_labels.txt",header = F)

test_train_with_actvity_labels<-merge(test_train,activitylabels,by.x="activityid",by.y="V1")

dim(test_train_with_actvity_labels)

#********Q4: Appropriately labels the data set with descriptive variable names.

colnames(test_train_with_actvity_labels)<-gsub("mean()","mean_value",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("std()","standard deviation",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("mad()","Median_absolute_deviation",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("max()","Largest_value_in_array",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("min()","Smallest_value_in_array",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("energy()","Energy measure. Sum of the squares divided by the number of values.",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("iqr()", "Interquartile_range",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("entropy()", "Signal entropy",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("arCoeff()", "Autorregresion coefficients with Burg order equal to 4",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("correlation()", "correlation coefficient between two signals",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("maxInds()","index of the frequency component with largest magnitude",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("meanFreq()", "Weighted average of the frequency components to obtain a mean frequency",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("skewness()","skewness of the frequency domain signal",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("energy()","Energy measure. Sum of the squares divided by the number of values.",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("kurtosis()", "kurtosis of the frequency domain signal",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("bandsEnergy()","Energy of a frequency interval within the 64 bins of the FFT of each window",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-gsub("angle()","Angle between to vectors",colnames(test_train_with_actvity_labels))
#---- replace t=time, f=frequency
colnames(test_train_with_actvity_labels)<-sub("^t","time",colnames(test_train_with_actvity_labels))
colnames(test_train_with_actvity_labels)<-sub("^f","frequency",colnames(test_train_with_actvity_labels))
#----- replace blank space in the variable name
colnames(test_train_with_actvity_labels)<-gsub(" ","_",colnames(test_train_with_actvity_labels))
#------ giving variable a meaningful name
test_train_with_actvity_labels<-rename(test_train_with_actvity_labels,activityname=V2)

#********Q5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#---removing the redundent field

test_train_with_actvity_labels<-select(test_train_with_actvity_labels,-activityid)
#---calculating mean of the fileds except subjectid and activityname, they are used for grouping.

Aggdata<-aggregate(x=test_train_with_actvity_labels[,-c(1,2)],by=list("subjectid"=test_train_with_actvity_labels$subjectid,"activity"=test_train_with_actvity_labels$activityname),FUN="mean")

write.table(Aggdata, file = "./tidy_data.txt")


