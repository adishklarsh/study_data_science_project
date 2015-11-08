# setwd("Coursera R Course\\ex3\\")


# This function reads the outcome-of-care-measures.csv file and returns a 
# character vector with the name of the hospital that has the best (i.e. lowest)
# 30-day mortality for the specified outcome in that state. The hospital name is
# the name provided in the Hospital.Name variable. The outcomes can be one of 
# "heart attack", "heart failure", or "pneumonia". Hospitals that do not have 
# data on a particular outcome should be excluded from the set of hospitals when
# deciding the rankings.

run_analysis <- function() {

    # load data
    Xtrain = read.table("./Data/UCI HAR Dataset/train/X_train.txt",colClasses = "character")
    Ytrain = read.table("./Data/UCI HAR Dataset/train/y_train.txt",colClasses = "character")
    subject_train = read.table("./Data/UCI HAR Dataset/train/subject_train.txt",colClasses = "character")
    
    Xtest = read.table("./Data/UCI HAR Dataset/test/X_test.txt",colClasses = "character")
    Ytest = read.table("./Data/UCI HAR Dataset/test/y_test.txt",colClasses = "character")
    subject_test = read.table("./Data/UCI HAR Dataset/test/subject_test.txt",colClasses = "character")
    
    activity_labels = read.table("./Data/UCI HAR Dataset/activity_labels.txt",colClasses = "character")
    features = read.table("./Data/UCI HAR Dataset/features.txt",colClasses = "character")
    
    # column names and types
    activity_labels = rename(activity_labels, activityNum = V1, activityString = V2)
    subject_train = rename(subject_train, subjectNumber = V1)
    subject_test = rename(subject_test, subjectNumber = V1)
    Ytrain = rename(Ytrain, activityLabel = V1)
    Ytest = rename(Ytest, activityLabel = V1)
    features = rename(features, featureId = V1, featureString = V2)
    
    # factor SubjectNum
    subject_train = mutate(subject_train, subjectNum = factor(subject_train$subjectNum))
    subject_test = mutate(subject_test, subjectNum = factor(subject_test$subjectNum))
    
    # factor activity
    Ytrain = mutate(Ytrain, activityLabel = factor(activityLabel, levels = activity_labels$activityNum, labels = activity_labels$activityString)) 
    Ytest = mutate(Ytest, activityLabel = factor(activityLabel, levels = activity_labels$activityNum, labels = activity_labels$activityString))
    
    # quantify numerics in data
    Xtrain = as.data.frame(lapply(Xtrain,as.numeric))
    Xtest = as.data.frame(lapply(Xtest,as.numeric))
    
    # factor features
    names(Xtrain) = paste(features$featureId,features$featureString, sep=": ")
    names(Xtest) = paste(features$featureId,features$featureString, sep=": ")
    
    # factor combined subjects and combined activities
    subjectFactor = as.factor((rbind(subject_train,subject_test))$subjectNum)
    YdataFactor = as.factor((rbind(Ytrain,Ytest))$activityLabel)
    
    # combine data
    Xdata = rbind(Xtrain,Xtest)
    
    # build factor of subject X activity, split data by this factor and calculate means by it
    factorSep = ": "
    subjectYFactor = interaction(subjectFactor,YdataFactor, sep=factorSep)
    splitData = split(Xdata,subjectYFactor)
    factorMeans = t(sapply(splitData, colMeans))
    
    # build skinny matrix with subject, activity, measurement and value of group mean
    result = melt(factorMeans, id=0 ,measure.vars=subjectYFactor)
    
    # rename measuremnt and value columns
    result = rename(result, measurement=Var2)
    result = rename(result, groupMean = value)
    
    # remove column number from measurement column
    result = mutate(result, measurement = sapply(strsplit(as.character(result$measurement),": "), function(x) x[2]))
    
    # split subject X activity column to two columns - subject, activity
    splitNames = strsplit(as.character(result$Var1),": ")
    result = mutate(result, subject = sapply(splitNames,function(x) x[1]), activity = sapply(splitNames,function(x) x[2]))
    
    # select chosen columns
    result = select(result, subject, activity, measurement, value)
}