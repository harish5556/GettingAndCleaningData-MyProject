########final assignment getting and cleaning datta ####
library(data.table)
setwd("G:/coursera/cleaning data/asgn")

list.files("Dataset",recursive = TRUE)

path <- getwd()
path
list.files(path)
path <- file.path(path,"Dataset")

####reading subject files######

subtrain <- read.table(file.path(path,"train","subject_train.txt"))
subtraindt <- data.table(subtrain)

subtest <- read.table(file.path(path,"test","subject_test.txt"))
subtestdt <- data.table(subtest)
subtestdt


##### reading activity or label files ####

acttrain <- read.table(file.path(path,"train","y_train.txt"))
acttraindt <- data.table(acttrain)
acttraindt

acttest <- read.table(file.path(path,"test","y_test.txt"))
acttestdt <- data.table(acttest)
acttestdt


#####reading datasets ######
train <- read.table(file.path(path,"train","X_train.txt"))
traindt <- data.table(train)
traindt

test <- read.table(file.path(path,"test","X_test.txt"))
testdt <- data.table(test)
testdt




######### bind train and test tables ####

subtable <- rbind(subtraindt,subtestdt)
subtable
setnames(subtable,"V1","subject")
subtable

acttable <- rbind(acttraindt,acttestdt)
acttable
setnames(acttable,"V1","activitynum")
acttable

#######bind table####
table <- rbind(traindt,testdt)
table

############ Final merging#####

merge1 <- cbind(subtable,acttable)
ftable <- cbind(merge1,table)
merge1
ftable


#############setting key######
setkey(ftable,subject,activitynum)

######Extracting mean and standard deviation 
####reading features.txt to what mean and standard deviation are in the table "ftable"

featuredt <- fread(file.path(path,"features.txt"))
featuredt

setnames(featuredt,names(featuredt),c("featurenum","featurename"))

req_featdt <- featuredt[grepl("mean\\(\\)|std\\(\\)",featurename)]
req_featdt
req_featdt$featureCode <- req_featdt[,paste0("V",featurenum)]
req_featdt$featureCode

cnames <- c(key(ftable),req_featdt$featureCode)
cnames 
ftable[,cnames,with=FALSE]
  
####################### Giving descriptive names  
####reading activity_labels.txt
library(data.table)
act_labels <- fread(file.path(path,"activity_labels.txt"))
act_labels

setnames(act_labels,names(act_labels),c("activitynum","activityname"))  
  
#########labelling with descripting names ####
ftable <- merge(ftable,act_labels, by="activitynum",all.x = T)

setkey(ftable,subject,activitynum,activityname)
head(ftable)


ftable <- melt(ftable,key(ftable),variable.name = "featureCode")
head(ftable)
ftable

####### merge activity name############

req_featdt
ftable <- merge(ftable,req_featdt[,list(featurenum,featurename,featureCode)],by="featureCode" )
ftable

#########creating factors for activityname and feature name##

ftable$activity <- factor(ftable$activityname)
ftable$feature <- factor(ftable$featurename)
ftable

grepfunc <- function(temp){
  grepl(temp,ftable$feature)
}

################feature with two categories
n<-2
a <- matrix(seq(1,n),nrow = n)
a
b<- matrix(c(grepfunc("^t"),grepfunc("^f")),ncol=nrow(a))
b
ftable$feat_domain <- factor(b %*% a, labels=c("Time","Freq"))

b<-matrix(c(grepfunc("Acc"),grepfunc("Gyro")),ncol = nrow(a))
ftable$feat_instru <- factor(b %*% a,labels = c("Accelerometer","Gyroscope"))
ftable

b<- matrix(c(grepfunc("BodyAcc"),grepfunc("GravityAcc")),ncol = nrow(a))
ftable$feat_acceleration <- factor(b %*% a, labels= c(NA,"Body","Gravity"))

b <- matrix(c(grepfunc("mean()"),grepfunc("std()")),ncol = nrow(a))
ftable$feat_var <-factor(b %*% a,labels=c("Mean","StD") )
ftable

##save.image("project.Rdata")


##features with one category

ftable$feat_jerk <- factor(grepfunc("Jerk"),labels = c(NA, "Jerk"))
ftable$feat_magnitude <- factor(grepfunc("Mag"),labels = c(NA,"Magnitude"))
ftable

#####features with three category

n<-3
a <- matrix(seq(1,n),nrow = n)
b <- matrix(c(grepfunc("-X"),grepfunc("-Y"),grepfunc("-Z")), ncol= nrow(a))
ftable$feat_axis <- factor(b%*%a, labels = c(NA,"X","Y","Z") )


###########checking if all features are present

r1 <- nrow(ftable[, .N, by= c("feature")])
r1
r2 <- nrow(ftable[, .N , by =c("feat_domain", "feat_acceleration", "feat_instru","feat_jerk", "feat_magnitude", "feat_var", "feat_axis")])

r1==r2


#################Creating tidy data 

setkey(ftable,subject,activity,feat_domain, feat_acceleration, feat_instru,feat_jerk, feat_magnitude, feat_var,feat_axis)
tidydata <- ftable[,list(count=.N,average = mean(value)),by= key(ftable)]
tidydata



##########creating text file to submit Tidy data

f <- file.path(path, "TidyData.txt")
write.table(tidydata, f, quote=FALSE, sep="\t", row.names=FALSE)

