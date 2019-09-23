library(dplyr)
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "HARUS.zip"
download.file(fileURL, destfile=zipFile)
unzip(zipFile, exdir=".")
data.dir <- "UCI HAR Dataset"
fpathr <- function(set,index,flag=0){
  ext <- ".txt"          
  if(flag == 0){
    pathF <- paste(data.dir,"/",set,"/",index,"_",set,ext,sep="")
    tab  <- read.table(pathF,header=FALSE,colClasses="numeric")            
  } else if (flag == 1){
    pathF <- paste(data.dir,"/",set,ext,sep="")  
    tab  <- read.table(pathF,header=FALSE,colClasses="character")
  } 
  msg <- sprintf("Read file: %s",pathF); print(msg)
  tab
}
# Read: Training set (data,labels,subject) & Test set (data,labels,subject) 
#       & features & activity labels ...in this order
train.dat <- fpathr("train","X")
train.lab <- fpathr("train","y")
train.sub <- fpathr("train","subject")
test.dat  <- fpathr("test","X")
test.lab  <- fpathr("test","y")
test.sub  <- fpathr("test","subject")
features  <- fpathr("features",flag=1)
activ.lab <- fpathr("activity_labels",flag=1)
## 1. Label the Training & Test data sets with descriptive variable names
features[,2] <- gsub("BodyBody","Body",features[,2],fixed=TRUE)
valid.names <- make.names(features[,2],unique=TRUE,allow_=TRUE)
colnames(train.dat) <- valid.names
colnames(test.dat) <- valid.names

## 2. Subset to retain only the mean() and std() variables for the
#     Training & Test sets
train.dat <- select(train.dat,contains(".mean."),contains(".std."))
test.dat <- select(test.dat,contains(".mean."),contains(".std."))

## 3. Merge the Training & Test sets
#  3.1 For each set (Training,Test) combine the Subject and Labels tables
#  into one data frame & name the variables accordingly
train.sub.lab <- bind_cols(train.sub,train.lab)
colnames(train.sub.lab) <- c("Subject","Label")
test.sub.lab <- bind_cols(test.sub,test.lab)
colnames(test.sub.lab) <- c("Subject","Label")

# 3.2 Combine each further with the (Training,Test) measurements data
train.sub.lab.dat <- bind_cols(train.sub.lab, train.dat)
test.sub.lab.dat <- bind_cols(test.sub.lab, test.dat)

# 3.3 Merge the Training & Test tables now
Merged.dat <- merge(train.sub.lab.dat,test.sub.lab.dat,all=TRUE)

## 4. Label the activities with descriptive names
colnames(Merged.dat)[2] <- "Activity"
Merged.dat[,2] <- as.character(factor(Merged.dat[,2],labels=factor(activ.lab[,2])))

## 5. Create the tidy dataset as the average of each variable for 
#     each activity and each subject.
MergedAvg <- Merged.dat %>% 
  group_by(Subject,Activity) %>%
  summarise_each(funs(mean), -Subject,-Activity)
write.table(MergedAvg, file="MergedAvg.txt", row.names=FALSE)