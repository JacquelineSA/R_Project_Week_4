installed.packages("dplyr")
library(dplyr)

#First I'll download the file:
if(!file.exists("./data")){dir.create("./data")}
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "./data/Dataset.zip", method = "curl")

#Then I'll unzip the file: 
unzip(zipfile = "./data/Dataset.zip", exdir = "./data")

#I will get the list of the files from the folder called UCI HAR Dataset, in which they are stored:
DataPath <- file.path("./data", "UCI HAR Dataset")
files <- list.files(DataPath, recursive = TRUE)
files 

#Before we merge our datasets, we will read the data into variables:
Training_Features <- read.table(file.path(DataPath, "train", "x_train.txt"))
Training_Activity <- read.table(file.path(DataPath, "train", "y_train.txt"))
Training_Subject <- read.table(file.path(DataPath, "train", "subject_train.txt"))

Test_Features <- read.table(file.path(DataPath, "test", "x_test.txt"))
Test_Activity <- read.table(file.path(DataPath, "test", "y_test.txt"))
Test_Subject <- read.table(file.path(DataPath, "test", "subject_test.txt"))

#Now we check our varibles:
str(Training_Features)
str(Training_Activity)
str(Training_Subject)

str(Test_Features)
str(Test_Activity)
str(Test_Subject)

#We are now able to link the data tables together by row using rbind:
FeaturesData <- rbind(Training_Features, Test_Features)
ActivityData <- rbind(Training_Activity, Test_Activity)
SubjectData <- rbind(Training_Subject, Test_Subject)

#Then we will set the name to the varianles:
names(ActivityData) <- c("Activity")
names(SubjectData) <- c("Subject")
FeaturesDataNames <- read.table(file.path(DataPath, "features.txt"))
names(FeaturesData) <- FeaturesDataNames$V2

#1. Merges the training and the test sets to create one data set.
#Now we can merge the columns to get the data frame:
CombineData <- cbind(ActivityData, SubjectData)
Data <- cbind(FeaturesData, CombineData)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#Here I'll create a vector of only the measurements on the mean and standard deviation:
MeanStdVector <- grep("mean()|std()", FeaturesDataNames$V2)
#Then I subset the name of Features with the measurements on the mean and std:
Subset <- FeaturesDataNames$V2[MeanStdVector]
SelectNames <- c(as.character(Subset), "Subject", "Activity")
NewData <- subset(Data, select = SelectNames)

#3. Uses descriptive activity names to name the activities in the data set.
#First I read the descriptive activity names from "activity_labels.txt":
ActLab <- read.table(file.path(DataPath, "activity_labels.txt"))
head(ActLab)
head(NewData$Activity, 5)
#Then I can give descriptive activity names to the activities in the data set:
NewData$Activity <- factor(NewData$Activity, levels = ActLab[, 1], labels = ActLab[, 2]) 
head(NewData$Activity, 5)

#4. Appropriately labels the data set with descriptive variable names.
names(NewData) <- gsub("^t", "time", names(NewData))
names(NewData) <- gsub("^f", "freauency", names(NewData))
names(NewData) <- gsub("Acc", "Accelerometer", names(NewData))
names(NewData) <- gsub("Gyro", "Gyroscope", names(NewData))
names(NewData) <- gsub("Mag", "Magnitude", names(NewData))
names(NewData) <- gsub("BodyBody", "Body", names(NewData))
names(NewData)

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#In install the plyr packages:
install.packages("plyr")
library(plyr)
#Since we are working with a data frame, I can now use the aggragate function from the package:
NewData2 <- aggregate(. ~ Subject + Activity, NewData, mean)
# Now we use the order function:
NewData2 <- NewData2[order(NewData2$Subject, NewData2$Activity),]
#We can now create the second data set:
write.table(NewData2, file = "tidydate.txt", row.name = FALSE)

