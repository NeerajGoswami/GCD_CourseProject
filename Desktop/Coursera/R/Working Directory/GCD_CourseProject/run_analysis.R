options(warn=-1)
## Assumption: All files are present in the working directory

## Get measurements from test & train files
## Measurements are present in X_test & X_train
X_test <- read.table("X_test.txt", header= FALSE)
X_train <- read.table("X_train.txt", header= FALSE)

## Get activities associated with each row of measurements
## Activity codes are present in y_test & y_train
y_test <- read.table("y_test.txt", header= FALSE)
y_train <- read.table("y_train.txt", header= FALSE) 

## Get subjects associated with each row of measurement
## Subject nos. are present in subject_test & subject_train
subject_test <- read.table("subject_test.txt",header=FALSE)
subject_train <- read.table("subject_train.txt",header=FALSE)

## Merge measurement rows from test & train into a single column
X <- rbind(X_test, X_train)

## Merge activity id rows from test & train into a single column
y <- rbind(y_test,y_train)

## Merge subject nos. from test & train into a single column
subjects <- rbind(subject_test,subject_train)

## Combine measurements, subject nos. and activity codes to form
## a consolidated data set
consol_data <- cbind(X,subjects,y)

## Read column names for measurements from 'features'
col_names_measurements <- read.table("features.txt",header=FALSE)

## Get indices of relevant columns for extraction, with mean and
## standard deviation readings
## Criteria used for extraction should be acceptable, as discussed in
## https://class.coursera.org/getdata-008/forum/thread?thread_id=24

## Extract column indices from second column of col_names_measurements
## First for mean()
relv_cols_mean <- grep("mean()",col_names_measurements[[2]],fixed = TRUE)
## Then for std()
relv_cols_std <- grep("std()",col_names_measurements[[2]],fixed = TRUE)
## And combine both to get the final array
relv_cols <- sort(c(relv_cols_mean,relv_cols_std))

## Extract mean and standard deviation readings for each
## row of measurement
columns <- ncol(consol_data)
## initialise matrix to store measurements
relv_dataset <- NULL

## Extract measurements from relevant columns
for (i in 1:columns) {
    if (i %in% relv_cols) {
        relv_dataset <- cbind(relv_dataset,consol_data[,i])
        } 
        
}

## Use descriptive activity names to name activities in data set
## Read descriptive activity names from 'activity_labels' file
activity_labels <- read.table("activity_labels.txt", header=FALSE)

## Read activity codes from consolidated data set as factors
activity_codes <- as.factor(consol_data[,columns])

## Replace codes with descriptions
activity_desc <- activity_codes
levels(activity_desc) <- activity_labels[[2]]

## Create consolidated data set with relevant columns, alongwith
## columns for subject nos. and activity descriptions
consol_data2 <- cbind(relv_dataset,subjects,activity_desc)

## Assign appropriate column names for consol_data2
## Assign column names for measurements 
relv_cols_names <- col_names_measurements[relv_cols,2]
colnames(consol_data2) <- relv_cols_names

## Assign column names for Subject and Activity
colnames(consol_data2)[no_relv_cols-1] <- "Subject"
colnames(consol_data2)[no_relv_cols] <- "Activity"

## Create independent tidy data set with average of
## each variable for each activity and each subject
## Initialise tidy data set with Subject & Activity columns
tidy_data <- unique(consol_data2[,c("Subject","Activity")])
tidy_data <- tidy_data[order(tidy_data$Subject,tidy_data$Activity),]
colnames(tidy_data) <- c("Subject","Activity")

## Append tidy_data with averages for measurements
no_relv_cols <- ncol(consol_data2)
for (col in 1:(no_relv_cols-2)) {
    temp <- aggregate(consol_data2[,col],list(consol_data2$Subject,
            consol_data2$Activity),mean)
    colnames(temp) <- c("Subject","Activity","Value")
    tidy_data <- merge(tidy_data,temp, by.x=c("Subject","Activity"),
                       by.y=c("Subject","Activity"))
 }

## Order tidy data set by Subject
tidy_data <- tidy_data[order(tidy_data$Subject),]

## Replace unacceptable characters in column names
temp_names <- gsub("-","_",colnames(consol_data2))
temp_names <- gsub("\\(|\\)","",temp_names)
## Assign column names to tidy data set
colnames(tidy_data)[3:no_relv_cols] <- temp_names[1:(no_relv_cols-2)]

## Write tidy data set into file
write.table(tidy_data,file="tidy_data.txt",row.names=FALSE)

## To read data from tidy_data.txt, use
## read.table("tidy_data.txt",header = TRUE)