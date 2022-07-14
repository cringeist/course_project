# creating the vector with id of needed measurement from X set
features <- read.delim("data/features.txt", sep = " ", header = FALSE)
names(features) <- c("id", "features")
filter_features <- features[grep("mean|std", features$features), 1]


#read all needed data (train directory)
subject_train <- read.delim("data/train/subject_train.txt", header = FALSE)
activity_train <- read.delim("data/train/y_train.txt", header = FALSE)
mean_std_train <- read.delim("data/train/X_train.txt", 
                      sep = "\n", header = FALSE)
subject_test <- read.delim("data/test/subject_test.txt", header = FALSE)
activity_test <- read.delim("data/test/y_test.txt", header = FALSE)
mean_std_test <- read.delim("data/test/X_test.txt", 
                           sep = "\n", header = FALSE)
activity_labels <- read.delim("data/activity_labels.txt", 
                              header = FALSE, sep = " ")


#cast vector of string to list of numeric vector (using lapply)
cast_num_vector <- function(data) {
    temp <- strsplit(x = data, " ")
    temp <- temp[[1]]
    temp <- as.numeric(temp)
    temp[!is.na(temp)]
}


# filter only needed measurement (using lapply)
filter_msr <- function(data) {
    data[filter_features]
}

temp_train <- lapply(mean_std_train$V1, cast_num_vector)
temp_test <- lapply(mean_std_test$V1, cast_num_vector)
temp_train <- lapply(temp_train, filter_msr)
temp_test <- lapply(temp_test, filter_msr)


#creating data frame of tidy data
har <- data.frame(matrix(nrow = length(temp_test) + length(temp_train), ncol = 3))
names(har) <- c("Subject", "Activity", "MeanStd")
har$Subject <- c(subject_train$V1, subject_test$V1)
har$Activity <- factor(c(activity_train$V1, activity_test$V1))
levels(har$Activity) <- activity_labels$V2
har$MeanStd <- c(temp_train, temp_test)


#function to group all elements of vector by activity and calculate average 
calc_mean_act <- function(data) {
    res <- c()
    temp <- c()
    levels_of_act <- levels(factor(data$Activity))
    for (i in levels_of_act) {
        temp <- data %>% filter(Activity == i) %>% select(MeanStd) %>% unlist()  
        res <- c(res, mean(temp))
        temp <- c()
    }
    res
}

#function to group all elements of vector by subject and calculate average 
calc_mean_subject <- function(data) {
    res <- c()
    temp <- c()
    levels_of_sub <- as.numeric(levels(factor(data$Subject)))
    for(i in levels_of_sub) {
        temp <- data %>% filter(Subject == i) %>% select(MeanStd) %>% unlist()
        res <- c(res, mean(temp))
        temp <- c()
    }
    res
}


#data frame with average measurement for each activity and subject
levels_sub <- as.numeric(levels(factor(har$Subject)))
average_har <- data.frame(matrix(nrow = nrow(activity_labels) + 
                                     length(levels_sub), ncol = 3))
names(average_har) <- c("Average", "Type", "NameOfMeasurement")
average_har$Average <- c(calc_mean_act(har), calc_mean_subject(har))
average_har$Type <- c(rep(1, nrow(activity_labels)), rep(2, length(levels_sub))) 
average_har$NameOfMeasurement <- c(activity_labels$V2, as.character(levels_sub))