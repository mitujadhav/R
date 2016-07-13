#Question 1st

    #combining X_train and X_test
    X_test<-read.table("C:/Users/madhumitaj/Desktop/R/Coursera Assignment/Data Cleaning/UCI HAR Dataset/test/X_test.txt")
    X_train<-read.table("C:/Users/madhumitaj/Desktop/R/Coursera Assignment/Data Cleaning/UCI HAR Dataset/train/X_train.txt")
    y<-rbind(X_train,X_test)
    dim(y)
    head(y,2)
    
    #combining Y_train and Y_test
    y_train<-read.table("C:/Users/madhumitaj/Desktop/R/Coursera Assignment/Data Cleaning/UCI HAR Dataset/train/y_train.txt")
    y_test<-read.table("C:/Users/madhumitaj/Desktop/R/Coursera Assignment/Data Cleaning/UCI HAR Dataset/test/y_test.txt")
    
    type=c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
    
    for(i in 1:6)
    {
      y_train<-gsub(i,type[i],y_train)
      y_test<-gsub(i,type[i],y_test)
    }
    
    w<-rbind(y_train,y_test)
   # z<-as.data.frame(z)
    dim(w)
    d<-cbind(y,w) 
    dim(d)
    head(d,2)
    
    X_subject<-read.table("C:/Users/madhumitaj/Desktop/R/Coursera Assignment/Data Cleaning/UCI HAR Dataset/train/subject_train.txt")
    y_subject<-read.table("C:/Users/madhumitaj/Desktop/R/Coursera Assignment/Data Cleaning/UCI HAR Dataset/test/subject_test.txt")
    s<-rbind(X_subject,y_subject)
    
    #Combining Columns of X and Y and Subjects
   
    d<-cbind(d,s)

    #removing - and replace t and f with time and frequency 
    features<-read.table("C:/Users/madhumitaj/Desktop/R/Coursera Assignment/Data Cleaning/UCI HAR Dataset/features.txt")
    data<-gsub("-","",features$V2)
    features$V2<-data
    
    pattern<-"^t" #converting t to time
    data<-gsub(pattern,"time",data)
    features$V2<-data
    
    pattern<-"^f" #converting f to frequency
    data<-gsub(pattern,"frequency",data)
    features$V2<-data
    
    
    colnames(d) <- features$V2
    colnames(d)[562] <- "activity"
    colnames(d)[563] <- "subject"

  meanf<-grep("mean()",features$V2,value = TRUE,fixed = TRUE)
  meanf2<-grep("std()",features$V2,value = TRUE,fixed = TRUE)
  x<-rbind(meanf,meanf2)

  write.csv(d,file="total.CSV")
