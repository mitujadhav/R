
#combining X_train and X_test
X_test<-read.table("D:/R/Coursera Assignment/Data Cleaning/UCI HAR Dataset/test/X_test.txt")
X_train<-read.table("D:/R/Coursera Assignment/Data Cleaning/UCI HAR Dataset/train/X_train.txt")
y<-rbind(X_train,X_test)
dim(y)


#combining Y_train and Y_test
y_train<-read.table("D:/R/Coursera Assignment/Data Cleaning/UCI HAR Dataset/train/y_train.txt")
y_test<-read.table("D:/R/Coursera Assignment/Data Cleaning/UCI HAR Dataset/test/y_test.txt")
z<-rbind(y_train,y_test)
dim(z)

#combining Subjects
X_subject<-read.table("D:/R/Coursera Assignment/Data Cleaning/UCI HAR Dataset/train/subject_train.txt")
y_subject<-read.table("D:/R/Coursera Assignment/Data Cleaning/UCI HAR Dataset/test/subject_test.txt")
s<-rbind(X_subject,y_subject)
dim(s)

#addind columns to train data
z<-cbind(z,s)
y<-cbind(y,z)
dim(y)
colnames(y)[562]="Activity"
colnames(y)[563]="Subject"
head(y,1)

#Adding values to table 
type=c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
for(i in 1:6)
{
  y$Activity<-gsub(i,type[i],y$Activity)
}
head(y,1)

#adding colnames to dataset
features<-read.table("D:/R/Coursera Assignment/Data Cleaning/UCI HAR Dataset/features.txt")
data<-gsub("-","",features$V2)
features$V2<-data

pattern<-"^t" #converting t to time
data<-gsub(pattern,"time",data)
features$V2<-data

pattern<-"^f" #converting f to frequency
data<-gsub(pattern,"frequency",data)
features$V2<-data

colnames(y) <- features$V2
colnames(y)[562]="Activity"
colnames(y)[563]="Subject"

# write into the file
write.csv(d,file="Final.CSV")
head(y,2)

m<-aggregate(y[, 1:561], list(y$Activity,y$Subject), mean)
dim(m)
head(m,1)

#Write average into a file
write.csv(m,file="avrage.CSV")







