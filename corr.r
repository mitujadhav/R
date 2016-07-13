corr<-function(directory,threshold=0)
{
  setwd(directory)
  files<-list.files("C:/Users/madhumitaj/Desktop/R/Coursera/rprog-data-specdata/specdata",full.names =TRUE)
 
  for(i in seq(from=1,to=length(files),by=1))
  {
    data<-read.csv(files[i])
    printID<-data[1,"ID"]
    count<-data[!is.na(data$"sulfate"),]
    count <- NROW(count)
    
    if(count<threshold)
    {
      corre<-0;
      print("0")
    }
    if(count>=threshold)
    {
      x<-data[!is.na(data$"sulfate"),]
      y<-data[!is.na(data$"sulfate"),]
      corre<-cor(x$"sulfate",y$"nitrate")
      print(corre)
    }
  }
}