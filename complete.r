complete<-function(directory,id=1:332)
{
  setwd(directory)
  df <- data.frame(x = numeric(length(id)), y = numeric(length(id)))
  for(i in seq(from=1,to=length(id),by=1))
  {
    if(id[i]<10)
    {
      data<-read.csv(paste0("00",id[i],".csv"))
    }
    else if(id[i]<100)
    {
      data<-read.csv(paste0("0",id[i],".csv"))
    }
    else
    {
      data<-read.csv(paste0(id[i],".csv"))
    }
    printID<-data[1,"ID"]
    count<-data[!is.na(data$"sulfate"),]
    df$x[i] <- printID
    df$y[i] <- NROW(count)
  }
  df
}
