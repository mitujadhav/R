pollutantmean<-function(directory,pollutant,id=111:332)
{
  total=0;
  setwd(directory)
  for(i in seq(from=1, to=length(id), by=1))
  {
    if(id[i]<10)
    {
      data<-read.csv(paste0("00",id[i],".csv"))
    }
    else if(id[i]<100 && id[i]>=10)
    {
      data<-read.csv(paste0("0",id[i],".csv"))
    }
    else
    {
      data<-read.csv(paste0(id[i],".csv"))
    }
    meanval<-mean.default(data[,pollutant], na.rm = TRUE)
    total=total+meanval
  }
  total<-total/length(id)
  print(paste("Total Mean of",pollutant,"=",total))
}