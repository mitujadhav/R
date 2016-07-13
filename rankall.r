rankall<-function(outcome,rank="best")
{
  data<- read.csv("outcome-of-care-measures.csv")
  df<-levels(data[,7])
  ranked <- data.frame(x = character(length(df)), y = character(length(df)), stringsAsFactors = FALSE)
  
  for(i in seq(from=1,to=length(df),by=1))
  {
    if(outcome=="heart attack")
    {
      subdata<-subset(data[, c(2,7, 11)])
      subsubdata<-subdata[which(subdata$State==df[i]),]
      subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
      NotnaData<-na.omit(subsubdata)
      if(is.numeric(rank))
      {
        hospitalname<-NotnaData[order(NotnaData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,NotnaData$Hospital.Name)[rank],1]
        ranked$x[i]<-as.character(hospitalname)
      }
      else if(rank=="worst")
      {
        minval<-max(subsubdata[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"], na.rm = TRUE)
        hospitalname<-subsubdata[which(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==minval),1]
        ranked$x[i]<-as.character(hospitalname)
      }
      else
      {
        minval<-min(subsubdata[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"], na.rm = TRUE)
        hospitalname<-subsubdata[which(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==minval),1]
        ranked$x[i]<-as.character(hospitalname)
      }
      ranked$y[i] <-df[i]
    }
    
    else if(outcome=="heart failure")
    {
      subdata<-subset(data[, c(2,7, 17)])
      subsubdata<-subdata[which(subdata$State==df[i]),]
      subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<- as.numeric(as.character(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      NotnaData<-na.omit(subsubdata)
      if(is.numeric(rank))
      {
        hospitalname<-NotnaData[order(NotnaData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,NotnaData$Hospital.Name)[rank],1]
        ranked$x[i]<-as.character(hospitalname)
      }
      else if(rank=="worst")
      {
        minval<-max(subsubdata[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"], na.rm = TRUE)
        hospitalname<-subsubdata[which(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==minval),1]
        ranked$x[i]<-as.character(hospitalname)
      }
      else
      {
        minval<-min(subsubdata[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"], na.rm = TRUE)
        hospitalname<-subsubdata[which(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==minval),1]
        ranked$x[i]<-as.character(hospitalname)
      }
      ranked$y[i] <-df[i]
      
    }
    
    else if(outcome=="pneumonia")
    {
      subdata<-subset(data[, c(2,7, 23)])
      subsubdata<-subdata[which(subdata$State==df[i]),]
      subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      NotnaData<-na.omit(subsubdata)
      
      if(is.numeric(rank))
      {
        hospitalname<-NotnaData[order(NotnaData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,NotnaData$Hospital.Name)[rank],1]
        ranked$x[i]<-as.character(hospitalname)
      }
      else if(rank=="worst")
      {
        minval<-max(subsubdata[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"], na.rm = TRUE)
        hospitalname<-subsubdata[which(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==minval),1]
        ranked$x[i]<-as.character(hospitalname)
      }
      else
      {
        minval<-max(subsubdata[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"], na.rm = TRUE)
        hospitalname<-subsubdata[which(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==minval),1]
        ranked$x[i]<-as.character(hospitalname)
      }
      ranked$y[i] <-df[i]
    }
  }
  ranked
}