rankhospital<-function(state,outcome,rank)
{
  data<- read.csv("outcome-of-care-measures.csv")
  if(outcome=="heart attack")
  {
    subdata<-subset(data[, c(2,7, 11)])
    subsubdata<-subdata[which(subdata$State==state),]
    subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    NotnaData<-na.omit(subsubdata)
    if(is.numeric(rank))
    {
      NotnaData[order(NotnaData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,NotnaData$Hospital.Name)[rank],1]
    }
    else if(rank=="worst")
    {
      minval<-max(subsubdata[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"], na.rm = TRUE)
      hospitalname<-subsubdata[which(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==minval),1]
      hospitalname
    }
  }
  else if(outcome=="heart failure")
  {
    subdata<-subset(data[, c(2,7, 17)])
    subsubdata<-subdata[which(subdata$State==state),]
    subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(as.character(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    NotnaData<-na.omit(subsubdata)
    if(is.numeric(rank))
    {
      NotnaData[order(NotnaData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,NotnaData$Hospital.Name)[rank],1]
    }
    else if(rank=="worst")
    {
      minval<-min(subsubdata[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"], na.rm = TRUE)
      hospitalname<-subsubdata[which(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==minval),1]
      hospitalname
    }
  }
  else if(outcome=="pneumonia")
  {
    subdata<-subset(data[, c(2,7, 23)])
    subsubdata<-subdata[which(subdata$State==state),]
    subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    NotnaData<-na.omit(subsubdata)
    if(is.numeric(rank))
    {
      NotnaData[order(NotnaData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,NotnaData$Hospital.Name)[rank],1]
    }
    else if(rank=="worst")
    {
      minval<-min(subsubdata[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"], na.rm = TRUE)
      hospitalname<-subsubdata[which(subsubdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==minval),1]
      hospitalname
    }
  }
  else
  {
    print("Invalid Outcome ")
  }
}