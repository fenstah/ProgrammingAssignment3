rankstate=function(stateOutcome, num)
{        
    ##get the row given the rank
    num<-ifelse(num=="best",1,ifelse(num=="worst", nrow(stateOutcome),num))
    data<-NA  #c(NA,as.character(stateOutcome[1,2]))
    if(num<=nrow(stateOutcome))  data<-as.character(stateOutcome[num,1])  #data[1]<-as.character(stateOutcome[num,1])
    data
}