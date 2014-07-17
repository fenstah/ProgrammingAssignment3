source('C:/Repos/ProgrammingAssignment3/cleanedData.R')

rankhospital<-function(state,outcome,num = "best") {
    ##validate that outcome can only be 'heart attack', 'heart failure', or 'pneumonia'
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!outcome %in% validOutcomes) stop("invalid outcome")
    
    ## Read outcome data
    outcomeData <- cleanAndCacheOutcomeData(read.csv("outcome-of-care-measures.csv", header=T, na.strings="Not Available"))
    orderedData <- outcomeData$orderbyOutcome(which(validOutcomes==outcome)+2)
        
    ##Check that state and outcome are valid
    stateOutcome <- orderedData[orderedData$state==state,]
    if(nrow(stateOutcome) <= 0) stop("invalid state")
    
    ##get the row given the rank
    if(num=="all") return (stateOutcome)
    num<-ifelse(num=="best",1,ifelse(num=="worst", nrow(stateOutcome),num))
    ifelse(num>nrow(stateOutcome),NA,as.character(stateOutcome[num,1]))       
}