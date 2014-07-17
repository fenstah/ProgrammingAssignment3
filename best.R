source('C:/Repos/ProgrammingAssignment3/cleanedData.R')

best<-function(state,outcome) {
    ##validate that outcome can only be 'heart attack', 'heart failure', or 'pneumonia'
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!outcome %in% validOutcomes) stop("invalid outcome")
    
    ## Read outcome data
    outcomeData <- cleanAndCacheOutcomeData(read.csv("outcome-of-care-measures.csv", header=T, na.strings="Not Available"))
    orderedData <- outcomeData$orderbyOutcome(which(validOutcomes==outcome)+2)
    
    ##Check that state and outcome are valid
    stateOutcome <- orderedData[orderedData$state==state,]
    if(nrow(stateOutcome) <= 0) stop("invalid state")
    as.character(stateOutcome[1,1])
}