source('C:/Repos/ProgrammingAssignment3/cleanedData.R')

rankall<-function(outcome,num = "best") {
    ##validate that outcome can only be 'heart attack', 'heart failure', or 'pneumonia'
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!outcome %in% validOutcomes) stop("invalid outcome")
    
    ## Read outcome data and get it by order of outcome
    outcomeData <- cleanAndCacheOutcomeData(read.csv("outcome-of-care-measures.csv", header=T, na.strings="Not Available"))
    orderedData <- outcomeData$orderbyOutcome(which(validOutcomes==outcome)+2)
    
    ##split by state
    splitOutcomes<-split(orderedData, orderedData$state)
    
    #run rankstate for each state 
    stateResults<-sapply(splitOutcomes, rankstate,num)
    output <- data.frame(stateResults, names(stateResults))
    colnames(output) <- c("hospital", "state")
    rownames(output)<-names(stateResults)
    output
}