best<-function(state,outcome) {
    ##validate that outcome can only be 'heart attack', 'heart failure', or 'pneumonia'
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!outcome %in% validOutcomes) stop("invalid outcome")
    
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ##Check that state and outcome are valid
    stateOutcome <- outcomeData[outcomeData$State==state,]
    numOutcomes<-nrow(stateOutcome)
    if(numOutcomes <= 0) stop("invalid state")
    
    outcomeColNums<-c(11,17,23)
    names(outcomeColNums)<-validOutcomes
    outcomeColumn <- outcomeColNums[outcome]    #set the column number for the outcome we want        
    
    ##Return hospital name in that state with the lowest 30-day death rate
    stateOutcomes<-stateOutcome[which.min(suppressWarnings(as.numeric(stateOutcome[,outcomeColumn]))),2]    ##convert the outcome column to numeric and find the minimum excluding NAs
    ifelse(length(stateOutcomes==1),stateOutcomes[1], stateOutcomes[which(order(stateOutcomes)==1)])
}