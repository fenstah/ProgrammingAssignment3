rankhospital<-function(state,outcome,num = "best") {
    ##validate that outcome can only be 'heart attack', 'heart failure', or 'pneumonia'
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!outcome %in% validOutcomes) stop("invalid outcome")
    
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ##Check that state and outcome are valid
    stateOutcome <- outcomeData[outcomeData$State==state,]
    if(nrow(stateOutcome) <= 0) stop("invalid state")
    
    outcomeColNums<-c(11,17,23)
    names(outcomeColNums)<-validOutcomes
    outcomeColumn <- outcomeColNums[outcome]    #set the column number for the outcome we want        
    
    ##Return hospital name in that state with the given rank 30-day death rate
    stateOutcome[,outcomeColumn] <-suppressWarnings(as.numeric(stateOutcome[,outcomeColumn]))                #make numeric
    omittedNAs<-stateOutcome[complete.cases(stateOutcome[,colnames(stateOutcome)]),]                         #remove NAs
    stateOutcomes<-omittedNAs[order(omittedNAs[,outcomeColumn], omittedNAs$Hospital.Name, decreasing=F),]    #get the ordered matrix by outcome and name 
    
    ##get the row given the rank
    if(num=="all") return (stateOutcomes)
    num<-ifelse(num=="best",1,ifelse(num=="worst", nrow(stateOutcomes),num))
    ifelse(num>nrow(stateOutcomes),NA,stateOutcomes[num,2])        
}