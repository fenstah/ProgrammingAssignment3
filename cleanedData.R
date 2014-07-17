## The cleanAndCacheOutcomeData function creates an object that can cache the outcome data as well as clean it for 
## only the columns we care about and quickly return an ordered version of the data given the outcome variable desired
cleanAndCacheOutcomeData <- function(x = data.frame()) {
        cleanedData <- ifelse(ncol(x)>=23, x[,c(2,7,11,17,23)], NULL)     #initialize cleanedData variable to NULL
        set <- function(y) {                                              #public set method for initializing variable
            x <<- y
            cleanedData <- x[,c(2,7,11,17,23)]
        }
        get <- function() x                         #public get that returns the original outcomedata
        getCleanedData <- function() {              #public getCleanedData that returns the cleanedData
            if (!is.null(x)) x[,c(2,7,11,17,23)]
        }
        orderbyOutcome <- function(outcomeNum) {
            data <- getCleanedData()
            data<-data[order(data$State, data[,outcomeNum], data$Hospital.Name, decreasing=F),]    #get the ordered matrix by state, outcome and name 
            data<-data[,c(1:2,outcomeNum)]            
            data<-data[complete.cases(data[,3]),]                         #remove NAs   
            colnames(data) <- c("hospital", "state", "outcome")
            data
        }
        orderedByHeartAttack <- function() {
            orderbyOutcome(3)
        }
        orderedByHeartFailure <- function() {
            orderbyOutcome(4)
        }
        orderedByPneumonia <- function() {
            orderbyOutcome(5)
        }
        list(set=set,get=get, getCleanedData=getCleanedData,
             orderbyOutcome=orderbyOutcome,
             orderedByHeartAttack=orderedByHeartAttack,
             orderedByHeartFailure=orderedByHeartFailure,
             orderedByPneumonia=orderedByPneumonia) 
}