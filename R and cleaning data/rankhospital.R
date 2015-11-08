# takes three arguments: the 2-character abbreviated name of a state (state), an
# outcome (outcome), and the ranking of a hospital in that state for that
# outcome (num). The function reads the outcome-of-care-measures.csv file and
# returns a character vector with the name of the hospital that has the ranking
# specified by the num argument.

rankhospital <- function(state, outcome, num = "best") {
    
    # outcome strings and matching column numbers in data 
    outcomeStrings = c("heart attack","heart failure","pneumonia")
    outcomeColNums = c(11,17,23)
    
    # check valid outcome and state
    if (sum(outcomeStrings == outcome) != 1) stop("invalid outcome")
    if (sum(state.abb == state) != 1) stop("invalid state")
    
    outcomeColNum = outcomeColNums[outcomeStrings == outcome]
    
    outcomeData = read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomeData[, outcomeColNum] = suppressWarnings(as.numeric(outcomeData[, outcomeColNum]))
    
    # clean na from data in State, Hospital Name, and outcome column
    outcomeData = outcomeData[complete.cases(outcomeData[,c("Hospital.Name","State")]),]
    outcomeData = outcomeData[!is.na(outcomeData[,outcomeColNum]),]
    
    # filter by state and work only on filtered rows
    isRowInState = outcomeData[,"State"] == state
    maxRank = sum(isRowInState)
    
    # check valid num and assign rank. 
    if (num == "best") num = 1
    else if (num == "worst") num = maxRank
    else {
        num = as.numeric(num)
        if (is.na(num)) stop("invalid num")
    }
    if (num > maxRank) return(NA)
    
    # order hospitals in state by rank first and then by hospital name and return hospital name in rank num
    orderedByRankAndNameInState = order(outcomeData[isRowInState,outcomeColNum],outcomeData[isRowInState,"Hospital.Name"],na.last=NA)
    (outcomeData[isRowInState,"Hospital.Name"])[orderedByRankAndNameInState[num]]
}