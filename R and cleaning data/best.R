# setwd("Coursera R Course\\ex3\\")


# This function reads the outcome-of-care-measures.csv file and returns a 
# character vector with the name of the hospital that has the best (i.e. lowest)
# 30-day mortality for the specified outcome in that state. The hospital name is
# the name provided in the Hospital.Name variable. The outcomes can be one of 
# "heart attack", "heart failure", or "pneumonia". Hospitals that do not have 
# data on a particular outcome should be excluded from the set of hospitals when
# deciding the rankings.

best <- function(state, outcome) {
    
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

    minValue = min(outcomeData[isRowInState, outcomeColNum],na.rm=TRUE)
    isRowInMinValue = isRowInState
    isRowInMinValue[isRowInState] = outcomeData[isRowInState, outcomeColNum] == minValue
    minRowFirstLex = min(outcomeData[isRowInMinValue,"Hospital.Name"],na.rm=TRUE)
    minRowFirstLex
}