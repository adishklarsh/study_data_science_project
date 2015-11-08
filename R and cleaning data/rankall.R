
# takes two arguments: an outcome name (outcome) and a hospital ranking (num).
# The function reads the outcome-of-care-measures.csv file and returns a 2-column
# data frame containing the hospital in each state that has the ranking specified
# in num. The function returns a value for every state (some may be NA). The first column in the
# data frame is named hospital, which contains the hospital name, and the second
# column is named state, which contains the 2-character abbreviation for the
# state name. Hospitals that do not have data on a particular outcome should be
# excluded from the set of hospitals when deciding the rankings.

rankall <- function(outcome, num = "best") {
    
    # outcome strings and matching column numbers in data 
    outcomeStrings = c("heart attack","heart failure","pneumonia")
    outcomeColNums = c(11,17,23)
    
    # check valid outcome  
    if (sum(outcomeStrings == outcome) != 1) stop("invalid outcome")

    outcomeColNum = outcomeColNums[outcomeStrings == outcome]
    
    outcomeData = read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomeData[,outcomeColNum] = suppressWarnings(as.numeric(outcomeData[,outcomeColNum]))
    
    # clean na from data in State, Hospital Name, and outcome column
    outcomeData = outcomeData[complete.cases(outcomeData[,c("Hospital.Name","State")]),]
#     outcomeData = outcomeData[!is.na(outcomeData[,outcomeColNum]),]
    
    # factor by state 
    statesFactor = factor(outcomeData$State)
    splitByState = split(outcomeData,statesFactor)
    
    # order list factored by state according to rank first and then by hospital name
    orderedSplitByState = lapply(splitByState, function (table) table[order(table[,outcomeColNum], table[,"Hospital.Name"], na.last=NA),])

    # apply max  
    if (num == "worst") {
        # apply a function that returns the Hospital Name giving the maximum value in a list of vectors ordered by rank and name and split by state
        rankHospitalsSplitByState = lapply(orderedSplitByState,function (ordered) ordered[which.max(ordered[,outcomeColNum]),"Hospital.Name"])
    }
    # check valid num and assign rank. 
    else {
        if (num == "best") num = 1
        else { 
            num = as.numeric(num)
            if (is.na(num) || num != round(num) || num < 1) stop("invalid num")
        }
        # apply a function that returns the Hospital Name with outcome rank num in a list of vectors ordered by rank and name and split by state
        rankHospitalsSplitByState = lapply(orderedSplitByState,function (ordered) if (nrow(ordered) < num) return(NA) else return(ordered[num,"Hospital.Name"]))
    } 
    
    # data frame with first column  ranked list of hospitals and second column as states
    data.frame(hospital = matrix(unlist(rankHospitalsSplitByState)),state = levels(statesFactor), row.names=levels(statesFactor))

}