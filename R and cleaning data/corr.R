
# Part 3
## returns vector of correlations of two pollutant values across monitors where the number of complete cases is 
## larger than threshold from data in directory

corr = function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    monitorNum = 332
    meetsThreshold = vector("logical", length=monitorNum)
    cors = vector("numeric",length=monitorNum)

    for (iii in 1:monitorNum) {
        fileName = paste(directory,getFileName(iii),sep="")
        monitorTable = read.csv(fileName)
        isCompleteRows = complete.cases(monitorTable)
        meetsThreshold[iii] = sum(isCompleteRows)>threshold
        completeMonitorTable = monitorTable[isCompleteRows,]
        if (meetsThreshold[iii] == TRUE) cors[iii] = cor(completeMonitorTable$sulfate,completeMonitorTable$nitrate)
    }
    cors[meetsThreshold]    
}