
# Part 2
## returns a data frame of monitor ids and complete rows across monitors in id from data in directory

complete = function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    ids = as.vector(id)
    monitorNum = length(ids)
    nobs = vector("numeric", length=monitorNum)
    
    for (iii in 1:monitorNum) {
        fileName = paste(directory,getFileName(ids[iii]),sep="")
        monitor_table = read.csv(fileName)
        nobs[iii] = sum(complete.cases(monitor_table))
    }
    data.frame(id = ids, nobs = nobs)
}
