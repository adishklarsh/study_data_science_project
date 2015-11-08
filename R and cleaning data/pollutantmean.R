# Part 1
## returns the weighted means across of pollutant across monitors in id from data in directory

pollutantmean = function(directory, pollutant, id = 1:332) {
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    # "D:\\code\\R\\Coursera R Course\\specdata"
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    ids = as.vector(id)
    monitorNum = length(ids)
    means = vector("numeric",length=monitorNum)
    weights = vector("numeric",length=monitorNum)
    
    for (iii in 1:monitorNum) {
        fileName = paste(directory,getFileName(ids[iii]),sep="")
        monitor_table = read.csv(fileName)
        means[iii] = mean(monitor_table[[pollutant]],na.rm=TRUE)
        weights[iii] = sum(!is.na(monitor_table[[pollutant]]))
    }
    weighted.mean(means,weights,na.rm=TRUE)
}