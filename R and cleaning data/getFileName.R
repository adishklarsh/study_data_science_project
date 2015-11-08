
## returns the file name associated with a monitor id

getFileName = function(monitorId) {
    MONITOR_ID_WIDTH = 3
    PAD_0 = TRUE
    
    if (PAD_0) 
        monitorStr = formatC(monitorId,width=MONITOR_ID_WIDTH,flag="0")
    else     
        monitorStr = monitorId
    paste("\\",monitorStr,".csv",sep="")
}