# ------------------------------------------------------------------------------
# plot2(filepath, data = NULL)
#
# Draws plot #2: Scatter plot of Active Global Power vs. Date and time
# for Feb 1th and 2nd 2007.
#
# Notes:
# 1. Requires readData() from readFile.R.
# 2.NA observations are filtered out of the data frame before plotting.
#
# Arguments:
# filepath: Path name of the data file to load.
# data (optional, NULL by default): A loaded data frame. When passed, the 
# filepath argument is ignored.
# ------------------------------------------------------------------------------
plot2 <- function(filepath, data = NULL) {
    
    message(paste('filepath:', filepath))
    if (!is.null(data)) {
        message('data: Provided')    
    }    
    
    # Read the data if not provided as an argument
    df <- data
    if (is.null(df)) {
        source('./readFile.R')
        df <- readData(filepath)
    }  
    
    # Filter the NA on the observations if any
    df <- filter(df, !is.na(Global_active_power))
    
    # Open the PNG device: 480x480 pixels by default.
    png(file = "plot2.png") 
    
    with(df, plot(DateTime, 
                  Global_active_power, 
                  type = 'l', 
                  xlab = '', 
                  ylab = 'Global Active Power (kilowatts)'))
    # Close the PNG device 
    dev.off()   
    
    # For convenience to caller, return the data frame
    df
}