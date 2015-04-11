# ------------------------------------------------------------------------------
# plot3(filepath, data = NULL)
#
# Draws plot #3: Scatter plots of Energy Sub-metering vs. Date and time
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
plot3 <- function(filepath, data = NULL) {
    
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
    
    # Open the PNG device: 480x480 pixels by default.
    png(file = "plot3.png")
    with(df, plot(DateTime, Sub_metering_1, xlab='', ylab = 'Energy Sub Metering', type = "n"))
    with(subset(df, !is.na(Sub_metering_1)), points(DateTime, Sub_metering_1, type = 'l'))
    with(subset(df, !is.na(Sub_metering_2)), points(DateTime, Sub_metering_2, type = 'l', col = "Red"))
    with(subset(df, !is.na(Sub_metering_3)), points(DateTime, Sub_metering_3, type = 'l', col = "Blue"))
    legend("topright",
           lty = c(1, 1, 1),
           col = c("black", "red", "blue"),
           legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))
    
    # Close the PNG device
    dev.off()
    
    # For convenience to caller, return the data frame
    df
}