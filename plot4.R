# ------------------------------------------------------------------------------
# plot4(filepath, data = NULL)
#
# Draws plot #4: 4 plots organized in a 2 rows x 2 cols layout.
# All plots are for Feb 1th and 2nd 2007.
# Row 1, Col 1: Scatter plot of Global Active Power vs. Date and time.
# Row 1, Col 2: Scatter plot of Voltage vs. Date and time.
# Row 2, Col 2: Scatter plot of Energy Sub-metering stations vs. Date and time
# Row 2, Col 2: Scatter plot of Global Re-Active Power vs. Date and time.
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
plot4 <- function(filepath, data = NULL) {
    
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
    png(file = "plot4.png")
    
    par(mfrow = c(2, 2))
    
    # Global Active Power over time
    with(filter(df, !is.na(Global_active_power)), 
         plot(DateTime, Global_active_power, type = 'l', xlab = '', ylab = 'Global Active Power'))
    
    # Voltage over time
    with(filter(df, !is.na(Voltage)),
         plot(DateTime, Voltage, type = 'l', xlab = 'datetime', ylab = 'Voltage'))

    # Sub-metering over time
    with(df, 
         plot(DateTime, Sub_metering_1, xlab='', ylab = 'Energy Sub Metering', type = "n"))
    with(subset(df, !is.na(Sub_metering_1)), 
         points(DateTime, Sub_metering_1, type = 'l'))
    with(subset(df, !is.na(Sub_metering_2)), 
         points(DateTime, Sub_metering_2, type = 'l', col = "Red"))
    with(subset(df, !is.na(Sub_metering_3)), 
         points(DateTime, Sub_metering_3, type = 'l', col = "Blue"))
    legend("topright",
           lty = c(1, 1, 1),
           col = c("black", "red", "blue"),
           bty = 'n',
           legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))

    # Global reactive power
    with(filter(df, !is.na(Global_reactive_power)),
         plot(DateTime, Global_reactive_power, type = 'l', xlab = 'datetime'))
    
    # Close the PNG device
    dev.off()
    
    # For convenience to caller, return the data frame
    df
}