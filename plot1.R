# ------------------------------------------------------------------------------
# plot1(filepath, data = NULL)
#
# Draws plot #1: Histogram of the Global Active Power for Feb 1th and 2nd 2007.
#
# Note: NA observations are filtered out of the data frame before plotting.
#
# Arguments:
# filepath: Path name of the data file to load.
# data (optional, NULL by default): A loaded data frame. When passed, the 
# filepath argument is ignored.
# ------------------------------------------------------------------------------
plot1 <- function(filepath, data = NULL) {
    
    message(paste('filepath:', filepath))
    if (!is.null(data)) {
        message('data: Provided')    
    }
    
    # Read the data if not provided as an argument
    df <- data
    if (is.null(df)) {
        df <- readData(filepath)
    }
    
    # Filter the NA on the observations if any
    df <- filter(df, !is.na(Global_active_power))
    
    # Open the PNG device: 480x480 pixels by default.
    png(file = "plot1.png")
    
    # Plot!
    with(df, hist(Global_active_power, 
                    col = 'Red', 
                    main = 'Global Active Power', 
                    xlab = 'Global Active Power (kilowatts)'))
    
    # Close the PNG device
    dev.off()
    
    # For convenience to caller, return the data frame
    df    
}