# ------------------------------------------------------------------------------
# readData(filepath): Reads data from a CSV file using fread, and subsets
# with dplyr's filter() function.
#
# The entire data set is loaded into RAM, then subsetted using dplyr.
#
# Argument:
# filepath: Path name of the data file to read.
# ------------------------------------------------------------------------------
readData <- function(filepath) {        
    require(data.table)
    require(dplyr)
    
    if (!file.exists(filepath)) {
        stop(paste('The filepath', paste("'", filepath, "'", sep=''), 'does not exist!'))
    }
    
    message("Reading", filepath)
    execTime <- system.time({
        data <- fread(filepath, 
                      sep=';', 
                      na.strings="?", 
                      colClasses=rep('character', 9))
    })
    dims <- dim(data)
    message("Done reading data: ", dims[1], " obs. of ", dims[2], " variables")    
    print(execTime)
    
    # Cleanup and return the data
    # Note: Date is in format dd/mm/yyyy 
    data <- data %>% 
            filter(Date == "1/2/2007" | Date == "2/2/2007") %>%
            fixData()        
    
    dims <- dim(data)
    message("Filtered data: ", dims[1], " obs. of ", dims[2], " variables") 
    
    data
}

# ------------------------------------------------------------------------------
# selectData(filepath): Reads and filters data from a CSV file using sqldf.
#
# Only the filtered data is loaded into RAM.
# 
# However, comparing the execution time of this function vs. selectData(), which
# uses fread(), the former is significantly longer. sqldf creates a database
# on disk before querying the data, hence the performance hit. It still a good
# tool for very large dataset. However, the household power dataset has 
# a small memory footprint (approx. 143MB- use object.size()) for today's 
# computers, and using fread(), then subsetting within RAM are adequate 
# techniques.
#
# Argument:
# filepath: Path name of the data file to read.
# ------------------------------------------------------------------------------
selectData <- function(filepath) {
    require(sqldf)
    require(dplyr)
    
    if (!file.exists(filepath)) {
        stop(paste('The filepath', paste("'", filepath, "'", sep=''), 'does not exist!'))
    }

    
    filter <- "Date = '1/2/2007' or Date = '2/2/2007'"
    message(paste('Reading', filepath, 'and filtering with', filter))
    sqlExpr <- paste("select * from file where", filter)
    execTime <- system.time({
        data <- read.csv.sql(filepath, 
                             sql = sqlExpr, 
                             sep=';', 
                             header = TRUE)
    })
    dims <- dim(data)
    message("Done reading data: ", dims[1], " obs. of ", dims[2], " variables")
    print(execTime)
    
    # Cleanup and return the data
    data %>%         
        fixData()
}

# ------------------------------------------------------------------------------
# fixData(df): Fixes the data set.
#
# - Creates a date and time column.
# - Coerces the observations as numeric.
#
# Notes: Alternative ways of creating the DateTime column:
# a. strptime, which returns a POSIXlt date. This  produces a warning!
# DateTime = strptime(paste(Date, Time), format = '%d/%m/%Y %H:%M:%S')
# 
# b. cast the strptime output to POSIXct to eliminate the warning:
# DateTime = as.POSIXct(strptime(paste(Date, Time), format = '%d/%m/%Y %H:%M:%S'))
#
# c. as.POSIXct
# DateTime = as.POSIXct(paste(Date, Time), format = '%d/%m/%Y %H:%M:%S'))
#
# Argument:
# df: Data frame to cleanup.
# ------------------------------------------------------------------------------
fixData <- function(df) {
    require(lubridate)
        
    df %>%          
        mutate(DateTime = dmy_hms(paste(Date, Time))) %>%
        transform(Global_active_power = as.numeric(Global_active_power)) %>%
        transform(Global_reactive_power = as.numeric(Global_reactive_power)) %>%
        transform(Voltage = as.numeric(Voltage)) %>%
        transform(Global_intensity = as.numeric(Global_intensity)) %>%
        transform(Sub_metering_1 = as.numeric(Sub_metering_1)) %>%
        transform(Sub_metering_2 = as.numeric(Sub_metering_2)) %>%
        transform(Sub_metering_3 = as.numeric(Sub_metering_3))    
}