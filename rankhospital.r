## Find the hospital in a state with the given rank for an outcome
rankhospital <- function(state, outcome, num = "best", directory = "./hospital_compare_data") {
    # Create list of outcome-column number pairs in
    # 'outcome-of-care_measures.csv'
    outcomeList <- list(c("heart attack", "heart failure", "pneumonia"),
                        c(11, 17, 23))
    names(outcomeList) <- c("measure", "column")
    
    # Match 'outcome' with column number; throw error if length == 0
    column <- outcomeList$column[outcomeList$measure == tolower(outcome)]
    if (length(column) == 0) stop("invalid outcome")
    
    # Read 'outcome-of-care-measures.csv' from 'directory'
    # Coerce all data into character class
    outcomeData <- read.csv(paste0(directory, "/outcome-of-care-measures.csv"),
                            colClass = "character")
    
    # Subset 'outcomeData' to rows with 'state'; throw error if nrows == 0
    outcomeData <- subset(outcomeData, toupper(state) == State)
    if(nrow(outcomeData) == 0) stop("invalid state")
    
    # Limit and re-order 'outcomeData' to 'outcome', 'name'
    outcomeData <- outcomeData[, c(column, 2)]
    names(outcomeData) <- c("outcome", "name")
    
    # Remove NA 'outcome' rows
    outcomeData <- subset(outcomeData, !is.na(as.numeric(outcome)))
    
    # Determine number of measurements and save to 'numMax'
    numMax <- nrow(outcomeData)
    
    # Test 'num' for non-numeric and out-of-range values
    num <- if(is.numeric(num)) {
        if(num < 1 | num > numMax) {
            return(NA)
        } else num
    } else if(tolower(num) == "best") {
        1
    } else if(tolower(num) == "worst") {
        numMax
    } else return(NA)
    
    # Order by 'outcome' then 'name' and return row 'num'
    outcomeData[order(as.numeric(outcomeData[[1]]), outcomeData[[2]]), 2][num]
}