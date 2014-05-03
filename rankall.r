## Find the hospitals in every state with the given rank for an outcome
rankall <- function(outcome, num = "best", directory = "./hospital_compare_data") {
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
    
    # Create sorted vector of states and empty vector for hospital names
    states <- sort(unique(outcomeData$State))
    rankedHospitals <- character(length(states))
    
    # Determine ranked hospitcal for each state then store to 'rankedHospitals'
    for(i in seq_along(states)) {
        # Subset 'outcomeData' to rows with 'state'
        tempData <- subset(outcomeData, states[i] == State)
        
        # Limit and re-order 'tempData' to 'outcome', 'name'
        tempData <- tempData[, c(column, 2)]
        names(tempData) <- c("outcome", "name")
        
        # Remove NA 'outcome' rows
        tempData <- subset(tempData, !is.na(as.numeric(outcome)))
        
        # Determine number of measurements and save to 'numMax'
        numMax <- nrow(tempData)
        
        # Test 'num' for non-numeric and out-of-range values
        n <- if(is.numeric(num)) {
            if(num < 1 | num > numMax) {
                NA
            } else num
        } else if(tolower(num) == "best") {
            1
        } else if(tolower(num) == "worst") {
            numMax
        } else NA
        
        # Order by 'outcome' then 'name'
        tempData <- tempData[order(as.numeric(tempData[[1]]), tempData[[2]]), ]
        
        # Store hospital with rank 'n' to 'rankedHospitals'
        # Store NA if 'n' invalid
        rankedHospitals[i] <- if(is.na(n)) NA else tempData[n, 2]
    }
    
    # Create data frame 'results' combining hospital names with states
    # Named columns to comply with assignment
    results <- data.frame(rankedHospitals, states)
    names(results) <- c("hospital", "state")
    
    # Return 'results'
    results
}