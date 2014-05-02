## Find the hospital in a state with the lowest (i.e. best) outcome
best <- function(state, outcome, directory = "./hospital_compare_data") {
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
    
    # Determine minimum (i.e. best) measure
    minMeasure <- min(as.numeric(outcomeData[, column]), na.rm = TRUE)
    
    # Subset 'outcomeData' to rows with outcome 'minMeasure'
    outcomeData <- subset(outcomeData, as.numeric(outcomeData[, column])
                          == minMeasure)
    
    # Return first entry in sorted "Hospital.Name' vector
    sort(outcomeData$Hospital.Name)[1]
}