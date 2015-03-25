best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", 
                     colClasses = "character", 
                     na.strings = c("NA","Not Available"))
    
    ## grab statelist
    ste <- unique ( data$State)
    
    ## grab names for outcome measures [NOTE: this is not flexible]
    o_names <- names(data)
    target <- matrix(o_names[c(11,17,23)],nrow=3,ncol=1)
    rownames(target) <- c("heart attack","heart failure","pneumonia")
    colnames(target) <- "Measure"
    
    ## Check that state and outcome are valid
    if (sum(ste == toupper(state)) == 0) stop ("invalid state") 
    if (sum(rownames(target) == tolower(outcome)) == 0) stop("invalid outcome")
    
    ## pull desired state's data
    state_data <- data[data[,"State"] == toupper(state),]
    ## get all hospitals for state
    hosp <- state_data["Hospital.Name"]
    ## get all desired outcomes for state and convert 'Not Available' to NA
    oc <- state_data[,target[outcome,]]
    
    ## Put in data frame for analysis
    df <- data.frame(hosp, as.numeric(oc))
    names(df) <- c("Hospital","Outcome")
    good <- na.omit(df)
    best <- good[good[,"Outcome"] == min(good$Outcome),]
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    o <- best$Hospital
    return (o)
}