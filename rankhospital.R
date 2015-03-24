rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## grab statelist
    ste <- unique ( outcome_data$State)
    
    ## grab names for outcome measures [NOTE: this is not flexible]
    o_names <- names(outcome_data)
    target <- matrix(o_names[c(11,17,23)],nrow=3,ncol=1)
    rownames(target) <- c("heart attack","heart failure","pneumonia")
    colnames(target) <- "Measure"
    
    ## Check that state and outcome are valid
    if (sum(ste == toupper(state)) == 0) stop ("invalid state") 
    if (sum(rownames(target) == tolower(outcome)) == 0) stop("invalid outcome")
    
    ## pull desired state's data
    state_data <- outcome_data[outcome_data[,"State"] == toupper(state),]
    
    ## get all hospitals for state
    hosp <- state_data["Hospital.Name"]
    
    
    ## get all desired outcomes for state and convert 'Not Available' to NA
    oc <- state_data[,target[outcome,]]
    oc[oc == 'Not Available'] <- NA
    
    ## Put in data frame for analysis
    df <- data.frame(hosp, as.numeric(oc))
    colnames(df) <- c("Hospital","Outcome")
    good <- na.omit(df)
    
    ord <- good [ order (good[,"Outcome"],good[,"Hospital"]),]
    rownames(ord) <- c(1:nrow(ord))
    
    if (!is.numeric(num) & num == "best") {
        h <- ord[1,"Hospital"]
    } else if (!is.numeric(num) & num == "worst") {
        h <- ord[nrow(ord),"Hospital"]
    } else if (is.numeric(num) & num > nrow(ord)) { 
        return (NA)  # if gt number of hospitals return NA
    } else {
        h <- ord[num,"Hospital"]
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    return (h)
    
    
}
