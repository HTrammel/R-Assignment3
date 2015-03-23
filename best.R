best <- function(state, outcome) {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ste <- unique ( outcome_data$State)
    sl <- NULL
    
    outcome_list <- function(o) {
        onames <- names ( o )
        for (i in seq_along(onames)) {
            tmp <- strsplit (onames[i], "[.]")
            for (j in seq_along(tmp)) {
                a <- tmp[j]
                if (a == "from") {
                    t2 <- tmp[j+1:length(tmp)]
                    sl <<- rbind(sl, t2 )
                } else {
                    next
                }
            }
        }
        return (sl)
    }
    ## Check that state and outcome are valid
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    o <- outcome_list(outcome_data)
    return(o)
}