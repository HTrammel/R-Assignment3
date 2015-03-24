junque <- function(state, outcome, num = "best") {
    n <- list("hospital"=2, 
              "state"=7, 
              "heart attack"=11, 
              "heart failure"=17,
              "pneumonia"=23 )
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data[data == 'Not Available'] <- NA
    
    dfrm <- data.frame(matrix(0,nrow(data),5))
    
    for (i in seq_along(n)) {
        dfrm[i] <- data[ n[[i]] ]        
    }
    
    names(dfrm) <- c("Hospital","State","Heart Attack","Heart Failure","Pneumonia")
    return(dfrm)
}
