junque <- function(outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", 
                     colClasses = "character", 
                     na.strings = c("NA","Not Available"))
    
    dfrm <- data.frame(matrix(0,nrow(data),5))
    
    n <- list("hospital"=2, 
              "state"=7, 
              "heart attack"=11, 
              "heart failure"=17,
              "pneumonia"=23 )
    
    oc <- tolower(outcome)
    if (is.null (unlist (n[oc])) == TRUE) stop("invalid outcome")
    
    for (i in seq_along(n)) {
        dfrm[i] <- data[ n[[i]] ]        
    }
    
    names(dfrm) <- c("hospital","state","heart attack","heart failure","pneumonia")
    df$'heart attack' <- as.numeric(df$'heart attack')
    df$'heart failure' <- as.numeric(df$'heart failure')
    df$'pneumonia' <- as.numeric(df$'pneumonia')
    
    #ste <- sort( unique ( df$state) )
    ste <- "WY"
    
    rslt <- NULL
    for (i in seq_along(ste)) {
        tmp <- df[df[,"state"] == ste[i],]
        sel <- tmp[c(outcome,"hospital")]
        sel <- na.omit(sel)
        ord <- sel [ order ( sel[, outcome], sel[,"hospital"], na.last=TRUE, decreasing=FALSE),]
        
        rnk <- c(1:nrow(ord))
        ord <- cbind(ord,rnk)
        
        if (!is.numeric(num) & num == "best") {
            h <- ord[min(rnk),"hospital"]
        } else if (!is.numeric(num) & num == "worst") {
            h <- ord[max(rnk),"hospital"]
        } else if (is.numeric(num) & num > nrow(ord)) { 
            h <- "<NA>"  # if gt number of hospitals return NA
        } else {
            h <- ord[ord[,"rnk"] == num,"hospital"]
        }
        rslt <- rbind(rslt, c(h,ste[i]))
        
    }
    
    colnames(rslt) <- c("hospital","state")
    rownames(rslt) <- ste
    r <- as.table(rslt)
    r <- noquote(r)
    return(r)
}
