# database interface functions====
require(dplyr)
# connect to sqlite db
file_db <- "data/timeFlies.sqlite"
db <- src_sqlite(file_db)
tblAll <- tbl(db, "allyears")

analyzeYearCarrier <- function(Yr = NULL, Carr = NULL){
    
    myFilter <- list()
    if (!is.null(Yr)) {
        myFilter <- c(myFilter, ~ Year == Yr)
    }
    
    if (!is.null(Carr)) {
        myFilter <- c(myFilter, ~ UniqueCarrier == Carr)
    }
    
    out <- tblAll %>% 
        filter_(.dots = myFilter) %>% 
        count(Year, UniqueCarrier) %>% 
        collect(n = Inf) %>% 
        as.data.frame()
    
    return(out)
}

