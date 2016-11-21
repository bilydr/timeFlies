### Airport Analytics ====
# carriers of current year in 1988/1998/2008/2016 - table 
output$gvTopCarr4Y <- renderGvis({
    
    getStat <- function(yr){
        analyzeYearCarrier(Yr = yr) %>% 
            select(-Year) %>% 
            setNames(c("Carr", paste0("Yr_", yr)))
            
    }
    withProgress(message = "querying data of 2016...", {
        df <- getStat(2016)
        
        setProgress(0.3, message = "adding data of 1988...")
        df <- df %>% left_join(getStat(1988), by = "Carr")
        
        setProgress(0.5, message = "adding data of 1998...")
        df <- df %>% left_join(getStat(1998), by = "Carr")
        
        setProgress(0.7, message = "adding data of 2008...")
        df <- df %>% left_join(getStat(2008), by = "Carr")
        
        setProgress(0.8, message = "adding Carrier names...")
        df <- df %>% left_join(uniCarr, by = c("Carr" = "Code")) %>% 
            rename(Carrier = Description)
        
        # sort by current period, i.e. last column
        # put columns in order by year
        df <- df[order(-df$Yr_2016), c(1, 6, 3:5, 2)]
        setProgress(0.9, message = "rendering google visualiation...")
        out <- gvisTable(df, options = list(height = "400px"))    
    })    
    
    return(out)
    
})
