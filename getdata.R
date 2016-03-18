library(readr)
library(dplyr)

# source http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236
# download pre-zipped month files
preurl <- "http://tsdata.bts.gov/PREZIP/"  
years <- 2011:2016
months <- 1:12
for (yr in years) {
  for (mo in months) {
    tgtfile <- paste0("downloads/", yr, "_", mo, ".zip")
    if (!file.exists(tgtfile)) {
      srcfile <- paste0("On_Time_On_Time_Performance_", yr, "_", mo, ".zip") 
      msg <- paste("Downloading", srcfile, "...\t")
      cat(msg)
      download.file(url = paste0(preurl, srcfile),
                    destfile = tgtfile, quiet = T)
      cat('Done\n')
      
    }
  }
}

 

