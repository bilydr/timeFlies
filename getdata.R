library(readr)
library(RSQLite)
library(dplyr)

### get data from BTS website -----------------------------------------------
# source http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

## download pre-zipped monthly files
preurl <- "http://tsdata.bts.gov/PREZIP/"
# set the years and months - only use Jan to represent the year 
years <- 1987:2016
months <- 1:1



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


## download lookup tables from BTS
# for field UniqueCarrier
download.file(
  url = 'http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_UNIQUE_CARRIERS',
  destfile = "downloads/L_UNIQUE_CARRIERS.csv", quiet = T)

# for field Carrier
download.file(
  url = 'http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_CARRIER_HISTORY',
  destfile = "downloads/L_CARRIER_HISTORY.csv", quiet = T)

# for fields OriginAirportID/DestAirportID
download.file(
  url = 'http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_AIRPORT_ID',
  destfile = "downloads/L_AIRPORT_ID.csv", quiet = T)

# for fields Origin/Dest
download.file(
  url = 'http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_AIRPORT',
  destfile = "downloads/L_AIRPORT.csv", quiet = T)


### Consolidate all years into a Sqlite database ----------------------------------------


# 1987 file was deleted by mistake - and bts site is down as of March 20
years <- 2001:2010
months <- 1:1


# open db connection
con <- dbConnect(SQLite(), "data/timeFlies.sqlite")

for (yr in years) {
  for (mo in months) {
    zipFile <- paste0("downloads/", yr, "_", mo, ".zip")
    df <- read_csv(zipFile) %>% 
      select(Year, #Quarter, Month, DayofMonth, DayOfWeek, FlightDate,
             UniqueCarrier, Carrier, #AirlineID, TailNum, FlightNum,
             OriginAirportID, Origin, #OriginCityName,
             DestAirportID, Dest)
    df <- as.data.frame(df)
    dbWriteTable(con, name = "allyears", value = df, append =T)
    # dfConso <- bind_rows(df, dfConso)
  }
}
rm(df)
gc()
dbDisconnect(con)
