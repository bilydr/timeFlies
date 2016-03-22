library(readr)
library(RSQLite)
library(dplyr)

### get data from BTS website -----------------------------------------------
# source http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

## download pre-zipped monthly files
preurl <- "http://tsdata.bts.gov/PREZIP/"
# set the years and months - only use Jan to represent the year
# 1987 pre-zipped data has been unavailable since March 20, 2016
years <- 1988:2016
months <- 1:1


for (yr in years) {
  for (mo in months) {
    tgtfile <- paste0("downloads/", yr, "_", mo, ".zip")
    if (!file.exists(tgtfile)) {
      srcfile <-
        paste0("On_Time_On_Time_Performance_", yr, "_", mo, ".zip")
      msg <- paste("Downloading", srcfile, "...\t")
      cat(msg)
      download.file(
        url = paste0(preurl, srcfile),
        destfile = tgtfile,
        quiet = T
      )
      cat('Done\n')
      
    }
  }
}


## download lookup tables from BTS
# for field UniqueCarrier
download.file(url = 'http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_UNIQUE_CARRIERS',
              destfile = "downloads/L_UNIQUE_CARRIERS.csv",
              quiet = T)

# for field Carrier
download.file(url = 'http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_CARRIER_HISTORY',
              destfile = "downloads/L_CARRIER_HISTORY.csv",
              quiet = T)

# for fields OriginAirportID/DestAirportID
download.file(url = 'http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_AIRPORT_ID',
              destfile = "downloads/L_AIRPORT_ID.csv",
              quiet = T)

# for fields Origin/Dest
download.file(url = 'http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_AIRPORT',
              destfile = "downloads/L_AIRPORT.csv",
              quiet = T)


### Consolidate all years into a Sqlite database ----------------------------------------


# 1987 file was deleted by mistake - and bts site is down as of March 20
years <- 2001:2016
months <- 1:1

# column types specs
mycols <- cols_only(
  Year = 'i',
  Quarter = 'i',
  Month = 'i',
  DayofMonth = 'i',
  DayOfWeek = 'i',
  FlightDate = 'D',
  UniqueCarrier = 'c',
  AirlineID = 'i',
  Carrier = 'c',
  TailNum = 'c',
  FlightNum = 'c',
  OriginAirportID = 'i',
  OriginAirportSeqID = 'i',
  OriginCityMarketID = 'i',
  Origin = 'c',
  OriginCityName = 'c',
  OriginState = 'c',
  OriginStateFips = 'c',
  OriginStateName = 'c',
  OriginWac = 'i',
  DestAirportID = 'i',
  DestAirportSeqID = 'i',
  DestCityMarketID = 'i',
  Dest = 'c',
  DestCityName = 'c',
  DestState = 'c',
  DestStateFips = 'c',
  DestStateName = 'c',
  DestWac = 'i'
)

# open db connection
con <- dbConnect(SQLite(), "data/timeFlies.sqlite")

for (yr in years) {
  for (mo in months) {
    zipFile <- paste0("downloads/", yr, "_", mo, ".zip")
    df <- read_csv(zipFile) %>%
      select(Year,
             #Quarter, Month, DayofMonth, DayOfWeek, FlightDate,
             UniqueCarrier,
             Carrier,
             #AirlineID, TailNum, FlightNum,
             OriginAirportID,
             Origin,
             #OriginCityName,
             DestAirportID,
             Dest)
    df <- as.data.frame(df)
    dbWriteTable(con,
                 name = "allyears",
                 value = df,
                 append = T)
    # dfConso <- bind_rows(df, dfConso)
  }
}
rm(df)
gc()
dbDisconnect(con)
