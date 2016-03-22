library(readr)
library(RSQLite)
library(dplyr)

# 1987 file was deleted by mistake - and bts site is down as of March 20
# 2007, 2009, 2010 years need special attention on importing with read_csv
years <- c(1988:2006, 2008, 2011:2016)
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
      select(Year, #Quarter, Month, DayofMonth, DayOfWeek, FlightDate,
             UniqueCarrier, Carrier, #AirlineID, TailNum, FlightNum,
             OriginAirportID, Origin, #OriginCityName,
             DestAirportID, Dest)
    df <- as.data.frame(df)
    dbWriteTable(con, name = "allyears", value = df, append =T)
    # dfConso <- bind_rows(df, dfConso)
    rm(df)
    gc()
  }
}

dbDisconnect(con)
