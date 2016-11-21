library(RSQLite)
library(dplyr)
library(data.table)

### get airport geo-coordinates reference data 

fileurl <- 'https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat'

aptGeo <- fread(fileurl, data.table = F) %>% 
    select(Code = V5, lat = V7, lng = V8)
    

### get On-Time Performance data from BTS website -----------------------------------------------
# source http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

## download pre-zipped monthly files
preurl <- "http://tsdata.bts.gov/PREZIP/"
# set the years and months - only Jan through Mar to represent the year
# as of 20161121, 201603 data unavailable on BTS site
years <- 1987:2016
months <- 1:3

for (yr in years) {
    for (mo in months) {
        tgtfile <- paste0("downloads/", yr, "_", mo, ".zip")
        if (!file.exists(tgtfile)) {
            srcfile <-
                paste0("On_Time_On_Time_Performance_",
                       yr,
                       "_",
                       mo,
                       ".zip")
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


## download lookup tables from BTS and save as a Rdata file
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


uniCarr <- fread('downloads/L_UNIQUE_CARRIERS.csv', data.table = F)
carrHist <- fread('downloads/L_CARRIER_HISTORY.csv', data.table = F)
aptID <- fread('downloads/L_AIRPORT_ID.csv', data.table = F) 
# change airport ID to integer type
aptID$Code <- as.integer(aptID$Code)
apts <- fread('downloads/L_AIRPORT.csv', data.table = F) %>%
    left_join(aptGeo)
    
save(uniCarr, carrHist, aptID, apts, file = "data/lookup.Rdata")

## Consolidate all years into a Sqlite database ----------------------------------------
# 1987 file was deleted by mistake - and bts site is down as of March 20
years <- 1987:2016
months <- 1:3

fields <- c(
    "Year",
    #Quarter, Month, DayofMonth, DayOfWeek, FlightDate,
    "UniqueCarrier", "Carrier",
    #AirlineID, TailNum, FlightNum,
    "OriginAirportID", "Origin",
    #OriginCityName,
    "DestAirportID", "Dest"
)

# open db connection
file_db <- "data/timeFlies.sqlite"
con <- dbConnect(SQLite(), file_db)

for (yr in years) {
    for (mo in months) {
        zipFile <- paste0("downloads/", yr, "_", mo, ".zip")
        if (file.exists(zipFile)) {
            # extract file
            unzip(zipFile)
            csvFile <-
                paste0("On_Time_On_Time_Performance_", yr, "_", mo, ".csv")
            
            print(csvFile)
            # import data
            df <- fread(csvFile, sep = ',', header = T, select = fields)
            print("....imported!")
            # append data to sqlite db
            dbWriteTable(con,
                         name = "allyears",
                         value = df,
                         append = T)
            
            print("....appended to DB!")
            # clean up
            rm(df)
            gc()
            file.remove(csvFile)
        }
       
    }
}
# further clean up
dbDisconnect(con)
file.remove('readme.html')
Sys.chmod(file_db, "0666", FALSE)

