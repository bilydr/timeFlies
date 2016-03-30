library(tidyr)
library(dplyr)
library(googleVis)
# connect to sqlite db
db <- src_sqlite("data/timeFlies.sqlite")
tblAll <- tbl(db, "allyears")
load('data/lookup.Rdata')
load('data/years.Rdata')

# top 100 airports by flights in current/latest year - for column chart
currYr <- max(aptChn$Year)
topAptCurr <- aptChn %>%
    filter(Year == currYr) %>%
    top_n(100, n) %>% 
    left_join(aptID, by = c('OriginAirportID' = 'Code')) %>% 
    select(AirportID = OriginAirportID, Airport = Description,
           nbFlights = n, Year) 

# create a named vector for top Airports as selectInput choices
vTopApt <- topAptCurr$AirportID
names(vTopApt) <- topAptCurr$Airport

# Nb of Carriers/Routes/Flights by year for top airports defined above
dfTopApt <- tblAll %>%
    filter(OriginAirportID %in% topAptCurr$AirportID) %>% 
    select(Year, UniqueCarrier, 
           OriginAirportID, DestAirportID) %>%
    group_by(OriginAirportID, Year) %>% 
    summarize(nFlights = n(), 
              nCarriers = n_distinct(UniqueCarrier),
              nRoutes = n_distinct(DestAirportID)) %>% 
    collect() %>% 
    left_join(aptID, by = c('OriginAirportID' = 'Code')) %>% 
    rename(Airport = Description, AirportID = OriginAirportID) %>% 
    ungroup()


# data for gvis bar chart
dfBar <- dfTopApt %>%
    filter(Year %in% c(1988, 1998, 2008, currYr)) %>%
    select(Year, Airport, nFlights) %>%
    spread(key = Year, value = nFlights) 
# add prefix to avoid using numbers as column names
names(dfBar)[-1] <- paste("Yr", names(dfBar)[-1], sep = "_")
# sort by current period, i.e. last column
dfBar <- dfBar[order(-dfBar[ncol(dfBar)]),]
    

ptopAptCurr <- gvisTable(dfBar,
                             # xvar = "Airport",
                             # yvar = c("1988", "2002", "2016"),
                             options = list(
                                 height = "300px",
                                 width = "540px"
                             ))
# plot(ptopAptCurr)

# top 20 airports per year by flights from 1988 to 2016 - motion chart
dfTopAptYr <- aptChn %>%
    group_by(Year) %>%
    top_n(20, n) %>%
    left_join(aptID, by = c('OriginAirportID' = 'Code')) %>%
    select(Year, Airport = Description, nFlights = n)

myStateSettings <- '\n{"iconType":"BAR"}\n'
pTopAptPerYr <- gvisMotionChart(
    dfTopAptYr,
    idvar = "Airport",
    timevar = "Year",
    xvar = "f",
    yvar = "nFlights",
    options = list(
        height = "300px",
        width = "540px",
        state = myStateSettings
    ),
    chartid = 'apt2'
)

# save outcomes into Rdata file
save(vTopApt, dfTopApt, ptopAptCurr, pTopAptPerYr,
     file = "data/airport.Rdata")