library(tidyr)
library(dplyr)
library(googleVis)
# connect to sqlite db
db <- src_sqlite("data/timeFlies.sqlite")
tblAll <- tbl(db, "allyears")
load('data/lookup.Rdata')
load('data/years.Rdata')

# top 100 airports by flights in 2016 - column chart
topApt16 <- aptChn %>%
    filter(Year == max(Year)) %>%
    top_n(100, n) %>% 
    left_join(aptID, by = c('OriginAirportID' = 'Code')) %>% 
    rename(Airport = Description)
# data frame for gvis bar chart
df <- aptChn %>%
    filter(OriginAirportID %in% topApt16$OriginAirportID,
           Year %in% c(1988, 2002, 2016)) %>%
    left_join(aptID, by = c('OriginAirportID' = 'Code')) %>%
    select(Year, Airport = Description, nFlights = n) %>%
    spread(key = Year, value = nFlights)

pTopApt16 <- gvisTable(df,
                             # xvar = "Airport",
                             # yvar = c("1988", "2002", "2016"),
                             options = list(
                                 height = "300px",
                                 width = "540px"
                             ))
# plot(pTopApt16)

# top 20 airports by flights from 1988 to 2016 - motion chart
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
save(topApt16, pTopApt16, pTopAptPerYr,
     file = "data/airport.Rdata")