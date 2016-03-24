library(dplyr)

# connect to sqlite db
db <- src_sqlite("data/timeFlies.sqlite")
tblAll <- tbl(db, "allyears")

# actual flights count per year
fliPerYr <- tblAll %>%
    count(Year) %>%
    collect() %>%
    mutate(var = n - lag(n),
           yoy = var / lag(n))


# Airports ----------------------------------------------------------------
# airports changes per Year
aptChn <- tblAll %>%
    count(OriginAirportID, Year) %>%
    collect() %>%  # groups:OriginAirportID only
    mutate(
        var = n - lag(n),
        yoy = var / lag(n),
        isNew = (row_number() == 1 | lag(Year) != Year - 1),
        toLea = (row_number() == n() | lead(Year) != Year + 1)
    ) %>%
    ungroup()

# airports count per year
aptNb <- aptChn %>%
    count(Year) %>%
    mutate(var = n - lag(n),
           yoy = var / lag(n))


# Airlines  ---------------------------------------------------------------
# airlines changes per Year
alnChn <- tblAll %>%
    count(UniqueCarrier, Year) %>%
    collect() %>%  # groups:UniqueCarrier only
    mutate(
        var = n - lag(n),
        yoy = var / lag(n),
        isNew = (row_number() == 1 | lag(Year) != Year - 1),
        toLea = (row_number() == n() | lead(Year) != Year + 1)
    ) %>%
    ungroup()

# airlines count per year
alnNb <- alnChn %>%
    count(Year) %>%
    mutate(var = n - lag(n),
           yoy = var / lag(n))

# Routes ------------------------------------------------------------------
# routes changes per Year
df <- tblAll %>%
    count(Year, OriginAirportID, DestAirportID) %>%
    collect() %>%
    mutate(
        routeID = paste(OriginAirportID, DestAirportID, sep = ';'),
        isReverse = OriginAirportID > DestAirportID
    )

# combine airport pairs into bi-directional routes
df[df$isReverse,]$routeID <-
    paste(df[df$isReverse,]$DestAirportID,
          df[df$isReverse,]$OriginAirportID, sep = ';')

rouChn <- df %>%
    group_by(routeID, Year) %>%
    summarize(n = sum(n)) %>%
    mutate(
        var = n - lag(n),
        yoy = var / lag(n),
        isNew = row_number() == 1,
        toLea = row_number() == n()
    ) %>%
    ungroup()

rm(df)

# routes count per year
rouNb <- rouChn %>%
    count(Year) %>%
    mutate(var = n - lag(n),
           yoy = var / lag(n))

save(aptChn, aptNb, alnChn, alnNb, rouChn, rouNb,
     file = "data/years.Rdata")
