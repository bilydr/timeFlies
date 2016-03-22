library(shiny)
library(shinydashboard)
library(googleVis)
library(ggplot2)
library(scales)
library(dplyr)

# connect to sqlite db
db <- src_sqlite("data/timeFlies.sqlite")
tblAll <- tbl(db, "allyears")

# read in lookup tables
load('data/lookup.Rdata')

# ##### airport  ----------------------------------------------------------------
#
# # top 5 airports by avg nb flights per period
# # https: /  / en.wikipedia.org / wiki / List_of_the_world%27s_busiest_airports_by_aircraft_movements
# dfToparp <- df %>%   
#     group_by(yr, mo, OriginAirportID) %>%   
#     summarise(n = sum(nfl)) %>%   
#     group_by(OriginAirportID) %>%   
#     summarise(n = mean(n)) %>%   
#     arrange(desc(n)) %>%   
#     top_n(4, wt = n)
#
# # evolution of top airports -    # airlines,    # flights
# dfEvo1 <- df %>%   
#     filter(mo == "01") %>%   
#     filter(OriginAirportID %in% dfToparp$OriginAirportID) %>%   
#     group_by(yr, OriginAirportID) %>%   
#     summarise(nFl = sum(nfl), nOpr = n_distinct(opr)) %>%   
#     select(
#         Year = yr,
#           Airport = OriginAirportID,
#         NbFlights = nFl,
#           NbAirlines = nOpr
#     )
#
# # draw a motion chart
# mTopEvo <- gvisMotionChart(
#     dfEvo1,
#     idvar = "Airport",
# timevar = "Year",
#     xvar = "NbAirlines",
# yvar = "NbFlights"
# )
# p1 <- plot(mTopEvo)
# p1


# app - UI ----------------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title = "TimeFlies"),
    dashboardSidebar(
        # google analytics
        tags$head(includeScript("google-analytics.js")),
        
        sidebarMenu(
            menuItem("Years", tabName = "years", icon = icon("clock-o")),
            menuItem(
                "Airports",
                tabName = "airports",
                icon = icon("plane", lib = "glyphicon")
            ),
            menuItem("Airlines", tabName = "airlines", icon = icon("plane")),
            menuItem(
                "Routes",
                tabName = "routes", 
                icon = icon("transfer", lib = "glyphicon")
            )
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(
                tabName = "years",
                fluidRow(
                    box(
                        # title = "Period",
                        sliderInput(
                            "newyr",
                            "Year",
                            min = 1989,
                            max = 2016,
                            value = 2015,
                            step = 1,
                            ticks = T,
                            sep = ""
                        ),
                        # background = 'white',
                        width = 3,
                        height = 100
                    ),
                    infoBoxOutput("aptBox", width = 3),
                    infoBoxOutput("alnBox", width = 3),
                    infoBoxOutput("rouBox", width = 3)
                ),
                # row of airports
                fluidRow(
                    box(
                        icon = icon("plane", lib = "glyphicon"),
                        title = "Airports: Newcomer",
                        tableOutput("tblAptNew"),
                        status = "success",
                        solidHeader = T,
                        width = 3,
                        height = 200
                    ),
                    box(
                        title = "Airports: Growth",
                        tableOutput("tblAptGro"),
                        status = "info",
                        solidHeader = T,
                        width = 3,
                        height = 200
                    ),
                    box(
                        title = "Airports: Decline",
                        tableOutput("tblAptDec"),
                        status = "warning",
                        solidHeader = T,
                        width = 3,
                        height = 200
                    ),
                    box(
                        title = "Airports: Leaver",
                        tableOutput("tblAptLea"),
                        status = "danger",
                        solidHeader = T,
                        width = 3,
                        height = 200
                    )
                ),
                # row of airlines
                fluidRow(
                    box(
                        title = "Airlines: Newcomer",
                        tableOutput("tblAlnNew"),
                        status = "success",
                        solidHeader = T,
                        width = 3,
                        height = NULL
                    ),
                    box(
                        title = "Airlines: Growth",
                        tableOutput("tblAlnGro"),
                        status = "info",
                        solidHeader = T,
                        width = 3,
                        height = NULL
                    ),
                    box(
                        title = "Airlines: Decline",
                        tableOutput("tblAlnDec"),
                        status = "warning",
                        solidHeader = T,
                        width = 3,
                        height = NULL
                    ),
                    box(
                        title = "Airlines: Leaver",
                        tableOutput("tblAlnLea"),
                        status = "danger",
                        solidHeader = T,
                        width = 3,
                        height = NULL
                    )
                ),
                
                # row of Routes
                fluidRow(
                    box(
                        title = "Routes: New",
                        tableOutput("tblRouNew"),
                        status = "success",
                        solidHeader = T,
                        width = 3,
                        height = 200
                    ),
                    box(
                        title = "Routes: Growth",
                        tableOutput("tblRouGro"),
                        status = "info",
                        solidHeader = T,
                        width = 3,
                        height = 200
                    ),
                    box(
                        title = "Routes: Decline",
                        tableOutput("tblRouDec"),
                        status = "warning",
                        solidHeader = T,
                        width = 3,
                        height = 200
                    ),
                    box(
                        title = "Routes: Discontinued",
                        tableOutput("tblRouLea"),
                        status = "danger",
                        solidHeader = T,
                        width = 3,
                        height = 200
                    )
                )
            ),
            
            # Second tab content
            tabItem(tabName = "airports",
                    h2("Widgets tab content"))
        )
    )
)




# app - SERVER ------------------------------------------------------------
server <- function(input, output, session) {
    dfPre <- reactive({
        myYr <- input$newyr - 1
        out <- tblAll %>%
            filter(Year == myYr) %>% 
            collect()
        return(out)
    })
    
    dfCur <- reactive({
        myYr <- input$newyr
        out <- tblAll %>%
            filter(Year == myYr) %>% 
            collect()
        return(out)
    })
    
    output$aptBox <- renderInfoBox({
        nAptPre <- n_distinct(dfPre()$OriginAirportID)
        nAptCur <- n_distinct(dfCur()$OriginAirportID)
        yoy <- nAptCur / nAptPre - 1
        infoBox(
            "Airports", 
            paste0(nAptCur, " : ", percent(yoy), ' YoY'), 
            icon = icon("plane", lib = "glyphicon"),
            color = "blue"
        )
    })
    
    output$alnBox <- renderInfoBox({
        nAlnPre <- n_distinct(dfPre()$UniqueCarrier)
        nAlnCur <- n_distinct(dfCur()$UniqueCarrier)
        yoy <- nAlnCur / nAlnPre - 1
        infoBox(
            "Airlines", 
            paste0(nAlnCur, " : ", percent(yoy), ' YoY'), 
            icon = icon("plane", lib = "font-awesome"),
            color = "blue"
        )
    })
    
    output$rouBox <- renderInfoBox({
        dfLY1 <- dfPre() %>% 
            select(OriginAirportID, DestAirportID) %>% 
            filter(OriginAirportID <= DestAirportID)
        dfLY2 <- dfPre() %>%
            select(OriginAirportID, DestAirportID) %>% 
            filter(OriginAirportID > DestAirportID)
        dfLY1$routeID <-
            paste(dfLY1$OriginAirportID, dfLY1$DestAirportID, sep = '-')
        dfLY2$routeID <-
            paste(dfLY2$DestAirportID, dfLY2$OriginAirportID, sep = '-')
        
        dfLY <- bind_rows(dfLY1, dfLY2)
        
        nRouPre <- n_distinct(dfLY$routeID)
        
        dfCY1 <- dfCur() %>% 
            select(OriginAirportID, DestAirportID) %>% 
            filter(OriginAirportID <= DestAirportID)
        dfCY2 <- dfCur() %>%
            select(OriginAirportID, DestAirportID) %>% 
            filter(OriginAirportID > DestAirportID)
        dfCY1$routeID <-
            paste(dfCY1$OriginAirportID, dfCY1$DestAirportID, sep = '-')
        dfCY2$routeID <-
            paste(dfCY2$DestAirportID, dfCY2$OriginAirportID, sep = '-')
        
        dfCY <- bind_rows(dfCY1, dfCY2)
        
        nRouCur <- n_distinct(dfCY$routeID)
        yoy <- nRouCur / nRouPre - 1
        infoBox(
            "Routes", 
            paste0(nRouCur, " : ", percent(yoy), ' YoY'), 
            icon = icon("transfer", lib = "glyphicon"),
            color = "blue"
        )
    })
    
    airlinesPre <- reactive(dfPre() %>%
                                group_by(UniqueCarrier) %>%
                                summarize(nflPre = n()))
    airlinesCur <- reactive(dfCur() %>%
                                group_by(UniqueCarrier) %>%
                                summarize(nflCur = n()))
    
    # new airlines entering the market
    newAirlines <- reactive({
        out <- airlinesCur() %>%
            anti_join(airlinesPre(), 
                      by = c('UniqueCarrier' = 'UniqueCarrier')) %>%
            arrange(desc(nflCur)) %>% 
            left_join(uniCarr, by = c('UniqueCarrier' = 'Code')) %>% 
            select(Carrier = Description, N_CY = nflCur)
        return(out)
    })
    
    # airlines active in both years 
    comAirlines <- reactive({
        out <- airlinesPre() %>%
            inner_join(airlinesCur(), 
                      by = c('UniqueCarrier' = 'UniqueCarrier')) %>%
            mutate(YoY = nflCur/nflPre -1) %>% 
            arrange(desc(YoY)) %>% 
            left_join(uniCarr, by = c('UniqueCarrier' = 'Code')) %>% 
            select(Carrier = Description, N_CY = nflCur, YoY)
        return(out)
    })
    
    # discontinued airlines
    disAirlines <- reactive({
        out <- airlinesPre() %>%
            anti_join(airlinesCur(), 
                      by = c('UniqueCarrier' = 'UniqueCarrier')) %>%
            arrange(desc(nflPre)) %>% 
            left_join(uniCarr, by = c('UniqueCarrier' = 'Code')) %>% 
            select(Carrier = Description, N_LY = nflPre)
        return(out)
    })
    
    output$tblAlnNew <- renderTable({
        newAirlines() 
    }, include.rownames = FALSE)
    output$tblAlnGro <- renderTable({
        comAirlines() %>% 
            filter(YoY >= 0) 
    }, include.rownames = FALSE)
    output$tblAlnDec <- renderTable({
        comAirlines() %>% 
            filter(YoY < 0) %>% 
            arrange(YoY)
    }, include.rownames = FALSE)
    
    output$tblAlnLea <- renderTable({
        disAirlines()
    }, include.rownames = FALSE)
}

shinyApp(ui, server)