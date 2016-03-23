library(shiny)
library(shinydashboard)
library(googleVis)
library(ggplot2)
# library(scales)
library(dplyr)
library(splitstackshape)

pctFmt <- function(x) {
    sprintf("%+1.1f%%", x * 100)
}

# connect to sqlite db
db <- src_sqlite("data/timeFlies.sqlite")
tblAll <- tbl(db, "allyears")

# read in lookup tables 
load('data/lookup.Rdata')
# read in pre - calculated data
load('data/years.Rdata')



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
# # evolution of top airports -            # airlines,            # flights
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
    dashboardHeader(title = "TimeFlies", titleWidth = 160),
    dashboardSidebar(
        width = 160,
        
        sidebarMenu(
            menuItem(
                "Years in Review", 
                tabName = "years", 
                icon = icon("clock-o")
            ),
            menuItem(
                "Airport Analytics",
                tabName = "airports",
                icon = icon("plane", lib = "glyphicon")
            ),
            menuItem(
                "Carrier Analytics",
                tabName = "airlines",
                icon = icon("plane")
            ),
            menuItem(
                "Route Analytics",
                tabName = "routes", 
                icon = icon("transfer", lib = "glyphicon")
            )
        )
    ),
    dashboardBody(
        tags$head(
            # google analytics
            includeScript("google-analytics.js"),
            # apply customized styles
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            # section 1:Years in Review
            tabItem(
                tabName = "years",
                # row of year input
                fluidRow(box(
                    status = "info",
                    sliderInput(
                        "yr1",
                        label = "Year",
                        min = 1989,
                        max = 2016,
                        value = 2015,
                        step = 1,
                        ticks = T,
                        sep = ""
                    ),
                    width = 12,
                    height = 100
                )),
                # a big container with 3 tabs
                fluidRow(
                    tabBox(
                        width = 12,
                        
                        # tab of Airports
                        tabPanel(
                            "Airports",
                            icon = icon("plane", lib = "glyphicon"),
                            fluidRow(
                                infoBoxOutput("aptBox1", width = 6),
                                valueBoxOutput("aptBox2", width = 3),
                                valueBoxOutput("aptBox3", width = 3),            
                                box(
                                    icon = icon("plane", lib = "glyphicon"),
                                    title = "New (top 10)",
                                    tableOutput("tblAptNew"),
                                    status = "success",
                                    solidHeader = T,
                                    width = 3,
                                    height = NULL,
                                    collapsible = T
                                ),
                                box(
                                    title = "Growth (top 10)",
                                    tableOutput("tblAptGro"),
                                    status = "primary",
                                    solidHeader = T,
                                    width = 3,
                                    height = NULL,
                                    collapsible = T
                                ),
                                box(
                                    title = "Downturn (top 10)",
                                    tableOutput("tblAptDec"),
                                    status = "warning",
                                    solidHeader = T,
                                    width = 3,
                                    height = NULL,
                                    collapsible = T
                                ),
                                box(
                                    title = "Defunct (top 10)",
                                    tableOutput("tblAptLea"),
                                    status = "danger",
                                    solidHeader = T,
                                    width = 3,
                                    height = NULL,
                                    collapsible = T
                                )                
                            )
                        ),
                        # tab of airlines
                        tabPanel(
                            "Airlines",
                            icon = icon("plane", lib = "font-awesome"),
                            fluidRow(
                                infoBoxOutput("alnBox1", width = 6),
                                valueBoxOutput("alnBox2", width = 3),
                                valueBoxOutput("alnBox3", width = 3),
                                box(
                                    title = "New (top 10)",
                                    tableOutput("tblAlnNew"),
                                    status = "success",
                                    solidHeader = T,
                                    width = 3,
                                    height = NULL,
                                    collapsible = T
                                ),
                                box(
                                    title = "Growth (top 10)",
                                    tableOutput("tblAlnGro"),
                                    status = "primary",
                                    solidHeader = T,
                                    width = 3,
                                    height = NULL,
                                    collapsible = T
                                ),
                                box(
                                    title = "Downturn (top 10)",
                                    tableOutput("tblAlnDec"),
                                    status = "warning",
                                    solidHeader = T,
                                    width = 3,
                                    height = NULL,
                                    collapsible = T
                                ),
                                box(
                                    title = "Defunct (top 10)",
                                    tableOutput("tblAlnLea"),
                                    status = "danger",
                                    solidHeader = T,
                                    width = 3,
                                    height = NULL,
                                    collapsible = T
                                )
                            )
                            
                        ),
                        
                        # tab of Routes
                        tabPanel(
                            "Routes",
                            icon = icon("transfer", lib = "glyphicon"),
                            fluidRow(
                                infoBoxOutput("rouBox1", width = 6),
                                valueBoxOutput("rouBox2", width = 3),
                                valueBoxOutput("rouBox3", width = 3),
                                box(
                                    title = "New (top 10)",
                                    tableOutput("tblRouNew"),
                                    status = "success",
                                    solidHeader = T,
                                    width = 3,
                                    height = NULL,
                                    collapsible = T
                                ),
                                box(
                                    title = "Growth (n>500, top 10)",
                                    tableOutput("tblRouGro"),
                                    status = "primary",
                                    solidHeader = T,
                                    width = 3,
                                    height = NULL,
                                    collapsible = T
                                ),
                                box(
                                    title = "Downturn (n>500, top 10)",
                                    tableOutput("tblRouDec"),
                                    status = "warning",
                                    solidHeader = T,
                                    width = 3,
                                    height = NULL,
                                    collapsible = T
                                ),
                                box(
                                    title = "Defunct (top 10)",
                                    tableOutput("tblRouLea"),
                                    status = "danger",
                                    solidHeader = T,
                                    width = 3,
                                    height = NULL,
                                    collapsible = T
                                )
                            )
                        )
                    )
                )
            ),
            
            # section 2:Airport Analytics 
            tabItem(tabName = "airports",
                    h2("tab 2")),
            # section 3:Airline Analytics 
            tabItem(tabName = "airlines",
                    h2("tab 3")),
            # section 4:Route Analytics 
            tabItem(tabName = "routes",
                    h2("tab 4"))
        )
    )
)

# app - SERVER ------------------------------------------------------------
server <- function(input, output, session) {
    ### Years Tab
    ## airport outputs
    aptStat <- reactive(aptNb[aptNb$Year == input$yr1,])
    output$aptBox1 <- renderInfoBox({
        infoBox(
            title = paste("N. Airports in", input$yr1),
            value = aptStat()$n, 
            icon = icon("plane", lib = "glyphicon"),
            color = "navy",
            fill = T
        )
    })
    
    output$aptBox2 <- renderValueBox({
        valueBox(
            value = sprintf("%+d", aptStat()$var),
            subtitle = 'vs. LY', 
            color = "navy"
        )
    })
    
    
    output$aptBox3 <- renderValueBox({
        valueBox(
            value = pctFmt(aptStat()$yoy),
            subtitle = 'YoY %', 
            color = "navy"
        )
    })
    
    output$tblAptNew <- renderTable({
        aptChn %>% 
            filter(Year == input$yr1, isNew) %>% 
            arrange(desc(n)) %>% 
            slice(1:10) %>%  
            left_join(aptID, by = c('OriginAirportID' = 'Code')) %>%
            select(Airport = Description, n)
        
    }, include.rownames = FALSE)
    
    output$tblAptGro <- renderTable({
        aptChn %>% 
            filter(Year == input$yr1, !isNew, var >= 0) %>% 
            arrange(desc(yoy)) %>% 
            slice(1:10) %>% 
            left_join(aptID, by = c('OriginAirportID' = 'Code')) %>% 
            mutate(YoY = pctFmt(yoy)) %>% 
            select(Airport = Description, n)
    }, include.rownames = FALSE)
    
    output$tblAptDec <- renderTable({
        aptChn %>% 
            filter(Year == input$yr1, !isNew, var < 0) %>% 
            arrange(yoy) %>% 
            slice(1:10) %>% 
            left_join(aptID, by = c('OriginAirportID' = 'Code')) %>% 
            mutate(YoY = pctFmt(yoy)) %>% 
            select(Airport = Description, n)
    }, include.rownames = FALSE)
    
    output$tblAptLea <- renderTable({
        aptChn %>% 
            filter(Year == input$yr1 - 1, toLea) %>% 
            arrange(desc(n)) %>% 
            slice(1:10) %>% 
            left_join(aptID, by = c('OriginAirportID' = 'Code')) %>% 
            select(Airport = Description, n_LY = n)
    }, include.rownames = FALSE)
    
    ## airline outputs
    alnStat <- reactive(alnNb[alnNb$Year == input$yr1,])
    output$alnBox1 <- renderInfoBox({
        infoBox(
            title = paste("N. Airlines in", input$yr1),
            value = alnStat()$n, 
            icon = icon("plane", lib = "font-awesome"),
            color = "navy",
            fill = T
        )
    })
    
    output$alnBox2 <- renderValueBox({
        valueBox(
            value = sprintf("%+d", alnStat()$var),
            subtitle = 'vs. LY', 
            color = "navy"
        )
    })
    
    
    output$alnBox3 <- renderValueBox({
        valueBox(
            value = pctFmt(alnStat()$yoy),
            subtitle = 'YoY %', 
            color = "navy"
        )
    })
    
    output$tblAlnNew <- renderTable({
        alnChn %>% 
            filter(Year == input$yr1, isNew) %>% 
            arrange(desc(n)) %>% 
            slice(1:10) %>% 
            left_join(uniCarr, by = c('UniqueCarrier' = 'Code')) %>% 
            select(Carrier = Description, n)
    }, include.rownames = FALSE)
    
    output$tblAlnGro <- renderTable({
        alnChn %>% 
            filter(Year == input$yr1, !isNew, var >= 0) %>%
            arrange(desc(yoy)) %>% 
            slice(1:10) %>% 
            left_join(uniCarr, by = c('UniqueCarrier' = 'Code')) %>% 
            mutate(YoY = pctFmt(yoy)) %>% 
            select(Carrier = Description, n, YoY)
    }, include.rownames = FALSE)
    
    output$tblAlnDec <- renderTable({
        alnChn %>% 
            filter(Year == input$yr1, !isNew, var < 0) %>%
            arrange(yoy) %>% 
            slice(1:10) %>% 
            left_join(uniCarr, by = c('UniqueCarrier' = 'Code')) %>% 
            mutate(YoY = pctFmt(yoy)) %>% 
            select(Carrier = Description, n, YoY)
    }, include.rownames = FALSE)
    
    output$tblAlnLea <- renderTable({
        alnChn %>% 
            filter(Year == input$yr1 - 1, toLea) %>% 
            arrange(desc(n)) %>% 
            slice(1:10) %>%
            left_join(uniCarr, by = c('UniqueCarrier' = 'Code')) %>% 
            select(Carrier = Description, n_LY = n)
    }, include.rownames = FALSE)
    
    ## route output
    rouStat <- reactive(rouNb[rouNb$Year == input$yr1,])
    
    output$rouBox1 <- renderInfoBox({
        infoBox(
            title = paste("N. Routes in", input$yr1),
            value = rouStat()$n,
            , 
            icon = icon("transfer", lib = "glyphicon"),
            color = "navy",
            fill = T
        )
    })
    
    output$rouBox2 <- renderValueBox({
        valueBox(
            value = sprintf("%+d", rouStat()$var),
            subtitle = 'vs. LY', 
            color = "navy"
        )
    })
    
    
    output$rouBox3 <- renderValueBox({
        valueBox(
            value = pctFmt(rouStat()$yoy),
            subtitle = 'YoY %', 
            color = "navy"
        )
    })
    
    output$tblRouNew <- renderTable({
        rouChn %>% 
            filter(Year == input$yr1, isNew) %>% 
            arrange(desc(n)) %>% 
            slice(1:10) %>% 
            cSplit_f('routeID', sep = ';') %>%       # resulting a data.table
            as.data.frame() %>% 
            left_join(aptID, by = c('routeID_1' = 'Code')) %>% 
            rename(Origin = Description) %>% 
            left_join(aptID, by = c('routeID_2' = 'Code')) %>%
            rename(Dest = Description) %>%
            mutate(Route = paste(Origin, Dest, sep = ' ~ ')) %>% 
            select(Route, n)
    }, include.rownames = FALSE)
    
    output$tblRouGro <- renderTable({
        rouChn %>% 
            filter(Year == input$yr1, !isNew, var >= 0, n >= 500) %>% 
            arrange(desc(yoy)) %>%
            slice(1:10) %>%
            cSplit_f('routeID', sep = ';') %>%       # resulting a data.table
            as.data.frame() %>% 
            left_join(aptID, by = c('routeID_1' = 'Code')) %>% 
            rename(Origin = Description) %>% 
            left_join(aptID, by = c('routeID_2' = 'Code')) %>%
            rename(Dest = Description) %>%
            mutate(Route = paste(Origin, Dest, sep = ' ~ ')) %>% 
            mutate(YoY = pctFmt(yoy)) %>%
            select(Route, n, YoY)
    }, include.rownames = FALSE)
    
    output$tblRouDec <- renderTable({
        rouChn %>% 
            filter(Year == input$yr1, !isNew, var < 0, n >= 500) %>% 
            arrange(yoy) %>%
            slice(1:10) %>%
            cSplit_f('routeID', sep = ';') %>%       # resulting a data.table
            as.data.frame() %>% 
            left_join(aptID, by = c('routeID_1' = 'Code')) %>% 
            rename(Origin = Description) %>% 
            left_join(aptID, by = c('routeID_2' = 'Code')) %>%
            rename(Dest = Description) %>%
            mutate(Route = paste(Origin, Dest, sep = ' ~ ')) %>% 
            mutate(YoY = pctFmt(yoy)) %>%
            select(Route, n, YoY)
    }, include.rownames = FALSE)
    
    output$tblRouLea <- renderTable({
        rouChn %>% 
            filter(Year == input$yr1 - 1, toLea) %>% 
            arrange(desc(n)) %>% 
            slice(1:10) %>% 
            cSplit_f('routeID', sep = ';') %>%       # resulting a data.table
            as.data.frame() %>% 
            left_join(aptID, by = c('routeID_1' = 'Code')) %>% 
            rename(Origin = Description) %>% 
            left_join(aptID, by = c('routeID_2' = 'Code')) %>%
            rename(Dest = Description) %>%
            mutate(Route = paste(Origin, Dest, sep = ' ~ ')) %>% 
            select(Route, n_LY = n)
    }, include.rownames = FALSE)
}

shinyApp(ui, server)