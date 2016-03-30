library(shiny)
library(shinydashboard)
library(googleVis)
library(ggplot2)
library(ggthemes)
library(plotly)
library(dplyr)
library(tidyr)
library(splitstackshape)
# library(scales)

# format percentage with 1 decimal
pctFmt <- function(x) {
    sprintf("%+1.1f%%", x * 100)
}

# connect to sqlite db
db <- src_sqlite("data/timeFlies.sqlite")
tblAll <- tbl(db, "allyears")

# read in lookup tables 
load('data/lookup.Rdata')
# read in pre-calculated data
load('data/years.Rdata')
load('data/airport.Rdata')
# import ui for each sections
source('uiYears.R')
source('uiAirport.R')
source('uiCarrier.R')
source('uiRoute.R')

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
                tabName = "airport",
                icon = icon("plane", lib = "glyphicon")
            ),
            menuItem(
                "Carrier Analytics",
                tabName = "carrier",
                icon = icon("plane")
            ),
            menuItem(
                "Route Analytics",
                tabName = "route", 
                icon = icon("transfer", lib = "glyphicon")
            ),
            hr(),
            tags$small(helpText("Data source:", 
                     a(href="http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236", 
                       "US DoT BTS"), br(),
                     "Created by:", a(href="https://github.com/bilydr", "Longyi Bi")))
        )
    ),
    dashboardBody(
        tags$head(
            # google analytics - remove the row below if not needed
            includeScript("google-analytics.js"), 
            # apply customized styles
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            # section 1:Years in Review
            uiYears,
            # section 2:Airport Analytics 
            uiAirport,
            # section 3:Carrier Analytics 
            uiCarrier,
            # section 4:Route Analytics 
            uiRoute
        )
    )
)

# app - SERVER ------------------------------------------------------------
server <- function(input, output, session) {
    ### Years in Review
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
    
    ## carrier outputs
    alnStat <- reactive(alnNb[alnNb$Year == input$yr1,])
    output$alnBox1 <- renderInfoBox({
        infoBox(
            title = paste("N. Carriers in", input$yr1),
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
    
    ### Airport Analytics 
    # top airports of current year in 1988/1998/2008/current - column chart
    output$gvTopAptCurr <- renderGvis({
        ptopAptCurr
    })
    
    # top airports motion chart
    output$gvTopAptMC <- renderGvis({
        pTopAptPerYr

    })
    
    ## user choice among top airports to focus
    aptFocusStat <- reactive(dfTopApt[dfTopApt$AirportID == input$aptFocus,])
    aptFocusCurr <- reactive(aptFocusStat()[aptFocusStat()$Year == max(aptFocusStat()$Year),])
    
    output$aptVBox1 <- renderValueBox({
        valueBox(
            value = aptFocusCurr()$nCarriers,
            subtitle = 'N. Carriers in 2016', 
            color = "olive"
        )
    })
    
    output$aptVBox2 <- renderValueBox({
        valueBox(
            value = aptFocusCurr()$nRoutes,
            subtitle = 'N. Routes in 2016', 
            color = "olive"
        )
    })
    
    output$aptVBox3 <- renderValueBox({
        valueBox(
            value = aptFocusCurr()$nFlights,
            subtitle = 'N. Flights in 2016', 
            color = "olive"
        )
    })
    # airport capacity evolution line chart
    output$gvAptEvo <- renderGvis({
        req(input$aptFocus)
        df <- aptFocusStat() %>%
            as.data.frame() 
        apt <- names(vTopApt[vTopApt == input$aptFocus])
        startYr <- min(aptFocusStat()$Year)
        endYr <- max(aptFocusStat()$Year)
        gvisLineChart(
            df,
            xvar = "Year",
            yvar = c("nCarriers", "nRoutes", "nFlights"),
            options = list(
                height = 320,
                title = paste0(
                    'Airport Capacity Evolution\n', 
                    apt, ' (', startYr, ' ~ ', endYr, ')'
                ),
                legend = "{alignment:'center', position:'top'}",
                series = "[{targetAxisIndex:0},{targetAxisIndex:0},
                {targetAxisIndex:1}]",
                vAxes = "[{title:'Nb. Carriers/ Routes'}, {title:'Nb. Flights'}]"
            )
            
        )
    })
    
    # route destination treemap
    output$gvRouDest <- renderGvis({
        req(input$aptFocus)
        apt <- names(vTopApt[vTopApt == input$aptFocus])
        df <- dfRouCurr %>%
            filter(OriginAirportID == input$aptFocus) %>%
            left_join(dfAptCurr[,c("AirportID", "Airport", "rank", "nFlights")], 
                      by = c('DestAirportID' = 'AirportID')) %>%
            rename(Dest = Airport, nDest = nFlights) %>%
            mutate(Origin = apt) %>%
            select(Origin, Dest, n, rank)
        df0 <-
            data.frame(
                Origin = NA,
                Dest = apt,
                n = sum(df$n),
                rank = NA
            )
        df <- rbind(df, df0)
        gvisTreeMap(
            df,
            idvar = "Dest",
            parentvar = "Origin",
            sizevar = "n",
            colorvar = "rank",
            options = list(width = 520,
                           height = 320,
                           minColor='#006D2C',
                           midColor='#66C2A4',
                           maxColor='#EDF8FB',
                           headerHeight=20,
                           fontColor='black',
                           showScale=TRUE)
        )
    })
    
    
    # Carriers' share - ggplot2 to plotly column chart 
    output$plCarrShare <- renderPlotly({
        req(input$aptFocus)
        p <- dfCarrShare %>%
            filter(AirportID == input$aptFocus,
                   share >= 0.05,
                   Year >= currYr - 9 ) %>%
            ggplot(aes(x = as.factor(Year), y = share, fill = Carrier)) +
            geom_bar(stat="identity", position="dodge") + 
            xlab("Year") + ylab("Share by Nb. Flights") +
            ggtitle(paste("Carriers operating more than 5% of total flights since", 
                          currYr - 9)) +
            theme_gdocs() + 
            scale_fill_gdocs()
        ggplotly(p) 
    })
        
}

shinyApp(ui, server)