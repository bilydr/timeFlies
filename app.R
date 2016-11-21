library(shiny)
library(shinydashboard)
library(googleVis)
library(geosphere)
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
                     "Created by:", a(href="https://github.com/bilydr", "Longyi Bi"),
                     br(),
                     "Last update: Nov 21, 2016"))
        )
    ),
    dashboardBody(
        tags$head(
            # google analytics - remove the row below if not needed
            # includeScript("google-analytics.js"), 
            # apply customized styles
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            # section 1:Years in Review
            source(file.path("ui", "uiYears.R"),  local = TRUE)$value,
            
            # section 2:Airport Analytics 
            source(file.path("ui", "uiAirport.R"),  local = TRUE)$value,
            
            # section 3:Carrier Analytics 
            source(file.path("ui", "uiCarrier.R"),  local = TRUE)$value,

            # section 4:Route Analytics 
            source(file.path("ui", "uiRoute.R"),  local = TRUE)$value

        )
    )
)

# app - SERVER ------------------------------------------------------------
server <- function(input, output, session) {
    ### Years in Review==========
    source(file.path("server", "srvYears.R"),  local = TRUE)$value
   
    ### Years in Review==========
    source(file.path("server", "srvAirport.R"),  local = TRUE)$value
        
}

shinyApp(ui, server)