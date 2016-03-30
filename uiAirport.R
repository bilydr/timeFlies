# ui elements of section: Airport Analytics
load('data/airport.Rdata')

# create a named vector for selectInput choices
lstAirport <- topApt16$OriginAirportID
names(lstAirport) <- topApt16$Airport

uiAirport <- tabItem(
    tabName = "airport",
    fluidRow(
        box(
            title = "Top 10 Airports in 2016",
            htmlOutput("gTopApt16"),
            solidHeader = T,
            status = "info",
            width = 6,
            height = 400
        ),
        
        box(
            title = "Top 20 Airports each year from 1988 to 2016",
            htmlOutput("gTopAptMC"),
            solidHeader = T,
            status = "info",
            width = 6,
            height = 400
        )
    ),
    fluidRow(
        box(
            status = "success",
            selectizeInput("aptFocus",
                           label = "Airport",
                           choices = lstAirport),
            width = 6,
            height = 90
        ),
        valueBoxOutput("aptVBox1", width = 2),
        valueBoxOutput("aptVBox2", width = 2),
        valueBoxOutput("aptVBox3", width = 2)
    ),
    fluidRow(
        box(title = "Map",
            p('use leaflet to show 5 nearby airports'),
            width = 6,
            height = 200
        ),
        box(title = "Evolution",
            p('use gvisLineChart to show #carriers, #routes, #flights along years'),
            width = 6,
            height = 200
        )
    ),
    fluidRow(
        box(title = "Route Destinations",
            p('use treemap to show destination shares'),
            width = 6,
            height = 200
        ),
        box(title = "Served Airlines",
            p('use ggplot2 facet to show airlines shares of flights'),
            width = 6,
            height = 200
            
        )
    )

)