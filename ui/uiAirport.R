# ui elements of section: Airport Analytics

tabItem(
    tabName = "airport",
    fluidRow(
        box(
            title = "Top 100 Airports in 2016",
            htmlOutput("gvTopAptCurr"),
            solidHeader = T,
            status = "success",
            width = 6,
            height = 400
        ),
        
        box(
            title = "Top 20 Airports each year from 1988 to 2016",
            htmlOutput("gvTopAptMC"),
            solidHeader = T,
            status = "warning",
            width = 6,
            height = 400
        )
    ),
    fluidRow(
        box(background = "olive",
            status = "success",
            selectizeInput("aptFocus",
                           label = "Airport",
                           choices = vTopApt),
            width = 6,
            height = 90
        ),
        valueBoxOutput("aptVBox1", width = 2),
        valueBoxOutput("aptVBox2", width = 2),
        valueBoxOutput("aptVBox3", width = 2)
    ),
    fluidRow(
        box(title = "Map",
            p('use gvisMap to show selected and 5 nearby airports'),
            width = 6,
            height = 400
        ),
        box(title = "Evolution of Capacity",
            htmlOutput("gvAptEvo"),
            width = 6,
            height = 400
        )
    ),
    fluidRow(
        box(title = "Route Destinations",
            htmlOutput("gvRouDest"),
            width = 6,
            height = 400
        ),
        box(title = "Carriers' Share",
            plotlyOutput("plCarrShare"),
            width = 6,
            height = 400
            
        )
    )

)