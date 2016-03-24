# ui elements of section: Years in Review
uiYears <- tabItem(
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
            # tab of carriers
            tabPanel(
                "Carriers",
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
)
