# ui elements of section: Carrier Analytics
tabItem(
    tabName = "carrier",
    fluidRow(
        box(
            title = "Carriers in 2016",
            htmlOutput("gvTopCarr4Y"),
            solidHeader = T,
            status = "success",
            width = 9,
            height = 500
        )
        # ,
        # 
        # box(
        #     title = "Top 20 Airports each year from 1988 to 2016",
        #     # htmlOutput("gvTopAptMC"),
        #     solidHeader = T,
        #     status = "warning",
        #     width = 6,
        #     height = 400
        # )
    )
    # ,
    # fluidRow(
    #     box(background = "olive",
    #         status = "success",
    #         selectizeInput("CarrFocus",
    #             label = "Carrier",
    #             choices = vCarrCurr),
    #         width = 6,
    #         height = 90
    #     ),
    # valueBoxOutput("aptVBox1", width = 2),
    # valueBoxOutput("aptVBox2", width = 2),
    # valueBoxOutput("aptVBox3", width = 2)
    # )
)
