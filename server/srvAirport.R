### Airport Analytics ====
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
        options = list(
            width = 520,
            height = 320,
            minColor = '#006D2C',
            midColor = '#66C2A4',
            maxColor = '#EDF8FB',
            headerHeight = 20,
            fontColor = 'black',
            showScale = TRUE
        )
    )
})

# airports nearby map
output$gvAptNear <- renderGvis({
    req(input$aptFocus)
    
    
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