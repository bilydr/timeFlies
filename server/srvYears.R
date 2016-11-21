### Years in Review==========

## airport outputs===========
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