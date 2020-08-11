library(shiny)
library(shinydashboard)
library(dygraphs)
library(RColorBrewer)
library(dplyr)
library(dichromat)
library(zoo)
library(xts)
library(visNetwork)
library(geomnet)
library(igraph)
library(stringr)
library(knitr)
library(DT)
library(shinyjs)
library(shinyWidgets)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory
load("data/data.RData")

shinyServer(function(input, output) {

    output$nodeinfo <- DT::renderDataTable(
        DT::datatable(nodeinfo %>% mutate(`Average Connection Density` = round(`Average Connection Density`,2)), 
                      filter = 'top', 
                      extensions = 'Buttons', 
                      options = list(
                          columnDefs = list(list(className = 'dt-center')),
                          autoWidth = TRUE,
                          dom = 'Bfrtlip',
                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
            DT::formatRound('Average Connection Density', digits = 2)
    )
    
    output$conceptnetwork <- renderVisNetwork({

        nodes.fltr <- nodes %>% filter(value >= input$node.min, 
                                       value <= input$node.max)
        edges.fltr <- edges %>% filter(width2 >= input$cov.min, 
                                       width2 <= input$cov.max,
                                       as.character(edges$from) %in% as.character(nodes.fltr$label),
                                       as.character(edges$to) %in% as.character(nodes.fltr$label))
        
        visNetwork(nodes.fltr, edges.fltr, heigth = "100%", width = "100%") %>%
            visIgraphLayout(layout = "layout_in_circle") %>%
            visNodes(
                shape = "dot",
                color = list(
                    background = "#0085AF",
                    border = "#013848",
                    highlight = "#FF8000"
                )
            ) %>%
            visEdges(
                shadow = FALSE,
                color = list(color = "#0085AF", highlight = "#C62F4B"),
                scaling = list(min = 10, max = 30)
            ) %>%
            visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T), 
                       nodesIdSelection = list(main = "Select variable"), 
                       selectedBy = list(variable = "sample", multiple = T, main="Select cluster")) %>% 
            visLayout(randomSeed = 11)
    })
    
    output$dygraph <- renderDygraph({
        
        tdata <- xts(x = tweet.code.dum %>% select(input$dev.vars),
                     order.by = tweet.code.dum$date)
        
        color.shade1 <- ifelse(input$rb.shade1 == TRUE, "lightgrey","white")
        color.shade2 <- ifelse(input$rb.shade2 == TRUE, "lightgrey","white")
        color.shade3 <- ifelse(input$rb.shade3 == TRUE, "lightgrey","white")
        
        dygraph(tdata) %>% 
            dyAxis("y", label = "Average Frequency per day") %>%
            dyRoller(rollPeriod = input$roll, showRoller=F) %>%
            dyShading(from = as.POSIXct("2015-4-14", format="%Y-%m-%d"), to = as.POSIXct("2015-11-1", format="%Y-%m-%d"), color = color.shade1) %>%
            dyShading(from = as.POSIXct("2015-12-1", format="%Y-%m-%d"), to = as.POSIXct("2016-5-31", format="%Y-%m-%d"), color = color.shade2) %>%
            dyShading(from = as.POSIXct("2016-8-1", format="%Y-%m-%d"), to = as.POSIXct("2018-6-20", format="%Y-%m-%d"), color = color.shade3) %>%
            dyEvent("2016-12-1", "NVA Report", labelLoc = "top") %>%
            dyEvent("2017-4-1", "NVA Report", labelLoc = "top") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) %>%
            dyHighlight()%>%
            dyLegend(labelsDiv = 'legend.div', labelsSeparateLines=T) %>%
            dyRangeSelector()
    })
    
    observeEvent(input$reset_input_net, {
        shinyjs::reset("network-controls")
    })
    
    observeEvent(input$reset_input_dev, {
        shinyjs::reset("development_controls")
    })
    
    #observeEvent(input$switch_Data, {
    #    updateTabItems(session, "tabs", "documents")
    #})
    
})