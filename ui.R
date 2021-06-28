library(shiny)
library(shinydashboard)
library(dygraphs)
library(RColorBrewer)
library(dplyr)
library(dichromat)
library(zoo)
library(xts)
library(dplyr)
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

# remove.packages("geomnet")
# install.packages("devtools")
# devtools::install_github("sctyner/geomnet")

shinyUI(
    dashboardPage(
        title = "ReMatriation Campaign",
        dashboardHeader(title=span( icon("fas fa-fist-raised"), "ReMatriate")),
        dashboardSidebar(
            sidebarMenu(
                menuItem(
                    "Info", tabName = "index", icon = icon("info")),
                menuItem("Info Table", tabName = "infotable", icon = icon("fas fa-table")),
                menuItem("Network", tabName = "network", icon = icon("fab fa-connectdevelop")),
                menuItem("Development", tabName = "development", icon = icon("fas fa-chart-line")),
                menuItem("Original Data", tabName = "documents", icon = icon("far fa-file-pdf"))
            )
        ),
        
        dashboardBody(
            tags$script(HTML("$('body').addClass('sidebar-mini');")),
            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
            tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
            tags$style("@import url(https://use.fontawesome.com/releases/v5.13.0/css/all.css);"),
            tags$script(HTML("
                            var openTab = function(tabName){
                              $('a', $('.sidebar')).each(function() {
                                if(this.getAttribute('data-value') == tabName) {
                                  this.click()
                                };
                              });
                            }
                          ")),
            
            tabItems(
                tabItem(tabName = "index",
                        h3("Welcome to the ReMatriation Data Tool"),
                        br(),
                        fluidRow(
                            box(#title = "Explore The Data", 
                                width = 8, 
                                heigth = "500px",
                                solidHeader = TRUE,
                                
                                h4("The Project:"),
                                #tags$b("The Project:"),
                                "Hi, I am ", 
                                tags$a(href="https://www.rug.nl/staff/j.schmidt/", 
                                                    target="_blank",
                                                    "Januschka Schmidt"), 
                                " and this website has been developed as part of my master's thesis project at the ",
                                tags$a(href="https://www.rug.nl", 
                                       target="_blank",
                                       "University of Groningen"),
                                ". In my thesis project I collaborated with the ReMatriate Collective to explore the content and 
                                character of the concept of rematriation and what it can contribute to Canada's reconciliation practices.
                                The ReMatriate Collective is an online campaign founded and run by indigenous womxn and aims to 
                                take back the identity of indigenous womxn by posting pictures of indigenous womxn on Social Media 
                                that depict the diversity and strength of indigenous womxn in Canada. To understand what the concept of rematriation refers to 
                                and how it evolved from the term the ReMatriate Collective introduced in 2015, I analyze two data sets in my thesis. 
                                The first one is an interview by the 'ReMatriate Collective' with the 'Red Man Laughing' podcast in 2015. 
                                The analysis of this interview shows us what the ReMatriate Collective originally wanted to achieve. 
                                The second data set consists of Twitter tweets from 14th of April 2015, when ReMatriate Collective first tweeted, 
                                to the 25th of May 2018, when this data was collected. This data sets includes all tweets with the hashtags 
                                #rematriation, #rematriate, as well as the Twitter account @ReMatriate. I have developed this applet to transparently
                                show the data I have collected and to give others an opportunity to use the data to come to their own conclusions.
                                ",
                                br(),
                                "You can find the ReMatriate Collective on: ",
                                tags$a(href="https://www.facebook.com/ReMatriate/", 
                                       target="_blank",
                                       icon("facebook")),
                                HTML("&nbsp"),
                                tags$a(href="https://www.instagram.com/rematriate_/", 
                                       target="_blank", 
                                       icon("instagram")),
                                br(),
                                br(),
                                h4("What You Can Do Here:"),
                                "This applet has ",
                                tags$b("four main interactive sections"),
                                " that enable visitors to directly interact with the data: ",
                                tags$ul(
                                    tags$li("The ",
                                            a("Original Data", onclick = "openTab('documents')", href="#"),
                                            " tab contains the full unedited print out of all the analyzed Twitter Tweets as well as the full transcript of the 
                                            'Red Man Laughing' episode from 2015.")),
                                "The remaining three tabs offer tools to visualize the coded results of the Twitter data set.",
                                tags$ul(
                                    tags$li("The ",
                                            a("Info Table", onclick = "openTab('infotable')", href="#"),
                                            " tab offers a table of all the codes that were deductively and inductively coded in the Twitter data set. 
                                            For every code the table lists some general descriptive statistics including its frequency, the number of co-occurences, 
                                            as well as two network density statistics that further indicate the connectedness of the codes. 
                                            The table is fully interactive, so that you can filter, search, and download the data."),
                                    tags$li("The ",
                                            a("Network", onclick = "openTab('network')", href="#"),
                                            " tab offers an interactive interface to explore the relationships between different codes within the Twitter data set."),
                                    tags$li("The ",
                                            a("Development", onclick = "openTab('development')", href="#"),
                                            " tab gives users the possibility to interactively explore how the frequency of codes changes over time. The interface offers
                                            the possibility to look at specific time windows, compare it to important events during the data collection period and observe the
                                            co-development of different codes.")
                                    )
                                
                            ),
                            box(width = 4,
                                HTML("<a class=\"twitter-timeline\" data-lang=\"en\" data-height=\"500\" href=\"https://twitter.com/ReMatriate?ref_src=twsrc%5Etfw\">Tweets by ReMatriate</a> <script async src=\"https://platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>")
                                    )
                        ),
                        fluidRow(
                            valueBox(1258, "Tweets", icon = icon("twitter"), width = 3),
                            valueBox("1:34", "Hours of Interview (20 pages)", icon = icon("microphone"), width = 3),
                            valueBox(129, "Codes", icon = icon("filter"), width = 3),
                            valueBox(4030, "Code Connections", icon = icon("project-diagram"), width = 3)
                        )
                    
                ),
                tabItem(tabName = "infotable",
                        h3("Twitter: Frequency and Connectedness Information"),
                        fluidRow(
                          box(#status = "primary",
                              width = 12,
                              DT::dataTableOutput('nodeinfo'),
                              hr(),
                              tags$i("Explanatory Note:"),
                              br(),
                              tags$b("Frequency:"),
                              " The frequency of a code is the total sum of times it was coded in the Twitter data set",
                              br(),
                              tags$b("Number of Co-Occurences:"),
                              " The number of co-occurences is measured as the total sum of times that the code co-occured with another code 
                              (note, that this includes multiple co-occurences of the same pair of codes; e.g., 'calls for action' might co-occure with 'critical' multiple times).",
                              br(),
                              tags$b("Average Connection Density:"),
                              " The average connection density also refers to code combinations but takes the amount of different connections into account 
                              (e.g., if a code has sixty co-occurrences these co-occurrences can be with sixty different other concepts: [60 co-occurrences] / [60 connection codes] = 1; 
                              or these sixty co-occurrences can always be with the same other concept: [60 co-occurrences] / [1 connection code] = 60). 
                              The density not only shows that codes are connected but also how strong these connections are on average.",
                              br(),
                              tags$b("Weighted Average Connection Density:"),
                              " The weighted average connection density combines the two connectedness measures (i.e., 'number of co-occurrences' x 'density of co-occurrences'). 
                              This accounts for the difficulty of taking an average in the connection density (e.g., [60 co-occurrences] / [60 connection codes] = 1 but also 
                              [3 co-occurrences] / [3 connection codes] = 1) it effectively weights the average strength connections (density) by the number of co-occurrences."
                              
                          )
                        )
                ),
                tabItem(tabName = "network",
                        h2("Twitter: Co-Occurrences Network"),
                        fluidRow(
                            box(#status = "primary",
                                width = 8,
                                #height = 500,
                                visNetworkOutput('conceptnetwork', height = "600px")
                            ),
                            box(#status = "primary",
                                shinyjs::useShinyjs(),
                                id = "network-controls",
                                width = 4,
                                height = "600px",
                                title = "Controls",
                                solidHeader = T,
                                status = "primary",
                                "Use these controls to inspect the connectedness of the codes.",
                                br(), 
                                
                                numericInput('node.min',
                                             HTML("Minimum Concept Frequency <br>(show if concept occures at least ___ times)"),
                                             value = min(nodes$value),
                                             min = min(nodes$value),
                                             max = max(nodes$value)),
                                numericInput('node.max',
                                             HTML("Maximum Concept Frequency <br>(show if concept occures at most ___ times)"),
                                             value = max(nodes$value),
                                             min = min(nodes$value),
                                             max = max(nodes$value)),
                                
                                #sliderInput('node_freq', 
                                #            HTML("Maximum Concept Frequency <br>(show if concept occures between ___ - ___ times)"),
                                #            min = min(nodes$value),
                                #            max = max(nodes$value),
                                #            value = c(min(nodes$value), max(nodes$value))),
                                
                                numericInput('cov.min',
                                             HTML("Minimum Connection Frequency <br>(show connection if concepts co-occure at least ___ times)"),
                                             value = min(edges$width2),
                                             min = min(edges$width2),
                                             max = max(edges$width2)),
                                numericInput('cov.max',
                                             HTML("Maximum Connection Frequency <br>(show connection if concepts co-occure at most ___ times)"),
                                             value = max(edges$width2),
                                             min = min(edges$width2),
                                             max = max(edges$width2)),
                                #sliderInput('cov.freq', 
                                #            HTML("Connection Frequency <br>(show connection if concepts co-occure between ___ - ___ times)"),
                                #            min = min(edges$width2),
                                #            max = max(edges$width2),
                                #            value = c(min(edges$width2), max(edges$width2))),
                                
                                hr(),
                                
                                div(style="display:inline-block;width:100%;text-align: center;",
                                    actionBttn(
                                        inputId = "reset_input_net",
                                        label = "Reset Inputs",
                                        style = "simple", 
                                        color = "primary",
                                        size = "sm"
                                    )
                                )
                            )
                        )
                ),
                tabItem(tabName = "development",
                        fluidRow(
                            h2("Twitter: (Co-)Development of the codes over time"),
                            br(),
                            box(status = "primary", 
                                width = 8,
                                #title = "Line Graph", 
                                #solidHeader = T,
                                dygraphOutput('dygraph'),
                                br(),
                                textOutput("legend.div")),
                            box(title = "Controls",
                                shinyjs::useShinyjs(),
                                id = "development_controls",
                                width = 4,
                                solidHeader = T,
                                status = "primary",
                                "Use these controls to inspect the development of the key variables.",
                                br(), br(),
                                selectInput("dev.vars", 
                                            label = "Please select your key variables of interst",
                                            choices = names(tweet.code.dum %>% dplyr::select(-id, -date)),
                                            multiple = T,
                                            selected = c("land", "Reconciliation", "MMIW", "spirituality")),
                                
                                prettySwitch(
                                    inputId = "rb.shade1",
                                    label = "Display TRC Period", 
                                    value = TRUE,
                                    status = "primary",
                                    fill = TRUE
                                ),
                                prettySwitch(
                                    inputId = "rb.shade2",
                                    label = "Display Pre-Inquiry MMIW", 
                                    value = TRUE,
                                    status = "primary",
                                    fill = TRUE
                                ),
                                prettySwitch(
                                    inputId = "rb.shade3",
                                    label = "Display Inquiry MMIW", 
                                    value = TRUE,
                                    status = "primary", # "default", "primary", "success", "info", "danger", "warning"
                                    fill = TRUE
                                ),
                                #checkboxInput("rb.shade1", label = "Display TRC Period", value = TRUE),
                                #checkboxInput("rb.shade2", label = "Display Pre-Inquiry MMIW", value = TRUE),
                                #checkboxInput("rb.shade3", label = "Display Inquiry MMIW", value = TRUE),
                                
                                sliderInput('roll', 
                                            HTML("Smoothing <br>(number of measurements in rolling window)"),
                                            0,
                                            1000,
                                            100),
                                
                                hr(),
                                
                                div(style="display:inline-block;width:100%;text-align: center;",
                                    actionBttn(
                                        inputId = "reset_input_dev",
                                        label = "Reset Inputs",
                                        style = "simple", 
                                        color = "primary",
                                        size = "sm"
                                    )
                                )
                                
                            )
                        )
                                
                ),
                tabItem(tabName = "documents",
                    tabsetPanel(type = "tab",
                        tabPanel("Twitter",
                                 HTML("<iframe src=\"https://drive.google.com/file/d/12Ix7DFUNXlj-t791K9_lP_l-Xp7nNeKB/preview\" width=\"100%\" height=\"725\"></iframe>")
                                 ),
                        tabPanel("Interview",
                                 HTML("<iframe src=\"https://drive.google.com/file/d/12ADnePfV5ifgQsPg8nMpn8Ohj46XVEZT/preview\" width=\"100%\" height=\"725\"></iframe>")
                                 )
                    )
                )
            )
        )
    )
)