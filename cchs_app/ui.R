library(shinydashboard, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinycssloaders, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(scales, warn.conflicts = FALSE)
library(GGally, warn.conflicts = FALSE)
library(FactoMineR, warn.conflicts = FALSE)
library(factoextra, warn.conflicts = FALSE)
library(ggforce, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)

source("functions.R")

ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "CCHS 2015"),

    # Sidebar with province selector
    dashboardSidebar(
        sidebarMenu(id = "sidebar",
                    menuItem("General Health", tabName = "gen", icon = icon("laugh-beam")),
                    menuItem("Flow", tabName = "flow", icon = icon("bacon")), 
                    menuItem("Parallel", tabName = "para", icon = icon("link")),
                    menuItem("Correspondence", tabName = "ca", icon = icon("arrows-alt")),
                    menuItem("Data", tabName = "dat", icon = icon("database")),

                        selectInput("province", "Province:",
                                    c("Canada" = "Canada",
                                      "British Columbia" = "British Columbia",
                                      "Alberta" = "Alberta", 
                                      "Saskatchewan" = "Saskatchewan", 
                                      "Manitoba" = "Manitoba",
                                      "Ontario" = "Ontario",
                                      "Quebec" = "Quebec", 
                                      "New Brunswick" = "New Brunswick", 
                                      "Nova Scotia" = "Nova Scotia",
                                      "Prince Edward Island" = "Prince Edward Island", 
                                      "Newfoundland and Labrador" = "Newfoundland and Labrador"),
                                    selected = "Canada"),
                    shiny::conditionalPanel(condition = "input.sidebar != 'dat'",
                                    selectInput("axis_x", "", choices = NULL),
                                    selectInput("axis_y", "", choices = NULL)
                    ),
                    shiny::conditionalPanel(condition = "input.sidebar == 'gen' || input.sidebar == 'flow'",
                            selectInput("facet", "", choices = NULL) 
                        ),
                    shiny::conditionalPanel(condition = "input.sidebar == 'gen'",
                            radioButtons("scale", "Scale", choices = c("free", "fixed"))
                        )
            
            )
                    
        ),

        # Show a plot or data for selected province
    dashboardBody(
        tabItems(
            tabItem(tabName = "gen",
                    fluidRow(
                        box(title = "Canadian Community Health Survey", status = "info", width = 10, 
                            collapsible = TRUE, collapsed = TRUE, 
                            p('The Canadian Community Health Survey (CCHS)', tags$sup(1), ' is conducted periodically to gain insight into health and nutritional habits of Canadians. These are results from the 2015 survey.'),
                            p("Self reported general and mental health from the responses are featured in this visualization as we explore respondents' sense of good or poor well being."), 
                            p("Perceived health is a combination of many factors and measured in terms of general and mental health."), 
                            p('Categorical survey responses are grouped and tallied to visualize distributions and relationships.'),
                            p("Try selecting different categories and see how the result compare. Can you spot any patterns?"), 
                            p(tags$sup(1), 'Statistics Canada (2017)', tags$em('Canadian Community Health Survey - Nutrition (CCHS)'), '[Data set] ', 
                              tags$br(),
                              tags$a(href="https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&Id=201486", "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&Id=201486")), 
                            
                        )
                    ),
                    fluidRow(
                            plotOutput("genHealthPlot", height = "500") %>% withSpinner()
                    )      
            ), 
            tabItem(tabName = "flow", 
                    fluidRow(
                        box(solidHeader = TRUE, title = "Mixed Relationships", status = "primary", width = 12, 
                            p("See how multiple factors relate and flow from one to another. Positive and negative sentiments grow in kind having dramatic influence on our health one way or the other."), 
                            
                        )
                    ), 
                    fluidRow(
                        plotOutput("forcePlot", height = "500") %>% withSpinner()
                    )
            ),
            tabItem(tabName = "para", 
                    fluidRow(
                        box(solidHeader = TRUE, title = "Parallel Lines", status = "primary", width = 12, 
                            p("This plot reveals relationships among multiple variables in respective groupings by connecting the values."), 
                            
                        )
                    ),
                    fluidRow(
                        plotOutput("parallelPlot", height = "500") %>% withSpinner()
                    )
            ),

            tabItem(tabName = "ca",
                    fluidRow(
                        box(solidHeader = TRUE, title = "Correspondence Analysis", status = "primary", width = 12, 
                                p("The degree of separation between values yields insight into possible corresponding factors.")
                            )
                    ), 
                    fluidRow(
                        plotOutput("biPlot", height = "500") %>% withSpinner()   
                    ), 
                    fluidRow(
                        # tableOutput("selected")
                        DT::dataTableOutput("selected")
                    )
            ), 
            
            tabItem(tabName = "dat",
                    fluidRow(
                        box(title = "About", solidHeader = TRUE, width = 12, status = "primary", 
                            p('Data are often in formats more challenging to measure and evaluating categories is such an example.'), 
                            p('The subject of health is complex and the aim is to pursue indicators of outcomes. Many relationships are obvious, others less so. '), 
                            p('Statistics Canada (2017)', tags$em('Canadian Community Health Survey - Nutrition (CCHS)'), '[Data set] ', 
                              tags$br(),
                              tags$a(href="https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&Id=201486", "https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&Id=201486"))
                          ),

                        DT::DTOutput("dataview") 

                    )
            )
            
        ),
    )
)

