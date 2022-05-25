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

Sys.setlocale('LC_ALL','C')

server <- function(input, output, session) {
  
  values <- reactiveValues(df_fs = NULL, sel = NULL, df = NULL, dfp = NULL, opts = NULL, facets = NULL, dfg = NULL)
  
  values$df_fs <- readRDS("health.Rds")
  values$opts <- c("General Health" = "general_health", "Mental Health" = "mental_health", "Sex" = "sex", "Age Group" = "age_group", "Main Activity" = "main_activity", "Life Satisfaction" = "life_satisfaction", "Life Stress" = "life_stress", "Work Stress" = "work_stress", "Belong Community" = "belong_community", "Household Income" = "income_household", "Food Security" = "food_security") 
  
  observe({
    updateSelectInput(session, "axis_x", choices = values$opts)
  })
  
  # remove the selected metric X from Y options
  observeEvent(input$axis_x, {
    yopts <- values$opts[values$opts!=input$axis_x]
    updateSelectInput(session, "axis_y", choices = yopts)
  })
  
  # remove selected metrics X, Y from facet selector
  observeEvent(input$axis_x, {
    if ((!is.na(input$axis_x) & !is.na(input$axis_y))) {
      # if ((!is.na(input$axis_x) & !is.na(input$axis_y)) && (input$axis_x == input$facet) ) {
      fopts <- values$opts[values$opts!=input$axis_x & values$opts!=input$axis_y]
      updateSelectInput(session, "facet", choices = fopts)
    } 
  })
  
  # remove selected metrics X, Y from facet selector
  observeEvent(input$axis_y, {
    if ((!is.na(input$axis_x) & !is.na(input$axis_y))) {
      # if ((!is.na(input$axis_x) & !is.na(input$axis_y)) && (input$axis_x == input$facet) ) {
      fopts <- values$opts[values$opts!=input$axis_x & values$opts!=input$axis_y]
      updateSelectInput(session, "facet", choices = fopts)
    } 
  })
  
  observeEvent(input$province, {
    
    values$sel <- c(input$province)
    
    if (input$province == "Canada") {
      d <- values$df_fs
    } else { 
      d <- values$df_fs %>% filter(province == toupper(input$province))
    }
    values$df <- d
    
    if (input$province == "Canada") {
      dfp <- values$df %>% 
        rename(x = country)
    } else {
      dfp <- values$df %>% 
        rename(x = province)
    }
    values$dfp <- dfp
    
  })
  
  
  output$dataview <-  DT::renderDT({
     t <- datatable(values$df, filter = "top", options = list(orderClasses = TRUE))
  })
  
  dfg <- reactive({
    parseGroupTri(values$dfp, !!as.symbol(input$axis_x), !!as.symbol(input$axis_y), !!as.symbol(input$facet))
  })
  
  output$genHealthPlot <- renderPlot({
    # many options are ideal with our base palette, yet some need more colours
    num_y <- nrow(unique(dfg()[,input$axis_y]))
    num_x <- nrow(unique(dfg()[,input$axis_x]))
    num_f <- nrow(unique(dfg()[,input$facet]))
    numPal <- max(num_y, num_x, num_f)
    pal <- if (numPal > 5) {
      colorRampPalette(healthPalette)(numPal)
    } else {
      rev(healthPalette)
    }
    
    out <- tryCatch (
      {
        dfg() %>% 
          ggplot(aes(x=get(input$axis_x), y=n, fill=get(input$axis_y))) +
          geom_bar(stat="identity") +
          labs(title = paste0(fixName(input$axis_y), " by ", 
                              fixName(input$axis_x), " and ", 
                              fixName(input$facet), " [", 
                              input$province, "]"), 
               x=fixName(input$axis_x), y="", fill = fixName(input$axis_y)) +
          theme(text = element_text(size = 18)) +
          scale_fill_manual(values = pal) +
          theme_minimal() + 
          theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5)) +
          facet_wrap(~ get(input$facet), scales = input$scale )                
      }, 
      error = function(cond) {
        message("Error: ")
        message(cond)
        return(NA)
      }, 
      warning = function(cond) {
        message("Warning: ")
        message(cond)
        return(NULL)
      }
    )
    return(out)
    
  })
  
  output$forcePlot <- renderPlot({
    
    mydft <- gather_set_data(dfg(), 1:3)
    
    mydft %>% 
      ggplot(aes(x, id = id, split = y, value = n)) +
      geom_parallel_sets(aes(fill = get(input$facet)), alpha = 0.3, axis.width = 0.05) +
      geom_parallel_sets_axes(axis.width = 0.05, fill = 'lightblue') +
      geom_parallel_sets_labels(colour = 'black', size = 3, angle = 0) + 
      labs(title = paste0(fixName(input$axis_y), " by ", fixName(input$axis_x), " [", input$province, "]"), x = "") + 
      # scale_fill_manual(values = rev(healthPalette)) +
      theme_minimal() +
      theme(legend.position = "none", axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })
  
  output$parallelPlot <- renderPlot({
    dfg <- parseGroup(values$dfp, !!as.symbol(input$axis_x), !!as.symbol(input$axis_y))
    dfg <- dfg %>% 
      select(-percent) %>% 
      spread(input$axis_x, n)
    
    # use 3 when 3rd input is available, else 2 
    p <- ggparcoord(dfg, columns = 2:ncol(dfg), groupColumn = 1, 
                    showPoints = TRUE, 
                    alphaLines = 1, 
                    boxplot = FALSE, 
                    scale = "globalminmax") +
      labs(title = paste0(fixName(input$axis_y), " by ", fixName(input$axis_x), " [", input$province, "]"), 
           x = "", y = "Occurrences", 
           colour = fixName(input$axis_y)) +
      theme_minimal() + 
      coord_flip() + 
      theme(legend.position = "top")
    
    return(p)
  })
  
  output$biPlot <- renderPlot({
    
    out <- tryCatch(
      {
        spreadMyGroup(values$dfp, !!as.symbol(input$axis_x), !!as.symbol(input$axis_y)) %>% 
          data.frame(row.names = 1) %>% 
          CA(graph = FALSE)  %>% 
          fviz_ca_biplot(title = paste0(fixName(input$axis_y), " by ", fixName(input$axis_x), " [", input$province, "]"),  
                         col.col = "#000099", col.row = "#CC0000", repel = TRUE)
      }, 
      error = function(cond) {
        message("Error: ")
        message(cond)
        return(NA)
      }, 
      warning = function(cond) {
        message("Warning: ")
        message(cond)
        return(NULL)
      }
    )
    return(out)
  })
  
  
  output$selected <-  DT::renderDataTable({
    x <- fixName(input$axis_x)
    y <- fixName(input$axis_y)
    option <- c(x, y)
    tdf <- data.frame(option)
    
    DT::datatable(tdf, colnames = c(''), rownames = c(''), style = "bootstrap", 
                  options = list(searching = FALSE, paging = FALSE, info = FALSE)) %>% 
      DT::formatStyle('option', color = DT::styleEqual(c(x, y), c('#000099', '#CC0000')), backgroundColor = "#E6ECF3", size = 6 )
    
  })
}

