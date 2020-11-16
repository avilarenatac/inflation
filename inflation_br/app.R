#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)


# source ------------------------------------------------------------------
source("load.R", encoding = "UTF-8")


# app ---------------------------------------------------------------------
ui <- fluidPage(

    # Application title
    titlePanel("Inflation Data - Brazil"),

    # Navbar page with two tabs
    navbarPage(" ",
        tabPanel("CPI", 
            sidebarLayout(
                sidebarPanel(
                    selectInput("variable", "Select series", choices = c("monthly", "12m")),
                    dateRangeInput("dates",
                                    "Select date range",
                                     start = "2016-01-01",
                                     end   = "2020-01-01",
                                     min   = "1990-01-01",
                                     max   = "2020-12-01"),
                   
                     conditionalPanel(condition = "input.variable != 'monthly'", 
                                      checkboxInput("show_target", "Show inflation target",
                                                    value = FALSE)
                                      )
                    
                    ), # Close sidebarPanel
            
                 mainPanel(
                     plotly::plotlyOutput("plot_aggregate")
                     )
        
        ) # Close sidebarLayout
     ), # Close tabpanel
     
            
         tabPanel("Categories",
              sidebarLayout(
                  sidebarPanel(
     
                        selectInput("group", "Select CPI category",
                                    choices = levels(ipca_group_all$category)),
     
                        selectInput("year", "Select first year",
                                    choices = c(2012:2020))
                  ),
     
                mainPanel(plotly::plotlyOutput("plot_group"))
              )
           
          )
    )   
)

# Define server logic 
server <- function(input, output) {

    output$plot_aggregate<- plotly::renderPlotly({
        
       p <- plot_agg(input$variable, input$dates[1], input$dates[2], input$show_target)
        
       
        ggplotly(p, tooltip = "y") %>%
            layout(xaxis = list(showline = TRUE),
                   yaxis = list(showline = TRUE))
        
    })
    
    
    output$plot_group <- plotly::renderPlotly({
        
      ggplotly(p = plot_group(input$group, input$year), tooltip = "y") %>%
                   layout(xaxis = list(showline = TRUE),
                   yaxis = list(showline = TRUE))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# Fix reactive - two inputs
# Alternative: add a plot button

#  plot_group_reac <- eventReactive(c(input$group, input$year), {

#      plot_group(input$group, input$year)
#  })