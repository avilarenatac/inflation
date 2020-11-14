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


# source ------------------------------------------------------------------
source("load.R", encoding = "UTF-8")


# app ---------------------------------------------------------------------
ui <- fluidPage(

    # Application title
    titlePanel("Inflation Data - Brazil"),

    # Sidebar with a slider input for number of bins 
   
    navbarPage(" ",
        tabPanel("Main", 
            sidebarLayout(
                sidebarPanel(
                    selectInput("variable", "Select series", choices = c("monthly", "12m")),
                    dateRangeInput("dates",
                                    "Select date range",
                                     start = "2016-01-01",
                                     end   = "2020-01-01",
                                     min   = "1990-01-01",
                                     max   = "2020-12-01")),
            
                 mainPanel(
                     plotOutput("plot_aggregate")
                     )
        
        )
     ),
     
            
         tabPanel("Group Plot",
              sidebarLayout(
                  sidebarPanel(
     
                        selectInput("group", "Select CPI group",
                                    choices = levels(ipca_group_all$category)),
     
                        selectInput("year", "Select first year",
                                    choices = c(2012:2020))
                  ),
     
                mainPanel(plotOutput("plot_group"))
              )
           
          )
    )   
)

# Define server logic 
server <- function(input, output) {

    output$plot_aggregate<- renderPlot({
        plot_agg(input$variable, input$dates[1], input$dates[2])
    })
    
    # Fix reactive - two inputs
    # Alternative: add a plot button
    
    plot_group_reac <- eventReactive(c(input$group, input$year), {
        
        plot_group(input$group, input$year)
    })
    
    
    output$plot_group <- renderPlot({
      plot_group_reac()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
