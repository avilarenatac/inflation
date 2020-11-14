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
                     plotOutput("plot_aggregate")
                     )
        
        ) # Close sidebarLayout
     ), # Close tabpanel
     
            
         tabPanel("Categories",
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
       p <- plot_agg(input$variable, input$dates[1], input$dates[2])
        
       if(input$show_target == TRUE) {
           p <- p + geom_line(aes(y = target), linetype = "solid") +
               geom_line(aes(y = upper), linetype = "dashed") +
               geom_line(aes(y = lower), linetype = "dashed") 
           
           #    + scale_linetype_manual(labels("Center", "Upper bound", "Lower bound"),
           #                         values = c("Center" = "solid", "Upper bound" = "dashed",
           #                                      "Lower bound" = "dashed"))
           
       }
       
        p
  
    })
    
    # Fix reactive - two inputs
    # Alternative: add a plot button
    
  #  plot_group_reac <- eventReactive(c(input$group, input$year), {
        
  #      plot_group(input$group, input$year)
  #  })
    
    
    output$plot_group <- renderPlot({
      plot_group(input$group, input$year)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
