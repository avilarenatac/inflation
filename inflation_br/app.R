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
library(shinycssloaders)
library(bslib)


# source ------------------------------------------------------------------
source("load.R", encoding = "UTF-8")


# app ---------------------------------------------------------------------
ui <- fluidPage(
   
     # Application theme
   # theme = bs_theme(version = 4, bootswatch = "cosmo"),

    # Application title
    titlePanel("Inflation Data - Brazil"),

    # Navbar page with two tabs
   navbarPage(" ",
  # theme = bslib::bs_theme(version = 4, bootswatch = "cerulean"),
        
         tabPanel("CPI", 
            sidebarLayout(
                sidebarPanel(
                    selectInput("variable", "Select series", choices = c("monthly", "12m")),
                    dateRangeInput("dates",
                                    "Select date range",
                                     start = "2016-01-01",
                                     end   = last(ipca_table$Date),
                                     min   = "1990-01-01",
                                     max   = last(ipca_table$Date)),
                   
                     conditionalPanel(condition = "input.variable != 'monthly'", 
                                      checkboxInput("show_target", "Show inflation target",
                                                    value = FALSE)
                                      ),
                    width = 3
                    
                    ), # Close sidebarPanel
            
                 mainPanel(
                     withSpinner(plotly::plotlyOutput("plot_aggregate"))
                     )
        
            ) # Close sidebarLayout
         ), # Close tabpanel
         
                
         tabPanel("Incidence",
              sidebarLayout(
                sidebarPanel(
                  
                  selectInput("date_contrib", "Select date",
                              choices = sort(levels(as.factor(df$Date)), decreasing = TRUE)),
                  
                  width = 3
                ),
                
                mainPanel(
                  withSpinner(plotly::plotlyOutput("plot_contrib"))
                  )
            )
                  
         ),
         
         tabPanel("Categories",
              sidebarLayout(
                  sidebarPanel(
     
                        selectInput("group", "Select CPI category",
                                    choices = levels(ipca_group_all$category)
                                    ),
     
                        selectInput("since", "Select first year",
                                    choices = c(2012:year(last(df$Date)))
                                    ),
                        width = 3
                    ),
     
                  mainPanel(
                    withSpinner(plotly::plotlyOutput("plot_group"))
                  )
               )
          
            ),
         
         tabPanel("Cores",
                  sidebarLayout(
                    sidebarPanel(
                      
                     dateRangeInput("dates_cores",
                                    "Select date range",
                                     start = "2016-01-01",
                                     end   = last(cores_df$Date),
                                     min   = "1990-01-01",
                                     max   = last(cores_df$Date)),
                     checkboxInput("show_target_core", "Show inflation target", value = FALSE),
                     checkboxInput("show_core_mean", "Show mean of cores", value = FALSE),
                     width = 3
                    ),
                    
                    mainPanel(
                      withSpinner(plotly::plotlyOutput("plot_cores"))
                      )
                  )
                  
         )
      ) # Close navbarPage  
  ) # Close fluidPage



# Define server logic 
server <- function(input, output) {

     output$plot_aggregate <-  plotly::renderPlotly({
    
          plot_agg(input$variable, input$dates[1], input$dates[2], input$show_target) %>% 
            ggplotly(., tooltip = "text", width = 700, height = 450) %>%
            layout(xaxis = list(showline = TRUE),
                   yaxis = list(showline = TRUE))
          
          }) 
     
     #%>% bindCache(input$variable, input$dates[1], input$dates[2], input$show_target)
     
     
        output$plot_contrib <- plotly::renderPlotly({
          
            p <- plot_group_contrib(input$date_contrib)
            
            p %>% ggplotly(., tooltip = "text", width = 700, height = 450) %>%
                  layout(xaxis = list(showline = TRUE),
                         yaxis = list(showline = TRUE)) %>%
                  hide_legend()
          
         })
    
    
        output$plot_group <- plotly::renderPlotly({
            
            p <-  plot_group(input$group, input$since)
            
            p %>% ggplotly(., tooltip = "y",
                            height = 450, width = 700) %>%
                  layout(xaxis = list(showline = TRUE),
                         yaxis = list(showline = TRUE))
        })
        
        
        output$plot_cores <- plotly::renderPlotly({
            p <-  plot_cores(input$dates_cores[1], input$dates_cores[2], 
                             input$show_target_core, input$show_core_mean)
            
            p %>% ggplotly(., tooltip = "text",
                           height = 450, width = 700) %>%
              layout(xaxis = list(showline = TRUE),
                     yaxis = list(showline = TRUE))
          
          }) 

}


# Run the application 
shinyApp(ui = ui, server = server)
