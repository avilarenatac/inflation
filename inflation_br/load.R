# options -----------------------------------------------------------------
theme_set(theme_minimal())


# packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)
library(stringr)
library(viridis)


# source ------------------------------------------------------------------
source("input/read_data.R", encoding = "UTF-8")


# Plot aggregates ---------------------------------------------------------
plot_agg <- function(variable, start_date, end_date, target = FALSE) {
  
  ipca_table <- ipca_table %>%
                  filter(Date >= start_date,
                         Date <= end_date)
                
  
  if (variable == "monthly") {
    df <- ipca_table %>% filter(Variable == "IPCA - Variação mensal")
    plot_title <- "CPI - Monthly"
  }
  
  else {
    df <- ipca_table %>% filter(Variable ==  "IPCA - Variação acumulada em 12 meses")
    df <- left_join(df, infl_target, "Date")
    plot_title <- "CPI - Last 12 months"
  }
  
  
  p <- df  %>%
        ggplot(., aes(x = Date, group = 1, 
                      text = paste('Date:', format(Date, "%b %y"), '<br>Value: ', Value))) +
        scale_x_date(breaks = "6 months", date_labels = "%b %y") +
        geom_line(aes(y = Value), col = "#003B77", size = 0.5) +
        geom_point(aes(y = Value), col = "#003B77", size = 0.75, shape = 1) +
        theme(axis.text.x = element_text(angle = 60)) +
        labs(x = '', y = '', title = plot_title)
    
        
  # Add inflation target bounds
  
  if(variable != 'monthly' & target == TRUE) {
    
      p <- p + geom_line(aes(y = target), linetype = "solid") +
               geom_line(aes(y = upper), linetype = "dashed") +
               geom_line(aes(y = lower), linetype = "dashed") 
  }
  
  return(p)
  
}

plot_agg("12m", "2016-01-01", "2020-01-01", target = TRUE) %>% ggplotly(tooltip = "text")



# Plot CPI by group (category) -------------------------------------------------
plot_group <- function (group, since) {
  
   df <- df %>%
          filter(year(Date) >= since,
                 Variable %in% c("IPCA - Variação mensal"),
                 category == group) %>%
          mutate(Year = factor(lubridate::year(Date)))
        
        
    p <- ggplot(df, aes(x = month(Date), y = Value)) +
         geom_line(aes(col = Year)) +
         geom_point(aes(col = Year), size = 0.75) +
         labs(x = 'Month', y = 'CPI (%)',
              title = paste0("CPI - ", df$category[1])) +
         scale_x_continuous(breaks = 1:12) +
         scale_color_viridis(discrete = TRUE, direction = -1) 
      
  return(p)
  
}

plot_group("Food at Home", 2018)


# Plot contributions by groups --------------------------------------------
plot_group_contrib <- function(date) {
  
  incidence_table <-  df %>%
      filter(Date == date) %>%
     pivot_wider(names_from = Variable,
                  values_from = Value) %>%
      # Incidence equals monthly inflation multiplied by the weight of each category
      mutate(Incidence = round((`IPCA - Variação mensal`)*(`IPCA - Peso mensal`/100), 2),
             signal = fifelse(Incidence >= 0, "positive", "negative"))
    
  
    ggplot(incidence_table, aes(x = reorder(category, Incidence))) +
    
    # Fill each bar with Incidence by group
    geom_col(aes(y = Incidence, fill = signal)) +
    
    scale_fill_manual(values = c("negative" = "#8B0000", "positive"= "#104E8B"), 
                      guide = FALSE) +
    theme(axis.text.x = element_text(size = 0.1)) +
    labs(x = "", y = "Incidence", 
         title = paste0("CPI Incidence by group - ", 
                        format(incidence_table$Date[1], "%b/%y"))) +
    coord_flip()
  
}


# Plot Cores function -----------------------------------------------------
plot_cores <- function(start_date, end_date, show_target, show_mean) {
  
  cores_df <- cores_df %>% 
    filter(date >= start_date, date <= end_date)

  if(show_target == TRUE) {
    cores_df <- left_join(cores_df, infl_target, c("date" = "Date"))
  }
  
    p <- ggplot(cores_df, aes(x = date, y = core_12m)) +
          geom_line(aes(col = .id, group = 1,
                        text = paste0(.id, ": ", core_12m, "<br>Date: ", format(date, "%b %y")))) + 
          scale_color_manual(values=c("darkblue", "#9E1B32", "#58595B", "#482677FF", "#7B68EE")) +
          scale_x_date(breaks = "6 months", date_labels = "%b %y") +
          theme(axis.text.x = element_text(angle = 60), legend.title = element_blank()) +
          labs(x = '', y = '', title = "CPI cores (12m)")

    
  if(show_mean == TRUE) {
    cores_df <- left_join(cores_df, cores_mean)
     
    p <- ggplot(cores_df, aes(x = date, y = mean_cores)) +
      geom_line(aes(group = 1, text = paste0("Mean of cores: ", mean_cores, "<br>Date: ", format(date, "%b %y"))), 
                col = "#003B77", size = 0.5) +
      scale_x_date(breaks = "6 months", date_labels = "%b %y") +
      theme(axis.text.x = element_text(angle = 60), legend.title = element_blank()) +
      labs(x = '', y = '', title = "CPI cores (12m)")
  } 
  
       
  if(show_target == TRUE) {
  p <- p + geom_line(aes(y = target, group = 1, 
                         text = paste0("Center of target: ", target)), linetype = "solid") +
    geom_line(aes(y = upper, group = 1, text = paste0("Upper bound: ", target)), linetype = "dashed") +
    geom_line(aes(y = lower, group = 1, text = paste0("Lower bound: ", target)), linetype = "dashed") 
  }
  
  return(p)
}


plot_cores("2015-01-01", "2019-12-01", show_target = TRUE, show_mean = FALSE) %>% 
  ggplotly(., tooltip = "text")

# Improvements:
  # - Core series: add monthly graph, similar to first tabPanel with "monthly" and "12m" options in a dropdown menu
  # - Improve speed with cache by using bindCache from Shiny 1.6 version

cores_df %>% filter(date >= "2016-01-01", .id == "DP") %>%
ggplot(., aes(x = date)) +
  scale_x_date(breaks = "6 months", date_labels = "%b %y") +
  geom_line(aes(y = core_series), col = "#003B77", size = 0.5) +
  geom_point(aes(y = core_series), col = "#003B77", size = 0.75, shape = 1) +
  theme(axis.text.x = element_text(angle = 60)) +
  labs(x = '', y = '')
