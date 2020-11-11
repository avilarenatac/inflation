# Packages ----------------------------------------------------------------
library(sidrar)
library(tidyverse)
library(lubridate)
library(data.table)
library(stringr)
library(RColorBrewer)


# Import CPI aggregates since 2012 ----------------------------------------
ipca <- sidrar::get_sidra(api = "/t/1737/n1/all/v/63,2265/p/last%20142/d/v63%202,v2265%202")


# Import CPI groups since 2012
ipca_group         <- sidrar::get_sidra(api = "/t/1419/n1/all/v/63/p/all/c315/7170,7171,7432,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202")
ipca_group_2020     <- sidrar::get_sidra(api = "/t/7060/n1/all/v/63/p/all/c315/7170,7171,7432,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202")
ipca_group_all    <- data.table::rbindlist(list(ipca_group, ipca_group_2020))



# Create Group labels in English ------------------------------------------
ipca_group_all <- ipca_group_all %>%
  mutate(category = fct_recode(`Geral, grupo, subgrupo, item e subitem`,
                               "Food and Beverages" = "1.Alimentação e bebidas",
                               "Food at Home" = "11.Alimentação no domicílio",
                               "Food Away from Home" = "12.Alimentação fora do domicílio",
                               "Housing"  =  "2.Habitação" ,
                               "Furnishing, Household Equip. and Maint. Of House" = "3.Artigos de residência",
                               "Clothing and Footwear" =  "4.Vestuário",
                               "Transportation"  = "5.Transportes",
                               "Health"=  "6.Saúde e cuidados pessoais",
                               "Personal goods and services" = "7.Despesas pessoais" ,
                               "Education" = "8.Educação",
                               "Communication" =  "9.Comunicação"))


# Plot aggregates ---------------------------------------------------------
plot_agg <- function(variable, start_date) {
  
  ipca_table <- ipca %>%
    select(Date = `Mês (Código)`,
           Variable = Variável, 
           Value = Valor) %>%
    mutate(Date = parse_date(Date, format = "%Y%m")) 
  
  
  if (variable == "monthly") {
    df <- ipca_table %>% filter(Variable == "IPCA - Variação mensal",
                                Date >= start_date)
    plot_title <- "CPI - Monthly"
  }
  
  else {
    df <- ipca_table %>% filter(Variable ==  "IPCA - Variação acumulada em 12 meses",
                                Date >= start_date)
    plot_title <- "CPI - Last 12 months"
  }
  
  p <- df  %>%
    ggplot(., aes(x = Date, y = Value)) +
    scale_x_date(breaks = "6 months", date_labels = "%b %y") +
    geom_line(col = "#003B77") +
    theme_set(theme_minimal() +
                theme(axis.text.x = element_text(angle = 60))) +
    labs(x = '', y = '', title = plot_title)
  
  return(p)
  
}

plot_agg("last_12", "2018-01-01")
plot_agg("monthly", "2012-01-01")


# Plot CPI by group (category) -------------------------------------------------
plot_group <- function (group) {
  
  if (str_length(group) == 2) {
    df <- ipca_group_all %>%
          filter(stringr::str_detect(`Geral, grupo, subgrupo, item e subitem`, paste0("^", group)))
  }
  
  else {
    df <- ipca_group_all %>% 
          filter(!stringr::str_detect(`Geral, grupo, subgrupo, item e subitem`, "\\d{2}"),
                  stringr::str_detect(`Geral, grupo, subgrupo, item e subitem`, paste0("^", group)))
  }
  
  df <- df %>% 
       select(Date = `Mês (Código)`,
             Variable = Variável, 
             Value = Valor,
             category) %>%
        mutate(Date = parse_date(Date, format = "%Y%m"),
               Year = factor(lubridate::year(Date)))

   
  p <- ggplot(df, aes(x = month(Date), y = Value)) +
        geom_line(aes(col = Year)) +
       theme_set(theme_minimal()) +
       labs(x = 'Month', y = 'CPI (%)',
            title = paste0("CPI - ", df$category[1])) +
       scale_x_continuous(breaks = 1:12) +
       scale_color_viridis(discrete = TRUE, direction = -1) 
  
  return(p)
  
}

plot_group("1")
plot_group("11")
plot_group("8")



# Plot CPI by group and year ----------------------------------------------
plot_group_year <- function (group) {
  
  if (str_length(group) == 2) {
    df <- ipca_group_all %>%
      filter(stringr::str_detect(`Geral, grupo, subgrupo, item e subitem`, paste0("^", group)))
  }
  
  else {
    df <- ipca_group_all %>% 
      filter(!stringr::str_detect(`Geral, grupo, subgrupo, item e subitem`, "\\d{2}"),
             stringr::str_detect(`Geral, grupo, subgrupo, item e subitem`, paste0("^", group)))
  }
  
  df <- df %>% 
    select(Date = `Mês (Código)`,
           Variable = Variável, 
           Value = Valor,
           category) %>%
    mutate(Date = parse_date(Date, format = "%Y%m"),
           Year = factor(lubridate::year(Date)))
  
  
  p <- ggplot(df, aes(x = month(Date), y = Value)) +
    geom_line(col = "#003B77") +
    facet_wrap(~Year, scales= 'free_x') +
    theme_set(theme_minimal()) +
    labs(x = 'Month',
         title = paste0("CPI - ", df$category[1])) +
    scale_x_continuous(breaks = 1:12) +
    scale_color_viridis(discrete = TRUE, direction = -1) 
  
  return(p)
  
}

plot_group_year("1")
plot_group_year("6")
plot_group_year("5")


# store full tables in .csv as input files? Shiny app would only subset them
