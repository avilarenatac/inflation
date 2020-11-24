# options -----------------------------------------------------------------
theme_set(theme_minimal())


# packages ----------------------------------------------------------------
library(lubridate)
library(zoo)
library(data.table)
library(stringr)
library(viridis)
library(plotly)


# LOAD DATA ------------------------------------------------------------- 

# Import CPI aggregates --------------------------------------------------
ipca <- sidrar::get_sidra(api = "/t/1737/n1/all/v/63,2265/p/all/d/v63%202,v2265%202")


# Import CPI groups since 2012
ipca_group        <- sidrar::get_sidra(api = "/t/1419/n1/all/v/63,66/p/all/c315/7169,7170,7171,7432,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202,v66%204")
ipca_group_2020   <- sidrar::get_sidra(api = "/t/7060/n1/all/v/63,66/p/all/c315/7169,7170,7171,7432,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202,v66%204")
ipca_group_all    <- data.table::rbindlist(list(ipca_group, ipca_group_2020))



# Create Group labels in English ------------------------------------------
ipca_group_all <- ipca_group_all %>%
  mutate(category = fct_recode(`Geral, grupo, subgrupo, item e subitem`,
                               "CPI" = "Índice geral" ,
                               "Food and Beverages" = "1.Alimentação e bebidas",
                               "Food at Home" = "11.Alimentação no domicílio",
                               "Food Away from Home" = "12.Alimentação fora do domicílio",
                               "Housing"  =  "2.Habitação" ,
                               "Furnishing, and Maint. Of House" = "3.Artigos de residência",
                               "Clothing and Footwear" =  "4.Vestuário",
                               "Transportation"  = "5.Transportes",
                               "Health"=  "6.Saúde e cuidados pessoais",
                               "Personal goods and services" = "7.Despesas pessoais" ,
                               "Education" = "8.Educação",
                               "Communication" =  "9.Comunicação"))

#Import inflation target series -----------------------------------------
infl_target <- rbcb::get_series(13521)
names(infl_target) <- c("Date", "target")


# Corrections -------------------------------------------------------------

# Turn year targets into monthly series
date_t <- data.frame(seq(first(infl_target$Date), last(infl_target$Date) , by='1 month'))
names(date_t) <- "Date"
infl_target <- left_join(date_t, infl_target, by = "Date")

# Define inflation target for all months of the year
infl_target <- infl_target %>%
                 mutate(target = na.locf(target))

infl_target <- data.table(infl_target) # makes following operations faster


# Define upper and lower bounds for yearly target as announced by the Central Bank
infl_target[, upper := 6.5]
infl_target[, lower := 2.5]

infl_target[(year(Date) == 2006 | year(Date) == 2018), upper := 6]

infl_target[(year(Date) == 2017 | year(Date) == 2018), upper := 6]
infl_target[(year(Date) == 2017 | year(Date) == 2018), lower := 3]


infl_target[(year(Date) == 2019), upper := 5.75]
infl_target[(year(Date) == 2019), lower := 2.75]

infl_target[(year(Date) == 2020), upper := 5.5]
infl_target[(year(Date) == 2020), lower := 2.5]

infl_target[(year(Date) == 2021), upper := 5.25]
infl_target[(year(Date) == 2021), lower := 2.25]

infl_target[(year(Date) == 2005), upper := 7]
infl_target[(year(Date) == 2005), lower := 2]

infl_target[(year(Date) == 2004), upper := 8]
infl_target[(year(Date) == 2004), lower := 3]

infl_target[(year(Date) == 2003), upper := 1.5]
infl_target[(year(Date) == 2003), lower := 6.5]

infl_target[(year(Date) == 2002), upper := 1.5]
infl_target[(year(Date) == 2002), lower := 5.5]

infl_target[(year(Date) == 2001), upper := 2]
infl_target[(year(Date) == 2001), lower := 6]


infl_target[(year(Date) == 2000), upper := 4]
infl_target[(year(Date) == 2000), lower := 8]



# Plot aggregates ---------------------------------------------------------
plot_agg <- function(variable, start_date, end_date, target = FALSE) {
  
  ipca_table <- ipca %>%
    select(Date = `Mês (Código)`,
           Variable = Variável, 
           Value = Valor) %>%
    mutate(Date = parse_date(Date, format = "%Y%m")) %>%
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
    ggplot(., aes(x = Date)) +
    scale_x_date(breaks = "6 months", date_labels = "%b %y") +
    geom_line(aes(y = Value), col = "#003B77") +
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

plot_agg("12m", "2010-01-01", "2018-01-01", target = TRUE)


df <- ipca_group_all %>% 
  select(Date = `Mês (Código)`,
         Variable = Variável, 
         Value = Valor,
         category) %>%
  mutate(Date = parse_date(Date, format = "%Y%m"))




# Plot CPI by group (category) -------------------------------------------------
plot_group <- function (group, since) {
  
   df <- df %>%
    filter(year(Date) >= since,
           Variable %in% c("IPCA - Variação mensal"),
           category == group) %>%
    mutate(Year = factor(lubridate::year(Date)))
  
  
  p <- ggplot(df, aes(x = month(Date), y = Value)) +
    geom_line(aes(col = Year)) +
    labs(x = 'Month', y = 'CPI (%)',
         title = paste0("CPI - ", df$category[1])) +
    scale_x_continuous(breaks = 1:12) +
    scale_color_viridis(discrete = TRUE, direction = -1) 
  
  return(p)
  
}

plot_group("Food and Beverages", 2015)



plot_group_contrib <- function(date) {
  
  incidence_table <-  df %>%
    filter(Date == date) %>%
   pivot_wider(names_from = Variable,
                values_from = Value) %>%
    # Incidence equals monthly inflation multiplied by the weight of each category
    mutate(Incidence = round((`IPCA - Variação mensal`)*(`IPCA - Peso mensal`/100), 2),
           signal = fifelse(Incidence >= 0, "positive", "negative"))
  
  
  ggplot(incidence_table, 
         aes(x = reorder(category, Incidence))) +
    
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

plot_group_contrib("2020-04-01")

# Import inflation cores series --------------------------------------------------
# 11427 # EX0,  27839 # EX3, 4466  # MS, # 16122 # DP, # 28750 # P55
ipca_cores <- rbcb::get_series(code = list(core_series = 11427, core_series = 27839, core_series= 4466,
                                           core_series = 16122, core_series = 28750))
names(ipca_cores) <- c("EX0", "EX3", "MS", "DP", "P55")


# Nest and add sum of last 12m --------------------------------------------------------
coreslist <- rbindlist(ipca_cores, idcol = TRUE)
cores_nest <- coreslist %>% group_by(.id) %>% nest()


core_12m <- function(x) {
  x %>%
    mutate(core_12m = rollsum(x[, "core_series"], k = 12, align = 'right', fill = 0))
  
}

cores_all <- cores_nest %>% 
  mutate(series_12m = map(.x = data, .f = core_12m))


# Unnest and plot graph function ------------------------------------------
cores_df <- unnest(cores_all, series_12m)

plot_cores <- function(start_date, end_date, show_target) {
  

  if(show_target == TRUE) {
    cores_df <- left_join(cores_df, infl_target, c("date" = "Date"))
  }

  p <-cores_df %>% 
        filter(date >= start_date, date <= end_date)  %>%
        
        ggplot(aes(x = date, y = core_12m)) +
        geom_line(aes(col = .id)) + 
        scale_color_manual(values=c("darkblue", "#9E1B32", "#58595B", "#482677FF", "#7B68EE")) +
        scale_x_date(breaks = "6 months", date_labels = "%b %y") +
        theme(axis.text.x = element_text(angle = 60), legend.title = element_blank()) +
        labs(x = '', y = '', title = "CPI cores (12m)")
  
  if(show_target == TRUE) {
  p <- p + geom_line(aes(y = target), linetype = "solid") +
    geom_line(aes(y = upper), linetype = "dashed") +
    geom_line(aes(y = lower), linetype = "dashed") 
  }

  return(p)
}


plot_cores("2015-01-01", "2019-12-01", show_target = TRUE)



# Add: filters for each core series
# Add: calculate series with mean of cores
# Add: option to display target series
# NEW: possibly - turn imported data into input script and source it from top of this script

