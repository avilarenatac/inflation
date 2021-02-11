
# Packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)
library(stringr)
library(tseries)


# LOAD DATA ------------------------------------------------------------- 

# Import CPI aggregates --------------------------------------------------
ipca <- sidrar::get_sidra(api = "/t/1737/n1/all/v/63,2265/p/all/d/v63%202,v2265%202")

# Corrections
ipca_table <- ipca %>%
                select(Date = `Mês (Código)`,
                       Variable = Variável, 
                       Value = Valor) %>%
                mutate(Date = parse_date(Date, format = "%Y%m"))


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


df <- ipca_group_all %>% 
  select(Date = `Mês (Código)`,
         Variable = Variável, 
         Value = Valor,
         category) %>%
  mutate(Date = parse_date(Date, format = "%Y%m"))




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



# Import inflation cores series --------------------------------------------------
# 11427 # EX0,  27839 # EX3, 4466  # MS, # 16122 # DP, # 28750 # P55
ipca_cores <- rbcb::get_series(code = list(core_series = 11427, core_series = 27839, 
                                           core_series= 4466,
                                           core_series = 16122, core_series = 28750))
names(ipca_cores) <- c("EX0", "EX3", "MS", "DP", "P55")


# Nest and add sum of last 12m --------------------------------------------------------
coreslist <- rbindlist(ipca_cores, idcol = TRUE)
cores_nest <- coreslist %>% group_by(.id) %>% nest()


core_12m <- function(x) {
  x %>%
    mutate(
      core_12m = rollsum(x[, "core_series"], k = 12, 
                         align = 'right', fill = 0)
      )
}

# Return to df format
cores_df <- cores_nest %>% 
  mutate(series_12m = map(.x = data, .f = core_12m)) %>%
  unnest(cols = series_12m) %>% select(-data)

# Add mean of cores
cores_mean <- cores_df %>% 
                group_by(date) %>% 
                summarize(mean_cores = mean(core_12m, na.rm = T))
