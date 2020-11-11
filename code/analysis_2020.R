#' @title inflation_analysis.R
#' @description Import Brazilian CPI data and plot graphs
#' @author Renata Avila


# The data used in this script is retrieved from the IBGE - the Brazilian NSO - using the 'sidrar' package
# Function 'get_sidra' allows R to connect with IBGE's API
# Parameters passed retrieve the series of interest


# Packages ----------------------------------------------------------------
library(sidrar)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(viridis)



# Import IPCA (Brazilian CPI) disaggregated data ----------------------
tab_ipca_2020 <- sidrar::get_sidra(api = "/t/7060/n1/all/v/all/p/last%2012/c315/all/d/v63%202,v66%204")



# Import CPI aggregates: monthly and aggregated over last 12 months --------------------
tab_ipca <- sidrar::get_sidra(api = "/t/1737/n1/all/v/all/p/last%2013/d/v63%202")


# Calculate the incidence of each group in the aggregate monthly index ------------
incidence_table <- tab_ipca_2020 %>%
                    filter(Variável %in% c("IPCA - Variação mensal", "IPCA - Peso mensal"),
                           !stringr::str_detect(`Geral, grupo, subgrupo, item e subitem`, "\\d{2}")) %>%
                    select(Date = `Mês (Código)`, 
                           Variable = Variável, 
                           Group = `Geral, grupo, subgrupo, item e subitem`,
                           Value = Valor) %>%
                    pivot_wider(names_from = Variable,
                                values_from = Value) %>%
  
                  # Incidence equals monthly inflation multiplied by the weight of each category
                    mutate(Incidence = (`IPCA - Variação mensal`)*(`IPCA - Peso mensal`/100),
                           Date = parse_date(Date, format = "%Y%m"))




# Plot CPI by month -------------------------------------------------------
ggplot(incidence_table %>% 
         filter(Group != "Índice geral"), 
       aes(x = Date)) +
  
  # Fill each bar with Incidence by group
  geom_col(aes(y = Incidence, fill = Group)) +
  
  # Draw line for aggregate result
  geom_line(data = subset(incidence_table, Group == "Índice geral"),
            aes(y = Incidence)) +
  scale_fill_brewer(palette = "PuBu", 
                    # Translate Portuguese labels to English
                    labels = c("Food and Beverages", "Housing", "Residency",
                               "Clothing and Apparel", "Transportation",
                               "Healthcare", "Personal expenditures",
                               "Education", "Communication")) +
  theme_minimal() +
  labs(x = "", y = "Incidence", 
       title = "IPCA (monthly) - Incidence by group")

ggsave(paste0("./output/", "cpi_brazil_groups.png"), width = 12, height = 8, units = "in")


# Other color options
  # scale_fill_brewer(palette = "Blues")
  #   trekcolors::scale_fill_trek("ufp")
  # scale_fill_viridis(discrete = TRUE)
  # gameofthrones::scale_fill_got(discrete = TRUE, option = 'white_walkers')



# Table with inflation aggregates for 2020 ----------------------------------
tab_agg <- tab_ipca %>%

  # Select monthly, 12 months and year aggregates
  filter(`Variável` == "IPCA - Variação mensal" |
                `Variável` == "IPCA - Variação acumulada em 12 meses" |
                `Variável` ==  "IPCA - Variação acumulada no ano",
                  ) %>%
  mutate(Date = parse_date(`Mês (Código)`, format = "%Y%m")) %>%
  filter(lubridate::year(Date) == 2020) %>%
  select(Date, 
         variable = Variável,
         value    = Valor) 

    
# Plot --------------------------------------------------------------------
ggplot(tab_agg, aes(x = Date, y = value)) +
  geom_col(aes(fill = variable)) +
  scale_fill_brewer(palette = "Blues",
                    labels = c("Last 12 months", "Year aggregate", "Month")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b/%Y") +
  geom_text(aes(label = value), position = position_stack(vjust = 0.5),
            check_overlap = TRUE) +

    labs(title = "IPCA - Brazilian CPI",
       x = "", y = "", alpha = "") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank())

   
ggsave(paste0("./output/", "cpi_brazil_aggregates.png"), width = 12, height = 8, units = "in")
