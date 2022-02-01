### Brazilian Inflation data

This repository contains code for a Shiny app that imports and plots inflation data from Brazil.

#### Content
- `README.md`: this file, summarizes the contents of the folder
- `/inflation_br`: shiny app with data visualizations and interactive options


### inflation_br

Shiny app structure:
- `input/read_data.R`: imports series from the APIs of the IBGE and the Brazilian Central Bank
- `load.R`: builds the functions used in the app
- `app.R`: contains the UI and server for the app
