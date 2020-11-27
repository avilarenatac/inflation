### Brazilian Inflation data

This repository contains code for importing and plotting inflation data from Brazil

#### Content
- `README.md`: this file, summarizes the contents of the repository
- `/code`: contains scripts for importing, processing and creating data visualizations
- `/output`: contains plots and other report files
- `/inflation_br`: shiny app for visualizing the series


### inflation_br

Shiny app structure:
- `input/read_data.R`: imports series from the APIs of the IBGE and the Brazilian Central Bank
- `load.R`: builds the functions used in the app
- `app.R`: contains the UI and server for the app
