# 🌤️ London Weather Dashboard

This Shiny app displays weather data collected at Heathrow Airport in London from 1979 to 2020. Users can view and compare daily readings or monthly averages for several variables, including temperature, precipitation, sunshine, cloud cover, pressure, radiation, and snow depth.

## 🖼️ Interface Preview

![alt text](screenshot.jpeg)

## ⚙️ Setup and Installation

To run the app:

- Install R (version 4.0 or later).
- Install required packages: shiny, shinythemes, shinyWidgets, ggplot2, dplyr, readr, leaflet, scales, plotly.
- Clone this repository.

In RStudio or an R console, run ``` shiny::runApp(".") ```

The dashboard will open in your browser.

## 🔧 Tools

- R – primary environment for data analysis, scripting, and running the Shiny app
- shiny – framework for building the interactive dashboard UI and server logic
- shinythemes & shinyWidgets – prebuilt themes and advanced input widgets for consistent styling and user controls
- readr – fast CSV import and parsing of the raw weather data
- lubridate – conversion and manipulation of integer dates into R’s Date class
- dplyr – data filtering, grouping, and summarization for daily/monthly aggregations
- ggplot2 – construction of static plots (temperature, extra variables) with a clear, layered grammar
- plotly – conversion of ggplot2 objects to interactive charts and creation of the correlation heatmap
- leaflet – rendering of the interactive map pinpointing Heathrow Airport
- scales – axis formatting and expansion for clean, readable chart scales
