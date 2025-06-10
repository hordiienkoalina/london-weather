library(shiny)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(readr)
library(leaflet)
library(scales)
library(plotly)

# ─────────────────────────────────────────────────────────────────────────────
weather <- read_csv("data.csv") %>%
  mutate(date = as.Date(as.character(date), "%Y%m%d"))

variable_units <- list(
  max_temp         = "Maximum Temperature (°C)",
  mean_temp        = "Mean Temperature (°C)",
  min_temp         = "Minimum Temperature (°C)",
  sunshine         = "Sunshine (hours)",
  global_radiation = "Global Radiation (W/m²)",
  precipitation    = "Precipitation (mm)",
  cloud_cover      = "Cloud Cover (oktas)",
  pressure         = "Atmospheric Pressure (Pa)",
  snow_depth       = "Snow Depth (cm)"
)

variable_colors <- c(
  max_temp         = "#F78679",
  mean_temp        = "#333333",
  min_temp         = "#5CAFE6",
  sunshine         = "#FFD700",
  global_radiation = "#FF8C00",
  precipitation    = "#00BFFF",
  cloud_cover      = "#A9A9A9",
  pressure         = "#8B0000",
  snow_depth       = "#4682B4"
)

extra_vars <- setdiff(
  names(variable_units),
  c("min_temp","mean_temp","max_temp")
)

# ─────────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML("
    body { background: #F0F0F0; }

    .summary-card {
      background: #ffffff; border-radius: 8px;
      padding:30px; text-align: center;
      margin-bottom: 8px; height: 110px;
    }

    .summary-value { font-size: 22px; font-weight: bold; }
    .summary-label { font-size: 12px; color: #555555; margin-top: 5px; }

    .info-panel {
      background: #ffffff; border-radius: 8px;
      padding: 15px; margin-bottom: 10px;
      padding-top: 5px;
      overflow: hidden;
    }

    .info-panel .leaflet-container {
      border-radius: 8px !important;
    }

    .chart-container {
      background: #ffffff; border-radius: 8px;
      padding: 15px; margin-top: 5px; margin-bottom: 20px;
    }

    .chart-container h4 {
      margin-top: 0px;
    }
  "))),
  
  br(),
  
  fluidRow(
    column(width = 3,
           br(),
           h1("London Weather", style = "text-align:center;"),
           h4("1979 – 2020", style = "text-align:center;"),
    ),
    column(width = 9,
           div(class = "info-panel",
               h4("About", style = "color:#666666;"),
               p("This dashboard presents a structured summary of weather data collected daily from a 
                 meteorological station near Heathrow Airport in London, covering the period from 1979 to 2020. 
                 The dataset includes continuous observations of temperature, precipitation, sunshine duration,
                 cloud cover, atmospheric pressure, global radiation, and snow depth. The interface enables data 
                 filtering by a specified date range and switching between daily and monthly-averaged views. 
                 Summary statistics are provided for each selected period, accompanied by interactive 
                 visualisations that reflect trends and variations across the recorded variables.",
                 style = "margin-bottom: 0;")
           )
    )
  ),
  br(),
  
  fluidRow(
    column(3,
           div(class = "info-panel", style = "height:250px;",
               h3("📍 Location"),
               leafletOutput("map", height = "160px")
           )
    ),
    column(9,
           fluidRow(
             column(4, uiOutput("avg_temp_card")),
             column(4, uiOutput("total_precip_card")),
             column(4, uiOutput("avg_pressure_card"))
           ),
           br(),
           fluidRow(
             column(4, uiOutput("avg_sunshine_card")),
             column(4, uiOutput("avg_cloud_card")),
             column(4, uiOutput("avg_radiation_card"))
           )
    )
  ),
  
  br(),
  
  fluidRow(
    column(3,
           div(class = "info-panel",
               h4("🗓️ Select Date Range"),
               dateRangeInput("date_range", NULL,
                              start  = as.Date("2020-01-01"),
                              end    = as.Date("2020-12-31"),
                              min    = min(weather$date),
                              max    = max(weather$date),
                              format = "yyyy-mm-dd",
                              width  = "100%"
               ),
               br(),
               h4("🔎 Select View"),
               radioGroupButtons(
                 inputId   = "view_mode",
                 label     = NULL,
                 choices   = c("Monthly Avg" = "monthly", "Daily" = "daily"),
                 selected  = "monthly",
                 status    = "default",
                 size      = "sm",
                 justified = TRUE
               )
           )
    ),
    column(9,
           div(class = "chart-container",
               h4("Temperature (°C)"),
               plotlyOutput("temp_chart", height = "300px")
           )
    )
  ),
  
  do.call(tagList, lapply(extra_vars, function(var) {
    fluidRow(
      column(3, NULL),
      column(9,
             div(class = "chart-container",
                 h4(variable_units[[var]]),
                 plotlyOutput(paste0(var, "_plot"), height = "300px")
             )
      )
    )
  }))
)

# ─────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$date_range)
    weather %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2])
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      setView(lng = -0.4543, lat = 51.4700, zoom = 11) %>%
      addMarkers(lng = -0.4543, lat = 51.4700,
                 popup = "Heathrow Airport")
  })
  
  output$avg_temp_card <- renderUI({
    df <- filtered_data()
    div(class="summary-card",
        div(class="summary-value",
            paste0(round(mean(df$mean_temp, na.rm=TRUE), 1), "°C")),
        div(class="summary-label", "Avg Temperature")
    )
  })
  output$total_precip_card <- renderUI({
    df <- filtered_data()
    div(class="summary-card",
        div(class="summary-value",
            paste0(round(sum(df$precipitation, na.rm=TRUE), 1), " mm")),
        div(class="summary-label", "Total Precipitation")
    )
  })
  output$avg_pressure_card <- renderUI({
    df <- filtered_data()
    div(class="summary-card",
        div(class="summary-value",
            paste0(round(mean(df$pressure, na.rm=TRUE)/100, 1), " hPa")),
        div(class="summary-label", "Avg Pressure")
    )
  })
  output$avg_sunshine_card <- renderUI({
    df <- filtered_data()
    div(class="summary-card",
        div(class="summary-value",
            paste0(round(mean(df$sunshine, na.rm=TRUE), 1), " hrs")),
        div(class="summary-label", "Avg Sunshine")
    )
  })
  output$avg_cloud_card <- renderUI({
    df <- filtered_data()
    div(class="summary-card",
        div(class="summary-value",
            paste0(round(mean(df$cloud_cover, na.rm=TRUE), 1), " oktas")),
        div(class="summary-label", "Avg Cloud Cover")
    )
  })
  output$avg_radiation_card <- renderUI({
    df <- filtered_data()
    div(class="summary-card",
        div(class="summary-value",
            paste0(round(mean(df$global_radiation, na.rm=TRUE), 1), " W/m²")),
        div(class="summary-label", "Avg Radiation")
    )
  })
  
  output$temp_chart <- renderPlotly({
    df <- filtered_data()
    
    if (input$view_mode == "monthly") {
      dfm <- df %>%
        mutate(month = factor(format(date, "%b"), levels = month.abb)) %>%
        group_by(month) %>%
        summarise(
          min_temp  = mean(min_temp,  na.rm = TRUE),
          mean_temp = mean(mean_temp, na.rm = TRUE),
          max_temp  = mean(max_temp,  na.rm = TRUE)
        )
      
      p <- ggplot(dfm, aes(x = month)) +
        geom_col(aes(y = max_temp), fill = variable_colors[["max_temp"]], alpha = 1, width = 0.6) +
        geom_col(aes(y = min_temp), fill = variable_colors[["min_temp"]], alpha = 1, width = 0.6) +
        geom_line(aes(y = mean_temp, group = 1), color = variable_colors[["mean_temp"]], size = 1.2) +
        geom_point(aes(y = mean_temp), color = variable_colors[["mean_temp"]], size = 2)
      
    } else {
      p <- ggplot(df, aes(x = date)) +
        geom_line(aes(y = min_temp), color = variable_colors[["min_temp"]], size = 0.6) +
        geom_line(aes(y = max_temp), color = variable_colors[["max_temp"]], size = 0.6) +
        geom_line(aes(y = mean_temp), color = variable_colors[["mean_temp"]], size = 1.2)
    }
    fluidRow(
      column(3,
             div(class = "info-panel", style = "height:210px;",
                 h3("📍 Location"),
                 leafletOutput("map", height = "120px")
             )
      ),
      column(9,
             fluidRow(
               column(4, uiOutput("avg_temp_card")),
               column(4, uiOutput("total_precip_card")),
               column(4, uiOutput("avg_pressure_card"))
             ),
             fluidRow(
               column(4, uiOutput("avg_sunshine_card")),
               column(4, uiOutput("avg_cloud_card")),
               column(4, uiOutput("avg_radiation_card"))
             )
      )
    )
    
    p <- p + labs(x = NULL, y = "°C") +
      theme_minimal() +
      scale_y_continuous(breaks = seq(0, 25, 5)) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text          = element_text(size = 10)
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(margin = list(b = 40))
  })
  
  lapply(extra_vars, function(var) {
    output[[paste0(var, "_plot")]] <- renderPlotly({
      df <- filtered_data()
      fixed <- switch(var,
                      pressure   = c(95000, 105000),
                      snow_depth = c(0, 25),
                      NULL
      )
      
      if (input$view_mode == "monthly") {
        dfm <- df %>%
          mutate(month = factor(format(date, "%b"),
                                levels = month.abb)) %>%
          group_by(month) %>%
          summarise(val = mean(.data[[var]], na.rm=TRUE))
        
        if (is.null(fixed)) {
          rng <- range(dfm$val, na.rm=TRUE)
          pad <- 0.05 * diff(rng)
          lower <- ifelse(rng[1] > 0, 0, rng[1] - pad)
          upper <- rng[2] + pad
        } else {
          lower <- fixed[1]; upper <- fixed[2]
        }
        
        p <- ggplot(dfm, aes(x = month, y = val)) +
          geom_col(fill = variable_colors[[var]], width = 0.6) +
          labs(x = NULL, y = variable_units[[var]]) +
          scale_y_continuous(limits = c(lower, upper),
                             expand = expansion(0)) +
          theme_minimal() +
          theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor   = element_blank(),
            axis.text          = element_text(size = 10)
          )
        
      } else {
        if (is.null(fixed)) {
          rng <- range(df[[var]], na.rm=TRUE)
          pad <- 0.05 * diff(rng)
          lower <- ifelse(rng[1] > 0, 0, rng[1] - pad)
          upper <- rng[2] + pad
        } else {
          lower <- fixed[1]; upper <- fixed[2]
        }
        
        p <- ggplot(df, aes(x = date, y = .data[[var]])) +
          geom_line(color = variable_colors[[var]], size = 0.8) +
          labs(x = NULL, y = variable_units[[var]]) +
          scale_y_continuous(limits = c(lower, upper),
                             expand = expansion(0)) +
          theme_minimal() +
          theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor   = element_blank(),
            axis.text          = element_text(size = 10)
          )
      }
      
      ggplotly(p, tooltip = c("x", "y")) %>% layout(margin = list(b = 40))
    })
  })
}

shinyApp(ui, server)
