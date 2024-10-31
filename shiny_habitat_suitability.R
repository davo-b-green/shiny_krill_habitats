library(shiny)
library(ncdf4)
library(leaflet)
library(raster)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Krill Habitat Suitability"),
  sidebarLayout(
    sidebarPanel(
      fileInput("temp_file", "Temperature NetCDF File", placeholder = "Default: ./temperature.nc"),
      fileInput("chl_file", "Chlorophyll NetCDF File", placeholder = "Default: ./chlorophyll.nc"),
      fileInput("ice_file", "Sea Ice Concentration NetCDF File", placeholder = "Default: ./sea_ice_concentration.nc"),
      
      selectInput("temp_eq", "Temperature Suitability Equation", choices = c("Sigmoid", "Lognormal")),
      uiOutput("temp_params"),
      
      selectInput("chl_eq", "Chlorophyll Suitability Equation", choices = c("Holling", "Sigmoid", "Lognormal")),
      uiOutput("chl_params"),
      
      selectInput("ice_eq", "Sea Ice Concentration Suitability Equation", choices = c("Sigmoid", "Lognormal", "No effect")),
      uiOutput("ice_params"),
      
      uiOutput("time_step_slider"),
      textOutput("time_step_date")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Habitat Suitability", leafletOutput("map_suitability")),
        tabPanel("Temperature", leafletOutput("map_temp")),
        tabPanel("Chlorophyll", leafletOutput("map_chl")),
        tabPanel("Sea Ice Concentration", leafletOutput("map_ice")),
        tabPanel("Suitability Functions", fluidRow(
          column(4, plotOutput("temp_plot")),
          column(4, plotOutput("chl_plot")),
          column(4, plotOutput("ice_plot"))
        ))
      )
    )
  )
)

server <- function(input, output, session) {
  default_temp_file <- "./glo_freeglorys2v4_rxd_025x7d_8week-moving-average_monthly_CLM_temperature_L1_1999_2017_rotated.nc"
  default_chl_file <- "./glo_freeglorys2v4_rxd_025x7d_8week-moving-average_monthly_CLM_pp_1999_2017_rotated.nc"
  default_ice_file <- "./global_reanalysis_phy_001_030_monthly_CLM_siconc_1999_2017_rotate.nc"
  
  # Load default files if they exist
  temp_file_path <- if (file.exists(default_temp_file)) default_temp_file else NULL
  chl_file_path <- if (file.exists(default_chl_file)) default_chl_file else NULL
  ice_file_path <- if (file.exists(default_ice_file)) default_ice_file else NULL
  
  if (!is.null(temp_file_path) && !is.null(chl_file_path) && !is.null(ice_file_path)) {
    # Read netCDF files to get the time dimension range
    nc_temp <- nc_open(temp_file_path)
    time_dim <- ncvar_get(nc_temp, "time")
    time_units <- ncatt_get(nc_temp, "time", "units")$value
    time_origin <- as.POSIXct(strptime(sub("seconds since ", "", time_units), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    time_dim <- time_origin + as.numeric(time_dim)
    nc_close(nc_temp)
    
    # Update slider range based on time dimension
    output$time_step_slider <- renderUI({
      sliderInput("time_step", "Time Step", min = 1, max = length(time_dim), value = 1, step = 1, 
                  animate = animationOptions(interval = 1000, loop = TRUE))
    })
  }
  
  observeEvent(input$temp_eq, {
    if (input$temp_eq == "Sigmoid") {
      output$temp_params <- renderUI({
        tags$div(style = 'margin-left: 20px;',
                 tagList(
                   sliderInput("temp_midpoint", "Suitability threshold", min = 0, max = 10, value = 3.03, step = 0.1),
                   sliderInput("temp_steepness", "Suitability slope", min = 0, max = 10, value = 2.2, step = 0.1)
                 )
        )
      })
    } else if (input$temp_eq == "Lognormal") {
      output$temp_params <- renderUI({
        tags$div(style = 'margin-left: 20px;',
                 tagList(
                   sliderInput("temp_mean", "Suitability mean", min = 0, max = 10, value = 1, step = 0.1),
                   sliderInput("temp_variance", "Suitability variance", min = 0, max = 10, value = 0, step = 0.1)
                 )
        )
      })
    }
  })
  
  observeEvent(input$chl_eq, {
    if (input$chl_eq == "Sigmoid") {
      output$chl_params <- renderUI({
        tags$div(style = 'margin-left: 20px;',
                 tagList(
                   sliderInput("chl_midpoint", "Suitability threshold", min = 0, max = 10, value = 10, step = 0.1),
                   sliderInput("chl_steepness", "Suitability slope", min = 0, max = 10, value = 1, step = 0.1)
                 )
        )
      })
    } else if (input$chl_eq == "Lognormal") {
      output$chl_params <- renderUI({
        tags$div(style = 'margin-left: 20px;',
                 tagList(
                   sliderInput("chl_mean", "Suitability mean", min = -10, max = 10, value = 1, step = 0.1),
                   sliderInput("chl_variance", "Suitability variance", min = -10, max = 10, value = 0, step = 0.1)
                 )
        )
      })
    } else if (input$chl_eq == "Holling") {
      output$chl_params <- renderUI({
        tags$div(style = 'margin-left: 20px;',
                 tagList(
                   sliderInput("chl_slope", "Suitability slope", min = 0, max = 500, value = 196.3, step = 0.1)
                 )
        )
      })
    }
  })
  
  observeEvent(input$ice_eq, {
    if (input$ice_eq == "Sigmoid") {
      output$ice_params <- renderUI({
        tags$div(style = 'margin-left: 20px;',
                 tagList(
                   sliderInput("ice_midpoint", "Suitability threshold", min = -20, max = 20, value = -10, step = 0.1),
                   sliderInput("ice_steepness", "Suitability slope", min = -10, max = 10, value = 0.4, step = 0.1)
                 )
        )
      })
    } else if (input$ice_eq == "Lognormal") {
      output$ice_params <- renderUI({
        tags$div(style = 'margin-left: 20px;',
                 tagList(
                   sliderInput("ice_mean", "Suitability mean", min = 0, max = 10, value = 1, step = 0.1),
                   sliderInput("ice_variance", "Suitability variance", min = 0, max = 10, value = 0, step = 0.1)
                 )
        )
      })
    } else if (input$ice_eq == "No effect") {
      output$ice_params <- renderUI({
        tags$div(style = 'margin-left: 20px;',
                 tagList(
                   numericInput("ice_suitability", "Ice suitability", value = 1)
                 )
        )
      })
    }
  })
  
  observeEvent({input$temp_file; input$chl_file; input$ice_file}, {
    output$time_step_date <- renderText({
      if (exists("time_dim")) {
        paste("Date: ", format(time_dim[input$time_step], "%Y-%m-%d %H:%M:%S"))
      } else {
        "Date: N/A"
      }
    })
    # Use default file paths if no file is uploaded
    temp_file_path <- if (is.null(input$temp_file)) default_temp_file else input$temp_file$datapath
    chl_file_path <- if (is.null(input$chl_file)) default_chl_file else input$chl_file$datapath
    ice_file_path <- if (is.null(input$ice_file)) default_ice_file else input$ice_file$datapath
    
    # Read netCDF files to get the time dimension range
    nc_temp <- nc_open(temp_file_path)
    time_dim <- ncvar_get(nc_temp, "time")
    time_units <- ncatt_get(nc_temp, "time", "units")$value
    time_origin <- as.POSIXct(strptime(sub("seconds since ", "", time_units), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    time_dim <- time_origin + as.numeric(time_dim)
    nc_close(nc_temp)
    
    # Update slider range based on time dimension
    output$time_step_slider <- renderUI({
      sliderInput("time_step", "Time Step", min = 1, max = length(time_dim), value = 1, step = 1, 
                  animate = animationOptions(interval = 1000, loop = TRUE))
    })
  })
  
  observeEvent(input$time_step, {
    output$time_step_date <- renderText({
      paste("Date: ", format(time_dim[input$time_step], "%Y-%m-%d %H:%M:%S"))
    })
    # Use default file paths if no file is uploaded
    temp_file_path <- if (is.null(input$temp_file)) default_temp_file else input$temp_file$datapath
    chl_file_path <- if (is.null(input$chl_file)) default_chl_file else input$chl_file$datapath
    ice_file_path <- if (is.null(input$ice_file)) default_ice_file else input$ice_file$datapath
    
    # Read netCDF files for the selected timestep
    nc_temp <- nc_open(temp_file_path)
    nc_chl <- nc_open(chl_file_path)
    nc_ice <- nc_open(ice_file_path)
    
    # Extract variables for the selected timestep
    timestep <- input$time_step
    temp_data <- ncvar_get(nc_temp, "dymvar")[,,timestep]
    chl_data <- ncvar_get(nc_chl, "dymvar")[,,timestep]
    ice_data <- ncvar_get(nc_ice, "siconc")[,,timestep]
    ice_data[is.na(ice_data)] <- 0
    
    lon <- nc_temp$dim$lon$vals
    lat <- nc_temp$dim$lat$vals
    # Close the netCDF files
    nc_close(nc_temp)
    nc_close(nc_chl)
    nc_close(nc_ice)
    
    # Calculate suitability using selected equations
    
    source("suitability_functions.R")
    
    temp_data <- switch(input$temp_eq,
                        "Sigmoid" = sigmoid(temp_data, input$temp_midpoint, input$temp_steepness),
                        "Lognormal" = lognormal(temp_data, input$temp_mean, input$temp_variance)
    )
    
    chl_data <- switch(input$chl_eq,
                       "Sigmoid" = sigmoid(chl_data, input$chl_midpoint, input$chl_steepness),
                       "Lognormal" = lognormal(chl_data, input$chl_mean, input$chl_variance),
                       "Holling" = holl(chl_data, input$chl_slope)
    )
    
    ice_data <- switch(input$ice_eq,
                       "Sigmoid" = sigmoid(-1*ice_data, input$ice_midpoint, input$ice_steepness),
                       "Lognormal" = lognormal(ice_data, input$ice_mean, input$ice_variance),
                       "No effect" = 1
    )
    # Calculate habitat suitability
    suitability <- temp_data * sigmoid((chl_data + ice_data) * -1, -0.5, 4)
    suitability[is.na(suitability)] <- NA
    
    # Convert matrices to raster format
    temp_raster <- raster::raster(t(temp_data), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), crs = CRS("+init=epsg:4326"))
    chl_raster <- raster::raster(t(chl_data), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), crs = CRS("+init=epsg:4326"))
    ice_raster <- raster::raster(t(ice_data), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), crs = CRS("+init=epsg:4326"))
    suitability_raster <- raster::raster(t(suitability), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat), crs = CRS("+init=epsg:4326"))
    
    # Render the habitat suitability map
    source("add_legend_decreasing.R")
    output$map_suitability <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addRasterImage(suitability_raster, colors = colorNumeric("viridis", na.omit(values(suitability_raster)), na.color = "transparent")) %>%
        addLegend_decreasing(pal = colorNumeric("viridis", range(na.omit(values(suitability_raster))), na.color = "transparent"),
                              values = range(na.omit(values(suitability_raster))), title = "Habitat Suitability", decreasing = TRUE)
    })
    
    # Render the temperature map
    output$map_temp <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addRasterImage(temp_raster, colors = colorNumeric("plasma", na.omit(values(temp_raster)), na.color = "transparent")) %>%
        addLegend_decreasing(pal = colorNumeric("plasma", range(na.omit(values(temp_raster))), na.color = "transparent"),
                  values = range(na.omit(values(temp_raster))), title = "Temperature", decreasing = TRUE)
    })
    
    # Render the chlorophyll map
    output$map_chl <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addRasterImage(chl_raster, colors = colorNumeric("Greens", na.omit(values(chl_raster)), na.color = "transparent")) %>%
        addLegend_decreasing(pal = colorNumeric("Greens", range(na.omit(values(chl_raster))), na.color = "transparent"),
                  values = range(na.omit(values(chl_raster))), title = "Chlorophyll", decreasing = TRUE)
    })
    
    # Render the sea ice concentration map
    output$map_ice <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addRasterImage(ice_raster, colors = colorNumeric("Blues", na.omit(values(ice_raster)), na.color = "transparent")) %>%
        addLegend_decreasing(pal = colorNumeric("Blues", range(na.omit(values(ice_raster))), na.color = "transparent"),
                  values = range(na.omit(values(ice_raster))), title = "Sea Ice Concentration", decreasing = TRUE)
    })
    
    # Render suitability functions plot for temperature
    output$temp_plot <- renderPlot({
      x_vals <- seq(0, 10, length.out = 100)
      temp_curve <- switch(input$temp_eq,
                           "Sigmoid" = sigmoid(x_vals, input$temp_midpoint, input$temp_steepness),
                           "Lognormal" = lognormal(x_vals, input$temp_mean, input$temp_variance)
      )
      ggplot(data.frame(x = x_vals, y = temp_curve), aes(x = x, y = y)) +
        geom_line(color = "indianred4") +
        ylim(c(0,1)) +
        labs(title = "Thermal suitability", x = "Temperature (C)", y = "Suitability") +
        theme_minimal()
    })
    
    # Render suitability functions plot for chlorophyll
    output$chl_plot <- renderPlot({
      x_vals <- seq(0, 500, length.out = 100)
      chl_curve <- switch(input$chl_eq,
                          "Sigmoid" = sigmoid(x_vals, input$chl_midpoint, input$chl_steepness),
                          "Lognormal" = lognormal(x_vals, input$chl_mean, input$chl_variance),
                          "Holling" = holl(x_vals, input$chl_slope)
      )
      ggplot(data.frame(x = x_vals, y = chl_curve), aes(x = x, y = y)) +
        geom_line(color = "darkgreen") +
        ylim(c(0,1)) +
        labs(title = "NPP (food) suitability", x = "npp", y = "Suitability") +
        theme_minimal()
    })
    
    # Render suitability functions plot for sea ice concentration
    output$ice_plot <- renderPlot({
      x_vals <- seq(0, 1, length.out = 100)
      ice_curve <- switch(input$ice_eq,
                          "Sigmoid" = sigmoid(x_vals, input$ice_midpoint, input$ice_steepness),
                          "Lognormal" = lognormal(x_vals, input$ice_mean, input$ice_variance),
                          "No effect" = rep(1, length(x_vals))
      )
      ggplot(data.frame(x = x_vals, y = ice_curve), aes(x = x, y = y)) +
        geom_line(color = "navyblue") +
        ylim(c(0,1)) +
        labs(title = "Ice fraction (food & refuge) suitability", x = "fractional coverage", y = "Suitability") +
        theme_minimal()
    })
  })
}

shinyApp(ui, server)
