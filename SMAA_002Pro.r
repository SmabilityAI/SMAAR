# ============================================================================
# AIR QUALITY MONITORING - R TEMPLATE FOR SMAA_002
# Bootcamp Training Material - Guatemala Client
# ============================================================================
# This template demonstrates how to:
# 1. Fetch data from air quality APIs
# 2. Process and analyze air quality metrics
# 3. Calculate KPIs and indicators
# 4. Create visualizations for reporting
# ============================================================================

# REQUIRED PACKAGES ----
# Install packages if needed:
# install.packages(c("httr", "jsonlite", "dplyr", "ggplot2", "lubridate",
#                    "tidyr", "scales", "gridExtra", "knitr"))

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
library(gridExtra)

# API CONFIGURATION ----
BASE_URL <- "https://jciiy1ok97.execute-api.us-east-1.amazonaws.com/default/getData"
DEVICE_ID <- "SMAA_002"

# AIR QUALITY STANDARDS (WHO & EPA) ----
# These thresholds help categorize air quality levels
AQ_STANDARDS <- list(
  pm25 = list(
    good = 12,
    moderate = 35.4,
    unhealthy_sensitive = 55.4,
    unhealthy = 150.4,
    very_unhealthy = 250.4
  ),
  pm10 = list(
    good = 54,
    moderate = 154,
    unhealthy_sensitive = 254,
    unhealthy = 354,
    very_unhealthy = 424
  ),
  o3 = list(
    good = 54,
    moderate = 70,
    unhealthy_sensitive = 85,
    unhealthy = 105,
    very_unhealthy = 200
  ),
  co = list(
    good = 4.4,
    moderate = 9.4,
    unhealthy_sensitive = 12.4,
    unhealthy = 15.4,
    very_unhealthy = 30.4
  )
)

# ============================================================================
# SECTION 1: DATA FETCHING FUNCTIONS
# ============================================================================

#' Fetch Latest Data from SMAA_002
#' @return List with current readings
get_latest_data <- function() {
  url <- paste0(BASE_URL, "?action=latest&deviceID=", DEVICE_ID)

  response <- GET(url)

  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(data)
  } else {
    stop(paste("Error fetching data. Status code:", status_code(response)))
  }
}

#' Fetch All Devices Status
#' @return Data frame with all devices information
get_devices_status <- function() {
  url <- paste0(BASE_URL, "?action=devices")

  response <- GET(url)

  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(data)
  } else {
    stop(paste("Error fetching devices. Status code:", status_code(response)))
  }
}

#' Fetch Historical Data
#' @param device_id Device identifier
#' @param hours Number of hours of history (default: 24)
#' @param limit Maximum number of records (default: 100)
#' @return Data frame with historical readings
get_historical_data <- function(device_id = DEVICE_ID, hours = 24, limit = 100) {
  url <- paste0(BASE_URL,
                "?action=history&deviceID=", device_id,
                "&hours=", hours,
                "&limit=", limit)

  response <- GET(url)

  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))

    # Convert to data frame and process timestamps
    if (!is.null(data$data) && length(data$data) > 0) {
      df <- as.data.frame(data$data)
      df$datetime <- as.POSIXct(df$timestamp, origin = "1970-01-01", tz = "America/Guatemala")
      return(df)
    } else {
      warning("No historical data available")
      return(NULL)
    }
  } else {
    stop(paste("Error fetching history. Status code:", status_code(response)))
  }
}

# ============================================================================
# SECTION 2: DATA PROCESSING & KPI CALCULATION
# ============================================================================

#' Classify Air Quality Level
#' @param value Measured value
#' @param pollutant Type of pollutant (pm25, pm10, o3, co)
#' @return Character string with quality level
classify_air_quality <- function(value, pollutant) {
  standards <- AQ_STANDARDS[[pollutant]]

  if (is.null(standards)) return("Unknown")

  if (value <= standards$good) return("Good")
  else if (value <= standards$moderate) return("Moderate")
  else if (value <= standards$unhealthy_sensitive) return("Unhealthy for Sensitive Groups")
  else if (value <= standards$unhealthy) return("Unhealthy")
  else if (value <= standards$very_unhealthy) return("Very Unhealthy")
  else return("Hazardous")
}

#' Calculate Summary Statistics
#' @param df Historical data frame
#' @return Data frame with summary statistics
calculate_summary_stats <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  summary_stats <- df %>%
    summarise(
      # Particulate Matter
      pm25_mean = mean(pm25, na.rm = TRUE),
      pm25_max = max(pm25, na.rm = TRUE),
      pm25_min = min(pm25, na.rm = TRUE),
      pm10_mean = mean(pm10, na.rm = TRUE),
      pm10_max = max(pm10, na.rm = TRUE),
      pm10_min = min(pm10, na.rm = TRUE),

      # Gases
      o3_mean = mean(o3, na.rm = TRUE),
      o3_max = max(o3, na.rm = TRUE),
      co_mean = mean(co, na.rm = TRUE),
      co_max = max(co, na.rm = TRUE),

      # Environmental
      temp_mean = mean(temperature, na.rm = TRUE),
      temp_max = max(temperature, na.rm = TRUE),
      temp_min = min(temperature, na.rm = TRUE),
      humidity_mean = mean(humidity, na.rm = TRUE),
      noise_mean = mean(noise, na.rm = TRUE),
      noise_max = max(noise, na.rm = TRUE),

      # Device Health
      battery_mean = mean(battery, na.rm = TRUE),
      readings_count = n()
    )

  return(summary_stats)
}

#' Calculate Key Performance Indicators (KPIs)
#' @param latest Latest reading data
#' @param historical Historical data frame
#' @return List of KPIs
calculate_kpis <- function(latest, historical) {
  kpis <- list()

  # Current readings
  kpis$current_pm25 <- latest$data$pm25
  kpis$current_pm10 <- latest$data$pm10
  kpis$current_temp <- latest$data$temperature
  kpis$current_humidity <- latest$data$humidity

  # Air quality classification
  kpis$pm25_status <- classify_air_quality(latest$data$pm25, "pm25")
  kpis$pm10_status <- classify_air_quality(latest$data$pm10, "pm10")
  kpis$o3_status <- classify_air_quality(latest$data$o3, "o3")
  kpis$co_status <- classify_air_quality(latest$data$co, "co")

  # Device health
  kpis$battery_level <- latest$data$battery
  kpis$device_online <- latest$data$online
  kpis$last_seen <- latest$data$last_seen_seconds

  # 24-hour trends (if historical data available)
  if (!is.null(historical) && nrow(historical) > 0) {
    kpis$pm25_24h_avg <- mean(historical$pm25, na.rm = TRUE)
    kpis$pm25_24h_max <- max(historical$pm25, na.rm = TRUE)
    kpis$exceedances_pm25 <- sum(historical$pm25 > AQ_STANDARDS$pm25$moderate, na.rm = TRUE)

    # Calculate trend (increasing/decreasing)
    if (nrow(historical) >= 2) {
      recent_avg <- mean(tail(historical$pm25, 6), na.rm = TRUE)
      older_avg <- mean(head(historical$pm25, 6), na.rm = TRUE)
      kpis$pm25_trend <- ifelse(recent_avg > older_avg, "Increasing", "Decreasing")
    }
  }

  # GPS location
  kpis$location <- latest$data$fixed_gps

  return(kpis)
}

# ============================================================================
# SECTION 3: VISUALIZATION FUNCTIONS
# ============================================================================

#' Plot Time Series for Multiple Pollutants
#' @param df Historical data frame
#' @return ggplot object
plot_pollutants_timeseries <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  # Reshape data for plotting
  df_long <- df %>%
    select(datetime, pm25, pm10, o3, co) %>%
    pivot_longer(cols = c(pm25, pm10, o3, co),
                 names_to = "pollutant",
                 values_to = "value")

  # Create faceted time series
  p <- ggplot(df_long, aes(x = datetime, y = value, color = pollutant)) +
    geom_line(size = 1) +
    geom_point(size = 2, alpha = 0.6) +
    facet_wrap(~pollutant, scales = "free_y", ncol = 2) +
    scale_color_manual(values = c("pm25" = "#e74c3c", "pm10" = "#e67e22",
                                   "o3" = "#3498db", "co" = "#95a5a6")) +
    labs(title = paste("Air Quality Trends -", DEVICE_ID),
         subtitle = paste("Period:", min(df$datetime), "to", max(df$datetime)),
         x = "Time",
         y = "Concentration",
         caption = "Source: SMAA Air Quality Network") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 14),
          strip.text = element_text(face = "bold"))

  return(p)
}

#' Plot Current Air Quality Dashboard
#' @param latest Latest reading data
#' @return ggplot object
plot_current_dashboard <- function(latest) {
  current <- latest$data

  # Create data frame for current readings
  metrics <- data.frame(
    pollutant = c("PM2.5", "PM10", "O3", "CO"),
    value = c(current$pm25, current$pm10, current$o3, current$co),
    status = c(
      classify_air_quality(current$pm25, "pm25"),
      classify_air_quality(current$pm10, "pm10"),
      classify_air_quality(current$o3, "o3"),
      classify_air_quality(current$co, "co")
    )
  )

  # Color mapping for status
  status_colors <- c(
    "Good" = "#27ae60",
    "Moderate" = "#f39c12",
    "Unhealthy for Sensitive Groups" = "#e67e22",
    "Unhealthy" = "#e74c3c",
    "Very Unhealthy" = "#8e44ad",
    "Hazardous" = "#7f8c8d"
  )

  p <- ggplot(metrics, aes(x = pollutant, y = value, fill = status)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = paste0(round(value, 1), "\n", status)),
              vjust = -0.5, size = 3.5, fontface = "bold") +
    scale_fill_manual(values = status_colors) +
    labs(title = "Current Air Quality Status",
         subtitle = paste("Device:", DEVICE_ID, "| Time:",
                         as.POSIXct(current$timestamp, origin = "1970-01-01")),
         x = "Pollutant",
         y = "Concentration",
         fill = "Status") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14),
          legend.position = "bottom")

  return(p)
}

#' Plot Environmental Conditions
#' @param df Historical data frame
#' @return ggplot object
plot_environmental_conditions <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  # Temperature plot
  p1 <- ggplot(df, aes(x = datetime, y = temperature)) +
    geom_line(color = "#e74c3c", size = 1) +
    geom_point(color = "#c0392b", size = 2, alpha = 0.6) +
    labs(title = "Temperature", x = "", y = "°C") +
    theme_minimal()

  # Humidity plot
  p2 <- ggplot(df, aes(x = datetime, y = humidity)) +
    geom_line(color = "#3498db", size = 1) +
    geom_point(color = "#2980b9", size = 2, alpha = 0.6) +
    labs(title = "Humidity", x = "", y = "%") +
    theme_minimal()

  # Noise plot
  p3 <- ggplot(df, aes(x = datetime, y = noise)) +
    geom_line(color = "#95a5a6", size = 1) +
    geom_point(color = "#7f8c8d", size = 2, alpha = 0.6) +
    labs(title = "Noise Level", x = "Time", y = "dB") +
    theme_minimal()

  # Combine plots
  combined <- grid.arrange(p1, p2, p3, ncol = 1,
                          top = "Environmental Conditions")

  return(combined)
}

#' Plot PM2.5 vs WHO Guidelines
#' @param df Historical data frame
#' @return ggplot object
plot_pm25_vs_guidelines <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  p <- ggplot(df, aes(x = datetime, y = pm25)) +
    geom_line(size = 1.2, color = "#e74c3c") +
    geom_point(size = 2, alpha = 0.6, color = "#c0392b") +
    geom_hline(yintercept = AQ_STANDARDS$pm25$good,
               linetype = "dashed", color = "#27ae60", size = 1) +
    geom_hline(yintercept = AQ_STANDARDS$pm25$moderate,
               linetype = "dashed", color = "#f39c12", size = 1) +
    annotate("text", x = min(df$datetime), y = AQ_STANDARDS$pm25$good,
             label = "Good (12 µg/m³)", hjust = 0, vjust = -0.5, size = 3) +
    annotate("text", x = min(df$datetime), y = AQ_STANDARDS$pm25$moderate,
             label = "Moderate (35.4 µg/m³)", hjust = 0, vjust = -0.5, size = 3) +
    labs(title = "PM2.5 Levels vs WHO/EPA Guidelines",
         x = "Time",
         y = "PM2.5 (µg/m³)",
         caption = "Dashed lines indicate WHO/EPA air quality thresholds") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14))

  return(p)
}

#' Create Summary Statistics Table
#' @param stats Summary statistics data frame
#' @return Formatted data frame
format_summary_table <- function(stats) {
  if (is.null(stats)) {
    return(NULL)
  }

  summary_table <- data.frame(
    Metric = c("PM2.5 Average", "PM2.5 Maximum", "PM2.5 Minimum",
               "PM10 Average", "PM10 Maximum", "PM10 Minimum",
               "O3 Average", "O3 Maximum",
               "CO Average", "CO Maximum",
               "Temperature Average", "Temperature Range",
               "Humidity Average", "Noise Average", "Noise Maximum",
               "Battery Level", "Total Readings"),
    Value = c(
      sprintf("%.1f µg/m³", stats$pm25_mean),
      sprintf("%.1f µg/m³", stats$pm25_max),
      sprintf("%.1f µg/m³", stats$pm25_min),
      sprintf("%.1f µg/m³", stats$pm10_mean),
      sprintf("%.1f µg/m³", stats$pm10_max),
      sprintf("%.1f µg/m³", stats$pm10_min),
      sprintf("%.1f ppb", stats$o3_mean),
      sprintf("%.1f ppb", stats$o3_max),
      sprintf("%.1f ppm", stats$co_mean),
      sprintf("%.1f ppm", stats$co_max),
      sprintf("%.1f °C", stats$temp_mean),
      sprintf("%.1f - %.1f °C", stats$temp_min, stats$temp_max),
      sprintf("%.1f %%", stats$humidity_mean),
      sprintf("%.1f dB", stats$noise_mean),
      sprintf("%.1f dB", stats$noise_max),
      sprintf("%.0f %%", stats$battery_mean),
      sprintf("%d", stats$readings_count)
    )
  )

  return(summary_table)
}

# ============================================================================
# SECTION 4: REPORT GENERATION
# ============================================================================

#' Generate Complete Air Quality Report
#' @param hours Number of hours of historical data (default: 24)
#' @return List containing all report components
generate_report <- function(hours = 24) {
  cat("=== GENERATING AIR QUALITY REPORT ===\n\n")

  # Fetch data
  cat("1. Fetching latest data...\n")
  latest <- get_latest_data()

  cat("2. Fetching historical data (", hours, "hours)...\n")
  historical <- get_historical_data(hours = hours, limit = 100)

  # Calculate metrics
  cat("3. Calculating KPIs...\n")
  kpis <- calculate_kpis(latest, historical)

  cat("4. Calculating summary statistics...\n")
  stats <- calculate_summary_stats(historical)

  # Print KPI summary
  cat("\n=== KEY PERFORMANCE INDICATORS ===\n")
  cat(sprintf("Device: %s\n", DEVICE_ID))
  cat(sprintf("Status: %s\n", ifelse(kpis$device_online, "ONLINE ✓", "OFFLINE ✗")))
  cat(sprintf("Battery: %d%%\n", kpis$battery_level))
  cat(sprintf("Location: %s\n", kpis$location))
  cat(sprintf("\nCurrent PM2.5: %.1f µg/m³ [%s]\n", kpis$current_pm25, kpis$pm25_status))
  cat(sprintf("Current PM10: %.1f µg/m³ [%s]\n", kpis$current_pm10, kpis$pm10_status))
  cat(sprintf("Current Temperature: %.1f °C\n", kpis$current_temp))
  cat(sprintf("Current Humidity: %.1f %%\n\n", kpis$current_humidity))

  if (!is.null(kpis$pm25_24h_avg)) {
    cat(sprintf("24h PM2.5 Average: %.1f µg/m³\n", kpis$pm25_24h_avg))
    cat(sprintf("24h PM2.5 Maximum: %.1f µg/m³\n", kpis$pm25_24h_max))
    cat(sprintf("Exceedances (>35.4): %d times\n", kpis$exceedances_pm25))
    if (!is.null(kpis$pm25_trend)) {
      cat(sprintf("Trend: %s\n", kpis$pm25_trend))
    }
  }

  # Create visualizations
  cat("\n5. Generating visualizations...\n")
  plots <- list()
  plots$dashboard <- plot_current_dashboard(latest)
  plots$timeseries <- plot_pollutants_timeseries(historical)
  plots$pm25_guidelines <- plot_pm25_vs_guidelines(historical)
  plots$environmental <- plot_environmental_conditions(historical)

  # Create summary table
  summary_table <- format_summary_table(stats)

  cat("\n=== REPORT GENERATION COMPLETE ===\n\n")

  # Return all components
  return(list(
    latest = latest,
    historical = historical,
    kpis = kpis,
    stats = stats,
    summary_table = summary_table,
    plots = plots
  ))
}

# ============================================================================
# SECTION 5: EXAMPLE USAGE
# ============================================================================

# BASIC EXAMPLE: Fetch and display current data
cat("\n=== EXAMPLE 1: Fetch Latest Data ===\n")
latest_data <- get_latest_data()
print(latest_data)

# EXAMPLE: Fetch 24-hour historical data
cat("\n=== EXAMPLE 2: Fetch Historical Data ===\n")
hist_24h <- get_historical_data(hours = 24, limit = 50)
if (!is.null(hist_24h)) {
  cat(sprintf("Retrieved %d readings from the past 24 hours\n", nrow(hist_24h)))
  print(head(hist_24h))
}

# EXAMPLE: Generate complete report
cat("\n=== EXAMPLE 3: Generate Complete Report ===\n")
report <- generate_report(hours = 24)

# Display summary table
cat("\n=== SUMMARY STATISTICS TABLE ===\n")
print(report$summary_table)

# Display visualizations
cat("\nDisplaying visualizations...\n")
print(report$plots$dashboard)
print(report$plots$timeseries)
print(report$plots$pm25_guidelines)

# EXAMPLE: Export data to CSV
cat("\n=== EXAMPLE 4: Export Data ===\n")
if (!is.null(report$historical)) {
  output_file <- paste0("air_quality_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
  write.csv(report$historical, output_file, row.names = FALSE)
  cat(sprintf("Data exported to: %s\n", output_file))
}

# EXAMPLE: Save summary table
summary_file <- paste0("air_quality_summary_", format(Sys.Date(), "%Y%m%d"), ".csv")
write.csv(report$summary_table, summary_file, row.names = FALSE)
cat(sprintf("Summary exported to: %s\n", summary_file))

# ============================================================================
# SECTION 6: ADVANCED EXAMPLES & CUSTOM ANALYSIS
# ============================================================================

#' Calculate hourly averages
#' @param df Historical data frame
#' @return Data frame with hourly aggregates
calculate_hourly_averages <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  hourly <- df %>%
    mutate(hour = floor_date(datetime, "hour")) %>%
    group_by(hour) %>%
    summarise(
      pm25_avg = mean(pm25, na.rm = TRUE),
      pm10_avg = mean(pm10, na.rm = TRUE),
      o3_avg = mean(o3, na.rm = TRUE),
      co_avg = mean(co, na.rm = TRUE),
      temp_avg = mean(temperature, na.rm = TRUE),
      humidity_avg = mean(humidity, na.rm = TRUE),
      readings = n()
    )

  return(hourly)
}

# EXAMPLE: Hourly analysis
cat("\n=== EXAMPLE 5: Hourly Averages ===\n")
hourly_data <- calculate_hourly_averages(report$historical)
if (!is.null(hourly_data)) {
  print(hourly_data)

  # Plot hourly PM2.5
  p_hourly <- ggplot(hourly_data, aes(x = hour, y = pm25_avg)) +
    geom_bar(stat = "identity", fill = "#3498db", alpha = 0.7) +
    geom_line(color = "#e74c3c", size = 1) +
    labs(title = "Hourly PM2.5 Averages",
         x = "Hour",
         y = "PM2.5 (µg/m³)") +
    theme_minimal()

  print(p_hourly)
}

# ============================================================================
# NOTES FOR BOOTCAMP PARTICIPANTS
# ============================================================================
#
# KEY TAKEAWAYS:
# 1. Always check API response status codes before processing data
# 2. Convert timestamps to proper datetime objects for analysis
# 3. Use air quality standards to classify readings
# 4. Create reusable functions for common tasks
# 5. Visualize data to identify patterns and trends
# 6. Export data for further analysis or reporting
#
# NEXT STEPS:
# - Customize visualizations based on your needs
# - Set up automated reports (e.g., daily/weekly)
# - Add email alerts for poor air quality
# - Integrate with databases for long-term storage
# - Create Shiny dashboard for real-time monitoring
#
# RESOURCES:
# - WHO Air Quality Guidelines: https://www.who.int/air-pollution
# - EPA AQI Information: https://www.epa.gov/aqi
# - R for Data Science: https://r4ds.had.co.nz/
# ============================================================================

cat("\n=== TEMPLATE EXECUTION COMPLETE ===\n")
cat("All examples have been executed successfully!\n")
cat("Modify the code above to customize for your specific needs.\n\n")
