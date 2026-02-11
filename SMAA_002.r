# ============================================================================
# SIMPLIFIED AIR QUALITY MONITORING SCRIPT
# Beginner-Friendly Version for Bootcamp Participants
# Device: SMAA_002 (Guatemala)
# ============================================================================

# ============================================================================
# STEP 1: INSTALL AND LOAD PACKAGES
# ============================================================================
# Run these lines ONCE to install packages:
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("dplyr")
# install.packages("ggplot2")

# Load the packages (run every time you start R)
library(httr)      # For API calls
library(jsonlite)  # For JSON data
library(dplyr)     # For data manipulation
library(ggplot2)   # For plotting

# ============================================================================
# STEP 2: SET UP YOUR CONFIGURATION
# ============================================================================

# API Base URL - DO NOT CHANGE
api_url <- "https://jciiy1ok97.execute-api.us-east-1.amazonaws.com/default/getData"

# Your device ID
device_id <- "SMAA_002"

# ============================================================================
# STEP 3: GET CURRENT (LATEST) DATA
# ============================================================================

cat("\n=== FETCHING CURRENT DATA ===\n")

# Build the URL for latest data
latest_url <- paste0(api_url, "?action=latest&deviceID=", device_id)

# Make the API call
latest_response <- GET(latest_url)

# Convert JSON to R data
latest_data <- fromJSON(content(latest_response, "text", encoding = "UTF-8"))

# Display the data
cat("\nDevice:", latest_data$deviceID, "\n")
cat("Online:", latest_data$data$online, "\n")
cat("Battery:", latest_data$data$battery, "%\n")
cat("\n--- Air Quality ---\n")
cat("PM2.5:", latest_data$data$pm25, "µg/m³\n")
cat("PM10:", latest_data$data$pm10, "µg/m³\n")
cat("O3:", latest_data$data$o3, "ppb\n")
cat("CO:", latest_data$data$co, "ppb\n")
cat("\n--- Environment ---\n")
cat("Temperature:", latest_data$data$temperature, "°C\n")
cat("Humidity:", latest_data$data$humidity, "%\n")
cat("Noise:", latest_data$data$noise, "dB\n")

# ============================================================================
# STEP 4: GET HISTORICAL DATA (Last 24 hours)
# ============================================================================

cat("\n=== FETCHING HISTORICAL DATA ===\n")

# Build the URL for historical data
# You can change 'hours' and 'limit' parameters
history_url <- paste0(api_url,
                     "?action=history",
                     "&deviceID=", device_id,
                     "&hours=24",    # Last 24 hours
                     "&limit=100")   # Max 100 readings

# Make the API call
history_response <- GET(history_url)

# Convert JSON to R data
history_data <- fromJSON(content(history_response, "text", encoding = "UTF-8"))

# Convert to data frame for easier analysis
df <- as.data.frame(history_data$data)

# Add a readable date/time column
df$datetime <- as.POSIXct(df$timestamp,
                         origin = "1970-01-01",
                         tz = "America/Guatemala")

cat("Total readings retrieved:", nrow(df), "\n")
cat("Time range:", min(df$datetime), "to", max(df$datetime), "\n")

# Show first few rows
cat("\nFirst 5 readings:\n")
print(head(df[, c("datetime", "pm25", "pm10", "temperature", "humidity")], 5))

# ============================================================================
# STEP 5: CALCULATE BASIC STATISTICS
# ============================================================================

cat("\n=== CALCULATING STATISTICS ===\n")

# PM2.5 Statistics
pm25_avg <- mean(df$pm25, na.rm = TRUE)
pm25_max <- max(df$pm25, na.rm = TRUE)
pm25_min <- min(df$pm25, na.rm = TRUE)

cat("\nPM2.5 Statistics (Last 24 hours):\n")
cat("Average:", round(pm25_avg, 2), "µg/m³\n")
cat("Maximum:", pm25_max, "µg/m³\n")
cat("Minimum:", pm25_min, "µg/m³\n")

# Check air quality level
if (pm25_avg < 12) {
  cat("Status: GOOD ✓ (Safe to breathe)\n")
} else if (pm25_avg < 35.4) {
  cat("Status: MODERATE ⚠ (Acceptable for most people)\n")
} else {
  cat("Status: UNHEALTHY ✗ (Sensitive groups should limit outdoor activity)\n")
}

# Count how many times PM2.5 was above safe level
unsafe_count <- sum(df$pm25 > 12)
cat("Readings above safe level (12 µg/m³):", unsafe_count, "out of", nrow(df), "\n")

# Temperature Statistics
cat("\nTemperature Statistics:\n")
cat("Average:", round(mean(df$temperature), 1), "°C\n")
cat("Range:", round(min(df$temperature), 1), "°C to", round(max(df$temperature), 1), "°C\n")

# ============================================================================
# STEP 6: CREATE A SIMPLE PLOT
# ============================================================================

cat("\n=== CREATING VISUALIZATION ===\n")

# Plot 1: PM2.5 over time
plot1 <- ggplot(df, aes(x = datetime, y = pm25)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "darkred", size = 2) +
  geom_hline(yintercept = 12, linetype = "dashed", color = "green") +
  labs(title = paste("PM2.5 Levels -", device_id),
       subtitle = "Last 24 Hours",
       x = "Time",
       y = "PM2.5 (µg/m³)",
       caption = "Green line = WHO safe level (12 µg/m³)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(plot1)

# Plot 2: Current pollutant levels (Bar chart)
current_levels <- data.frame(
  Pollutant = c("PM2.5", "PM10", "O3", "CO"),
  Value = c(latest_data$data$pm25,
           latest_data$data$pm10,
           latest_data$data$o3,
           latest_data$data$co),
  Unit = c("µg/m³", "µg/m³", "ppb", "ppb")
)

plot2 <- ggplot(current_levels, aes(x = Pollutant, y = Value, fill = Pollutant)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Value, " ", Unit)), vjust = -0.5) +
  labs(title = "Current Air Quality Readings",
       subtitle = format(Sys.time(), "%Y-%m-%d %H:%M"),
       y = "Concentration") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(plot2)

# Plot 3: Temperature and Humidity
plot3 <- ggplot(df, aes(x = datetime)) +
  geom_line(aes(y = temperature, color = "Temperature"), linewidth = 1) +
  geom_line(aes(y = humidity, color = "Humidity"), linewidth = 1) +
  scale_color_manual(values = c("Temperature" = "red", "Humidity" = "blue")) +
  labs(title = "Environmental Conditions",
       x = "Time",
       y = "Value",
       color = "Metric") +
  theme_minimal()

print(plot3)

# ============================================================================
# STEP 7: EXPORT DATA TO CSV
# ============================================================================

cat("\n=== EXPORTING DATA ===\n")

# Create filename with today's date
output_filename <- paste0("air_quality_",
                         format(Sys.Date(), "%Y%m%d"),
                         ".csv")

# Save to CSV
write.csv(df, output_filename, row.names = FALSE)
cat("Data saved to:", output_filename, "\n")

# Create summary report
summary_data <- data.frame(
  Metric = c("Average PM2.5", "Max PM2.5", "Min PM2.5",
             "Average Temperature", "Average Humidity",
             "Total Readings", "Unsafe Readings (PM2.5 > 12)"),
  Value = c(
    round(pm25_avg, 2),
    pm25_max,
    pm25_min,
    round(mean(df$temperature), 1),
    round(mean(df$humidity), 1),
    nrow(df),
    unsafe_count
  )
)

summary_filename <- paste0("summary_",
                          format(Sys.Date(), "%Y%m%d"),
                          ".csv")
write.csv(summary_data, summary_filename, row.names = FALSE)
cat("Summary saved to:", summary_filename, "\n")

# ============================================================================
# STEP 8: CREATE A SIMPLE REPORT FUNCTION
# ============================================================================

# This function generates a quick daily report
generate_daily_report <- function() {

  cat("\n")
  cat("====================================================\n")
  cat("       DAILY AIR QUALITY REPORT - SMAA_002         \n")
  cat("====================================================\n")
  cat("Date:", format(Sys.Date(), "%B %d, %Y"), "\n")
  cat("Time:", format(Sys.time(), "%H:%M"), "\n")
  cat("----------------------------------------------------\n")

  # Get latest data
  url <- paste0(api_url, "?action=latest&deviceID=", device_id)
  data <- fromJSON(content(GET(url), "text"))

  cat("\nDEVICE STATUS:\n")
  cat("  Device ID:", data$deviceID, "\n")
  cat("  Online:", ifelse(data$data$online, "YES ✓", "NO ✗"), "\n")
  cat("  Battery:", data$data$battery, "%\n")
  cat("  Location:", data$data$fixed_gps, "\n")

  cat("\nCURRENT AIR QUALITY:\n")
  cat("  PM2.5:", data$data$pm25, "µg/m³")
  if (data$data$pm25 < 12) {
    cat(" [GOOD ✓]\n")
  } else if (data$data$pm25 < 35.4) {
    cat(" [MODERATE ⚠]\n")
  } else {
    cat(" [UNHEALTHY ✗]\n")
  }

  cat("  PM10:", data$data$pm10, "µg/m³\n")
  cat("  O3:", data$data$o3, "ppb\n")
  cat("  CO:", data$data$co, "ppb\n")

  cat("\nENVIRONMENT:\n")
  cat("  Temperature:", data$data$temperature, "°C\n")
  cat("  Humidity:", data$data$humidity, "%\n")
  cat("  Noise:", data$data$noise, "dB\n")

  cat("====================================================\n\n")
}

# Run the report function
generate_daily_report()

# ============================================================================
# PRACTICE EXERCISES FOR BOOTCAMP
# ============================================================================

cat("\n=== PRACTICE EXERCISES ===\n\n")

cat("EXERCISE 1: Modify the code above to get data from the last 6 hours instead of 24 hours\n")
cat("Hint: Change the 'hours=24' parameter to 'hours=6'\n\n")

cat("EXERCISE 2: Calculate the average O3 (ozone) level\n")
cat("Hint: Use mean(df$o3)\n\n")

cat("EXERCISE 3: Create a plot showing humidity over time\n")
cat("Hint: Copy the PM2.5 plot code and change 'pm25' to 'humidity'\n\n")

cat("EXERCISE 4: Count how many readings have temperature above 30°C\n")
cat("Hint: Use sum(df$temperature > 30)\n\n")

cat("EXERCISE 5: Find the time when PM2.5 was at its maximum\n")
cat("Hint: Use df$datetime[which.max(df$pm25)]\n\n")

# ============================================================================
# BONUS: HOURLY SUMMARY
# ============================================================================

cat("\n=== BONUS: HOURLY SUMMARY ===\n")

# Group data by hour
library(lubridate)

df$hour <- floor_date(df$datetime, "hour")

hourly_summary <- df %>%
  group_by(hour) %>%
  summarise(
    avg_pm25 = round(mean(pm25), 1),
    avg_temp = round(mean(temperature), 1),
    readings = n()
  )

cat("\nHourly averages:\n")
print(hourly_summary)

# Plot hourly averages
hourly_plot <- ggplot(hourly_summary, aes(x = hour, y = avg_pm25)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 12, linetype = "dashed", color = "red") +
  labs(title = "Hourly Average PM2.5",
       x = "Hour",
       y = "PM2.5 (µg/m³)") +
  theme_minimal()

print(hourly_plot)

# ============================================================================
# COMPLETED!
# ============================================================================

cat("\n=== SCRIPT COMPLETED SUCCESSFULLY ===\n")
cat("You have successfully:\n")
cat("  ✓ Fetched current data from the API\n")
cat("  ✓ Retrieved historical data\n")
cat("  ✓ Calculated statistics and KPIs\n")
cat("  ✓ Created visualizations\n")
cat("  ✓ Exported data to CSV files\n")
cat("  ✓ Generated a daily report\n\n")
cat("Next steps:\n")
cat("  - Try the practice exercises\n")
cat("  - Customize the plots with your own styles\n")
cat("  - Set up automated daily reports\n")
cat("  - Explore the advanced template for more features\n\n")
