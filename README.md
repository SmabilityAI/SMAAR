# Air Quality Monitoring - R Scripts Guide
## SMAA_002 Device | Guatemala

---

## 📁 Files Overview

### 1. **simplified_air_quality_script.R**
**For:** Beginners and daily checks  
**What it does:**
- Fetches current air quality data
- Gets 24-hour history
- Calculates statistics (average, max, min)
- Creates 3 visualizations
- Exports data to CSV

**Run time:** ~30 seconds

### 2. **air_quality_monitoring_template.R**
**For:** Production reports and advanced analysis  
**What it does:**
- Complete API wrapper functions
- Automated KPI calculations
- Professional visualizations
- Air quality classification system
- Generates full reports

**Run time:** ~1 minute

---

## 🚀 Quick Start

### First Time Setup (Once)
```r
# Install packages
install.packages(c("httr", "jsonlite", "dplyr", "ggplot2", "lubridate", "tidyr", "scales", "gridExtra"))
```

### Every Time You Run
```r
# Load libraries
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# Run either script in RStudio
# File > Open File > Select script > Run
```

---

## 🔌 API Endpoints Used

1. **Latest Data** - Real-time readings
   ```
   ?action=latest&deviceID=SMAA_002
   ```

2. **Historical Data** - Time series
   ```
   ?action=history&deviceID=SMAA_002&hours=24&limit=100
   ```

3. **All Devices** - Device status
   ```
   ?action=devices
   ```

---

## 📊 What the Data Means

### Air Quality Metrics
| Metric | Safe Level | Unhealthy Level |
|--------|-----------|----------------|
| **PM2.5** | < 12 µg/m³ | > 35 µg/m³ |
| **PM10** | < 54 µg/m³ | > 154 µg/m³ |
| **O3** | < 54 ppb | > 70 ppb |
| **CO** | < 4.4 ppm | > 9.4 ppm |

### Device Health
- **Battery:** 0-100%
- **Online:** true/false
- **GPS:** Fixed location (14.618184,-90.553221)

---

## 💡 How It Works

### Simplified Script Flow:
```
1. Fetch latest data → Display current readings
2. Fetch 24h history → Convert timestamps to local time
3. Calculate stats → Mean, max, min for all metrics
4. Create plots → PM2.5 trend, current status, environment
5. Export data → Save CSV files with today's date
6. Generate report → Print summary to console
```

### Template Script Flow:
```
1. Define functions → get_latest_data(), get_historical_data()
2. Calculate KPIs → Air quality status, trends, exceedances
3. Classify levels → Good/Moderate/Unhealthy based on WHO/EPA
4. Create visuals → Dashboard, time series, guidelines comparison
5. Generate report → Complete summary with all metrics
```

---

## 🎯 Key Functions (Template Script)

### Data Fetching
```r
get_latest_data()                          # Current readings
get_historical_data(hours = 24, limit = 100)  # Time series
get_devices_status()                       # All devices
```

### Analysis
```r
calculate_kpis(latest, historical)         # Key metrics
calculate_summary_stats(df)                # Statistics
classify_air_quality(value, pollutant)     # Safety level
```

### Visualization
```r
plot_pollutants_timeseries(df)             # Line charts
plot_current_dashboard(latest)             # Bar chart
plot_pm25_vs_guidelines(df)                # With WHO limits
```

### Reporting
```r
generate_report(hours = 24)                # Complete report
```

---

## 📈 Common Tasks

### Task 1: Check Current Air Quality
```r
# Simplified script - just run it
# Output shows current PM2.5 and status (GOOD/MODERATE/UNHEALTHY)
```

### Task 2: Get Weekly Summary
```r
# Template script - modify this line:
hist_data <- get_historical_data(hours = 168, limit = 500)  # 168 = 7 days
```

### Task 3: Export Data
```r
# Both scripts auto-export to CSV
# Files saved: air_quality_YYYYMMDD.csv, summary_YYYYMMDD.csv
```

### Task 4: Change Visualization Colors
```r
# In plot code, change:
geom_line(color = "red")  # to any color: "blue", "green", etc.
```

---

## ⚠️ Troubleshooting

| Problem | Solution |
|---------|----------|
| "Package not found" | Run `install.packages("package_name")` |
| "Error in GET()" | Check internet connection |
| "No data returned" | Verify SMAA_002 is online, reduce hours parameter |
| "Plot not showing" | Make sure ggplot2 is loaded |
| Deprecated `size` warning | Change `size` to `linewidth` in geom_line() |

---

## 📝 Customization Examples

### Change Time Range
```r
# Instead of 24 hours:
hours = 6      # Last 6 hours
hours = 48     # Last 2 days
hours = 168    # Last week
```

### Modify Thresholds
```r
# Custom PM2.5 safe level:
my_safe_level <- 20  # Instead of WHO's 12
unsafe <- sum(df$pm25 > my_safe_level)
```

### Add Your Own Plot
```r
# Copy existing plot code and change variable:
ggplot(df, aes(x = datetime, y = o3)) +  # Changed from pm25 to o3
  geom_line(color = "blue", linewidth = 1) +
  labs(title = "Ozone Levels", y = "O3 (ppb)")
```

---

## 📤 Output Files

Both scripts create:
- **air_quality_YYYYMMDD.csv** - All historical data
- **summary_YYYYMMDD.csv** - Statistics table
- **Console output** - Report printed to screen
- **Plots** - Displayed in RStudio viewer

To save plots:
```r
ggsave("my_plot.png", width = 10, height = 6, dpi = 300)
```

---

## 🎓 Learning Path

1. **Day 1:** Run simplified script, understand output
2. **Day 2:** Modify time ranges, try different plots
3. **Day 3:** Run template script, explore functions
4. **Week 2:** Customize for your needs, automate reports
5. **Month 1:** Build dashboards, integrate with workflows

---

## 📞 Support

**Device Status:** SMAA_002 is online ✓  
**Location:** Guatemala (14.618184,-90.553221)  
**Timezone:** America/Guatemala (GMT-6)  
**Update Frequency:** ~60 seconds

**Quick Check Device:**
```r
url <- "https://jciiy1ok97.execute-api.us-east-1.amazonaws.com/default/getData?action=latest&deviceID=SMAA_002"
data <- fromJSON(content(GET(url), "text"))
data$data$online  # Should return TRUE
```

---

**Version:** 1.0 | **Last Updated:** February 2026 | **Device:** SMAA_002
