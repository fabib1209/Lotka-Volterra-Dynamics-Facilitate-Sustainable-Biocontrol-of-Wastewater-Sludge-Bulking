# Time Lag Analysis

Time lag analysis was employed to disentangle temporal dependencies between Arcella spp. and Ca. M. parvicella. The lag2.plot function from the astsa package v. 2.1 (Stoffer and Poison, 2024) was used to visualize the relationship between these variables at different time lags. This analysis calculated correlations between the y-variable (Arcella spp.) at time t and the x-variable (Ca. M. parvicella) at prior time points, identifying the optimal lag (in months) based on the highest observed correlation values (Fig. 4A, Fig. S11).

Here we show how time lag analysis was performed using astsa and lag2.plot.

# 01 Creating Time-series objects 

We need time-series objects for the analysis in astsa. Here is how they were created.
First from the replicated dataset the monthly averages get calculated for every variable and they get put at the 1st of the month.

```

# LOADING PACKAGES #########
library(astsa)


## LOADING DATA ########

source("LOADING_DATA_.R")


## create monthly sampling averages  ########

# Ensure the Date column is in Date format
combined_filtered_data$Date <- as.Date(combined_filtered_data$date)

# Extract Year and Month from the Date column
combined_filtered_data$Year <- format(combined_filtered_data$Date, "%Y")
combined_filtered_data$Month <- format(combined_filtered_data$Date, "%m")

# Group by YearMonth and calculate the monthly averages
monthly_combined_data <- combined_filtered_data %>%
  group_by(Year, Month) %>%
  summarise(across(c(microthrix_percent, arcella_percent, typ_0041_percent, Temperature),
                   mean, na.rm = TRUE))

# get a date column back
monthly_combined_data$date <- as.Date(paste0(monthly_combined_data$Year, "-", 
                                             monthly_combined_data$Month, "-01"))  # Assume day 1 for each month



## creating TS (TimeSeries) data ########

# Create a time series object from data
ts_micro <- ts(monthly_combined_data$microthrix_percent, start = c(2021,1), frequency = 12)
ts_arcella <- ts(monthly_combined_data$arcella_percent, start = c(2021,1), frequency = 12)
ts_temp <- ts(monthly_combined_data$Temperature, start = c(2021,1), frequency = 12)
```

## 02 Lag2.Plot analysis

We use lag2.plot to find the 'optimal' time ag in ou analysis. The first named series is the one that gets lagged.
```
#---------------- LAG2.PLOT ANALYSIS --------------------#######

# to use astsa in the best way we need to detach dplyr (if loaded)
detach(package:dplyr) 

# open pdf device to save output directly
pdf("lag2plot_mic.pdf", width = 14, height = 14)

# lag2.plot with customizations
lag2.plot(ts_micro, ts_arcella, max.lag = 12, 
          corr = TRUE, 
          smooth = TRUE, 
          col = 5,        # Set color to 5 (blue)
          lwl = 2,        # Lowess line width
          lwc = "red",    # Color of lowess line
          bgl = "white",  # Background of legend
          ltcol = "black",# Legend text color
          box.col = "gray",# Box color
          cex = 1.1)      # Point size
dev.off()
```
