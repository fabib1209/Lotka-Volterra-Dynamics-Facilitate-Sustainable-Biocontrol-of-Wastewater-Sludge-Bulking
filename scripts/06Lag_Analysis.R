#================ CROSS-CORRELATION-ANALYSIS ==============
# we perform corss correlation analysis to find the 'optimal' time lag between the periods of the abundances
# we use the astsa package

library(astsa)


## LOADING DATA ########

source("LOADING_DATA_new.R")


## create monthly sampling for combined_filtered_data ########

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


#---------------- LAG2.PLOT ANALYSIS --------------------#######

# to use astsa in the best way we need to detach dplyr (if loaded)
detach(package:dplyr) 


# the first named series is the one that gets lagged 

pdf("lag2plot_mic.pdf", width = 14, height = 14)

# Example lag2.plot with customizations
lag2.plot(ts_micro, arcella, max.lag = 12, 
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



