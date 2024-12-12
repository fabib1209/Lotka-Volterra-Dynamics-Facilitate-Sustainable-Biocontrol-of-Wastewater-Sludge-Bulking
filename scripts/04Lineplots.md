# Creating Lineplots 

Lineplots were used to visually assess temporal fluctuations in the long-term Dataset 1 and the replicated dataset. The replicated dataset was obtained by the aggregation of Datasets 1-4 with at least 3 samples per month covering 2021 to 2023 (n = 249), which allowed for a comparison of monthly sampling intervals. Only selected organisms, Ca. M. parvicella, "type 0041-like filamentous bacteria", Arcella spp., rotifers, the "filament density," and temperature were included in the replicated dataset. In this context, "filament density" refers to the sum of all individual filaments observed in the sample (German: "Gesamtfädigkeit") (Pinther et al., 2022), while type 0041-like filamentous bacteria represents bacterial filaments of unknown taxonomy, colloquial called “type 0041” and similar non-distinguishable morphotypes (Deepnarain et al., 2020; Sam et al., 2022).

## 01 Lineplot for dataset1

We create a lineplot function for all samples of Dataset 1. We include filament density, rotifers, Arcella spp. and temperature. This function can be used for every dataset to create a lineplot. 

```
## LOADING DATA ########

source("LOADING_DATA.R")

#-------- DATASET 1---------##########

# create a function for plotting
create_plot <- function(data, dataset_name, plot_width = 8, plot_height = 6) {
  # Normalize temperature in the long format
  temp_min <- min(data[["Temperature"]], na.rm = TRUE)
  temp_max <- max(data[["Temperature"]], na.rm = TRUE)
  
  # Create a column for normalized temperature in the data
  data$Temperature_normalized <- scales::rescale(data[["Temperature"]], 
                                                 to = c(0, 100), from = c(temp_min, temp_max))
  
  # Melt the data frame to long format for ggplot2
  plot_data_long <- reshape2::melt(data, id.vars = "date", 
                                   variable.name = "Variable", value.name = "Value")
  
  # Ensure 'Value' is numeric
  plot_data_long$Value <- as.numeric(plot_data_long$Value)
  
  # Create the plot
  p <- ggplot() +
    geom_smooth(data = plot_data_long[plot_data_long$Variable == "density_percent", ], 
                aes(x = date, y = Value, color = "filament density"),
                method = "loess", linewidth = 1, se = FALSE, span = 0.1) +
    geom_smooth(data = plot_data_long[plot_data_long$Variable == "arcella_percent", ], 
                aes(x = date, y = Value, color = "Arcella"), 
                method = "loess", linewidth = 1, se = FALSE, span = 0.1) +
    geom_smooth(data = plot_data_long[plot_data_long$Variable == "Temperature_normalized", ], 
                aes(x = date, y = Value, color = "Temperature"), 
                method = "loess", linewidth = 1, se = FALSE, span = 0.1) +
    geom_smooth(data = plot_data_long[plot_data_long$Variable == "rotifers_percent", ], 
                aes(x = date, y = Value, color = "Rotifers"), 
                method = "loess", linewidth = 1, se = FALSE, span = 0.1, linetype = 2) +
    # Manually assign colors to each variable
    scale_color_manual(values = c("filament density" = "deeppink", 
                                  "Arcella" = "orange1", 
                                  "Temperature" = "#2166AC",
                                  "Rotifers" = "grey")) +
    scale_y_continuous(
      name = "Abundance (%)",
      limits = c(0, 100),
      sec.axis = sec_axis(~ scales::rescale(., to = c(temp_min, temp_max), from = c(0, 100)), 
                          name = "Temperature (°C)")
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", expand = c(0, 0)) +
    labs(
      title = "Time Series of filament density, Arcella, Rotifers and Temperature",
      x = "Sampling Date",
      fill = "Variable",
      color = "Variable"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_blank()
    )
  
  # Save the plot with the dataset name in the filename
  ggsave(filename = paste0("lineplot_", dataset_name, "_", ".pdf"), 
         plot = p, width = plot_width, height = plot_height, units = "in")
  
  # Print the plot for display
  print(p)
}

# Example usage with dataset and specific dimensions
create_plot(dataset1, dataset_name = "dataset1", plot_width = 8, plot_height = 6)
```

## 02 Lineplot for replicated dataset 



```
# Function to calculate monthly stats (mean and standard deviation)
calculate_monthly_stats <- function(data) {
  # Extract year and month from date
  data$YearMonth <- format(as.Date(data$date), "%Y-%m")
  
  # Summarize by year-month to get mean and sd
  monthly_stats <- data %>%
    group_by(YearMonth) %>%
    summarise(
      sampling_date = as.Date(paste0(YearMonth, "-01")),
      microthrix_mean = mean(microthrix_percent, na.rm = TRUE),
      microthrix_sd = sd(microthrix_percent, na.rm = TRUE),
      arcella_mean = mean(arcella_percent, na.rm = TRUE),
      arcella_sd = sd(arcella_percent, na.rm = TRUE),
      rotifers_mean = mean(rotifers_percent, na.rm = TRUE),
      rotifers_sd = sd(rotifers_percent, na.rm = TRUE),
      density_mean = mean(density_percent, na.rm = TRUE),
      density_sd = sd(density_percent, na.rm = TRUE),
      temperature_mean = mean(Temperature_normalized, na.rm = TRUE),
      temperature_sd = sd(Temperature_normalized, na.rm = TRUE)
    )
  
  return(monthly_stats)
}

# Apply the function to combined_filtered_data
monthly_stats <- calculate_monthly_stats(combined_filtered_data)

# because sd and mean alway are at first of month, they were shifted to the 15th
mid_month <- function(date) {
  as.Date(format(date, "%Y-%m-15"))
}

# Apply the mid_month function to the sampling_date column in monthly_stats
monthly_stats$sampling_date <- mid_month(monthly_stats$sampling_date)

# we need to normalize the temperature for better visualisation with the other axes
temp_min <- min(combined_filtered_data[["Temperature"]], na.rm = TRUE)
temp_max <- max(combined_filtered_data[["Temperature"]], na.rm = TRUE)
# create column for normalized temp in combined filtered data 
combined_filtered_data$Temperature_normalized <- scales::rescale(combined_filtered_data[["Temperature"]], 
                                                                 to = c(0, 100), from = c(temp_min, temp_max))

#---------------- PLOTTING --------------------#######

# lineplot with filament density, rotifers, rotifers and temperature (same script for Ca. M. parvicella, type-0041-like)
# with standard deviation ribbons and loess-smoothed lines
p <- ggplot() +
  geom_ribbon(data = monthly_stats, aes(x = sampling_date, 
                                        ymin = density_mean - density_sd, 
                                        ymax = density_mean + density_sd, 
                                        fill = "density"), alpha = 0.2) +
  geom_ribbon(data = monthly_stats, aes(x = sampling_date, 
                                        ymin = arcella_mean - arcella_sd, 
                                        ymax = arcella_mean + arcella_sd, 
                                        fill = "arcella"), alpha = 0.2) +
  geom_ribbon(data = monthly_stats, aes(x = sampling_date, 
                                        ymin = temperature_mean - temperature_sd, 
                                        ymax = temperature_mean + temperature_sd, 
                                        fill = "Temperature"), alpha = 0.2) +
  geom_ribbon(data = monthly_stats, aes(x = sampling_date, 
                                        ymin = rotifers_mean - rotifers_sd, 
                                        ymax = rotifers_mean + rotifers_sd, 
                                        fill = "rotifers"), alpha = 0.2) +
  
  # Smoothed line plots for each variable
  geom_smooth(data = combined_filtered_data, 
              aes(x = date, y = density_percent, color = "density"),
              method = "loess", linewidth = 1, se = FALSE, span = 0.09) +
  geom_smooth(data = combined_filtered_data, 
              aes(x = date, y = arcella_percent, color = "arcella"), 
              method = "loess", linewidth = 1, se = FALSE, span = 0.09) +
  geom_smooth(data = combined_filtered_data, 
              aes(x = date, y = Temperature_normalized, color = "Temperature"), 
              method = "loess", linewidth = 1, se = FALSE, span = 0.09) +
  geom_smooth(data = combined_filtered_data, 
              aes(x = date, y = rotifers_percent, color = "rotifers"), 
              method = "loess", linewidth = 1, se = FALSE, span = 0.09, linetype = 2) +
  
  # Color and fill settings
  scale_color_manual(values = c("density" = "deeppink", "arcella" = "orange1", 
                                "Temperature" = "#2166AC", "rotifers" = "grey")) +
  scale_fill_manual(values = c("density" = "deeppink", "arcella" = "orange1", 
                               "Temperature" = "#2166AC", "rotifers" = "grey")) +
  
  # Y-axis with secondary temperature axis
  scale_y_continuous(
    name = "Abundance (%)",
    limits = c(-10, 120),
    sec.axis = sec_axis(~ scales::rescale(., to = c(temp_min, temp_max), from = c(0, 100)), 
                        name = "Temperature (°C)")
  ) +
  
  # X-axis date formatting
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    expand = c(0, 0)
  ) +
  
  # Titles and labels
  labs(
    title = "Time Series of density, Rotifers, Arcella, and Temperature",
    x = "Sampling Date",
    fill = "Variable",
    color = "Variable"
  ) +
  
  # Theme adjustments
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Print the plot
print(p)

# safe the plot
ggsave(filename = paste0("lineplot_combined_filtered_sd_mean.pdf"), 
       plot = p, width = 8, height = 6)
```
