# Structural equation modeling

Structural equation modeling (SEM) was used to test and refine hypothesized models of ecosystem processes and interactions (Arhonditsis et al., 2006). Models were calculated using the lavaan package v. 0.6.18 (Rosseel, 2012), and visualized using tidySEM v. 0.2.7 (Lissa, 2024).

Here we show how SEM was performed on our dataset. We specified two models, the first one with filament density, Arcella spp. and rotifers and temperature, and the second one with Ca. M. parvicella, Arcella spp. type 0041-like filamentous bacteria and temperature.

## 01 Function

We define a function to fit the SEM, summarize the results and plot them. Here we innclude both SEM model approaches.

```

# LOADING PACKAGES #########
library(lavaan)
library(tidySEM)
library(ggplot2)


## LOADING DATA ########

source("LOADING_DATA.R")


#---------------- FUNCTION --------------------#######

fit_and_plot_sem <- function(data, output_file = "sem_plot.pdf") {
  
  # Specify the SEM model
  sem_model <- '
    #
    # density + arcella + roti + temp (graph2)
    #density_percent ~ Temperature + arcella_percent + rotifers_percent
    #arcella_percent ~ Temperature 
    #rotifers_percent ~ Temperature 
    # 
    # micro + 0041 + arc + temp (graph3)
    microthrix_percent ~ arcella_percent + Temperature 
    typ_0041_percent ~ arcella_percent + microthrix_percent + Temperature
    density_percent ~ arcella_percent + microthrix_percent + typ_0041_percent + Temperature
    arcella_percent ~ Temperature
    #
    
  '
  
  # Fit the SEM model
  fit <- sem(sem_model, data = data)
  
  # Summarize the results
  cat("### Model Summary ###\n")
  print(summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
  
  # Simple SEM plot without layout
  cat("\n### Simple Plot ###\n")
  graph_sem(model = fit)
  
  # Create a custom layout with microthrix
  #sem_layout <- get_layout(
    #"Temperature", "", "density_percent",
    #"rotifers_percent", "","",
    #"","arcella_percent","",
    #rows = 3
  #)
  # Create a custom layout for grafik 3
  sem_layout <- get_layout(
    "Temperature", "", "microthrix_percent",
    "", "","density_percent",
    "arcella_percent","","",
    "", "typ_0041_percent","",
    rows = 4
  )

  
  # Generate the SEM plot with custom layout
  sem_plot <- graph_sem(model = fit, layout = sem_layout, text_size = 2)
  
  # Save the plot as a PDF
  ggsave(output_file, plot = sem_plot, device = "pdf", width = 8, height = 6)
  
  cat("\n### Plot saved as:", output_file, "###\n")
}

# USAGE ####
fit_and_plot_sem(data = data, output_file = "SEM_.pdf")
```
