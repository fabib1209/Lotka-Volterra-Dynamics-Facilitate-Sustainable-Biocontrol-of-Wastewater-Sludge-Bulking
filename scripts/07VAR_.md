# Vector Autoregression

Vector autoregression (VAR) models were constructed from the time series objects to quantify the reciprocal impact of taxa. The R packages tseries v. 0.10.57 (Trapletti and Hornik, 2024), vars v. 1.6.1 (Bernhard Pfaff, 2008), forecast v. 8.23.0 (Hyndman et al., 2009), urca v. 1.3.4 (B. Pfaff, 2008), and mFilter v. 0.1.5 (Balcilar, 2019) were used to perform VAR modeling and data visualization. 


Here we show how we calculated the VAR model as well as Granger-causality, impulse response function and forecast error variance decomposition.

## 01 Creating TS objects 

Again we need time-series objects for this.

```
## LOADING PACKAGES #########
library(tseries)
library(vars)
library(forecast)
library(urca)
library(mFilter)

## LOADING DATA ########

source("LOADING_DATA.R")

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
  
```
## 02 Model calculation

First we need to check some statistics things if the data is ok for modeling.

```
## CREATE VAR MODEL ############


# determine the persistence of the model 
acf(ts_micro, main = "ACF for microthrix")
pacf(ts_micro, main = "PACF for microthrix")

acf(ts_arcella, main = "ACF for arcella")
pacf(ts_arcella, main = "PACF for arcella")


# Check stationarity for each series
adf.test(ts_arcella)
adf.test(ts_micro)
adf.test(ts_temp)
```
If the p is under 0.05 its stationary,if not stationary, you may need to difference your data.

We need to find the optimal lag for the model. We create a dataframe from our ts and use VARselect to get indicators for the optimal lag.

```
micro_arc <- cbind(ts_arcella, ts_micro)
colnames(micro_arc) <- cbind( "arcella", "microthrix")

# we get indicators that indicate the best lag
lagselect <- VARselect(micro_arc, lag.max = 20, type = "const")


lagselect$selection
# 3 out of 4 say 8 so we use 8 as lag

# Fit the VAR model
var_model <- VAR(micro_arc, p = 8) 
# You can choose the lag order (p) based on criteria from lagselect

summary(var_model)
```
# 03 Diagnostics

Here we test if the model is fitted right.


```
# serial correlation with Portmanteau Test
serial1 <- serial.test(var_model, lags.pt = 12, type = "PT.asymptotic")
serial1

# heteroscedasticity
arch1 <- arch.test(var_model, lags.multi = 12, multivariate.only = TRUE)
arch1
# here we get 1 so the model does not have heteroscedasticity

# Check for normality of residuals
norm1 <- normality.test(var_model, multivariate.only = TRUE)
norm1
# the model passed the normality test because nothing is significant

# Check for stability - testing for structural breaks in residuals
stability1 <- stability(var_model, type = "OLS-CUSUM")
plot(stability1)
# nothing exceeds the red lines in the graphs so the system is stable
```
# 04 Application

Granger causality tests were conducted, using the causality function (package vars) to evaluate whether past values of Ca. M. parvicella could predict Arcella spp. abundance, and vice versa. Instantaneous causality was also assessed with this function.
To evaluate species-specific responses to unexpected changes in the other species’ abundance, impulse response function (IRF) analysis was performed using the irf function (package vars) (Fig. 4C)(Ewing et al., 2007). Additionally, forecast error variance decomposition (FEVD) was employed with the fevd function (package vars), to quantify how much of the variance in prediction errors for one organism could be attributed to changes in the other species’ abundance (Fig. 4B)(Lütkepohl, 2007).

Here we show applications of Granger-causality, IRF and FEVD.

```
## Granger causality #########
# we can look at the causality in both directions - unidirectional/bidirectional or no causality 
granger_test <- causality(var_model, cause = "microthrix")
granger_test
# we get two test results


granger_test2 <- causality(var_model, cause = "arcella")
granger_test2

```
For the first we get: 
granger: we can reject H0 so microthrix do granger cause arcella
instant: we cannot reject H0 so changes in microthrix do not appear to be associated with changes in arcella at the same time.

For the second one:
granger: we cannot reject the H0 so arcella does not granger cause microthrix
instant: we cannot reject H0 so No instantaneous causality between: arcella and microthrix


```
## Impulse response functions ###############
# can be used to see what would happen if one variable would "shock" the other (unexpected increase)

irf_result <- irf(var_model, impulse = "arcella", response = "microthrix", 
                  boot = TRUE, runs = 1000, n.ahead = 12)

plot(irf_result, ylab = "microthrix", main = "shock from arcella")
# from the plot we can see that an increase in arella decreases microthrix instantly


## Forecast Error Variance decomposition ############

fevd1 <- fevd(var_model, n.ahead = 12)
fevd1$microthrix[1,]

pdf("fevd_mic_arc.pdf", width = 14, height = 14)
plot(fevd1)
dev.off()
```
