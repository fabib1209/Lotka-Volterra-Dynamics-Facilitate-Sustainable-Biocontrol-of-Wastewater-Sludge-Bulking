# Loading Datasets into R

Here we show how the raw datasets were loaded into R and how they were preprocessed

## 01 Loading in Datasets 1,2,3,4

Individual datasets get loaded into respective dataframes in R. Column names get changed into English and abundance and metadata ssubsets get created.

```
# LOADING PACKAGES #########

library(vegan)
library(tidyverse)
library(readxl)
library(dplyr)

#------- DATASET 1----------#
file_path1 <- "dataset1.xlsx"
data1 <- read_excel(file_path1, sheet = 1)
data2 <- read_excel(file_path1, sheet = 2)
data3 <- read_excel(file_path1, sheet = 3)
data4 <- read_excel(file_path1, sheet = 4)
data5 <- read_excel(file_path1, sheet = 5)
data6 <- read_excel(file_path1, sheet = 6)
data7 <- read_excel(file_path1, sheet = 7)
data8 <- read_excel(file_path1, sheet = 8)
data9 <- read_excel(file_path1, sheet = 9)

# combine each df into one big one 
dataset1 <- rbind(data1, data2, data3, data4, data5, data6, data7,
                  data8, data9)

# remove the individual dfs
rm(data1, data2, data3, data4, data5, data6, data7, data8, data9)

# convert the column year into characters
dataset1$year <- as.character(dataset1$year)
dataset1$index <- NULL

# add wwtp identifier 
dataset1$dataset <- 'dataset1'

# renaming specific columns 
dataset1$Temperature <- dataset1$temperature
dataset1$temperature <- NULL
dataset1$rotifers <- dataset1$rotatoria
dataset1$rotatoria <- NULL
dataset1$pH <- dataset1$pH_Wert
dataset1$pH_Wert <- NULL
dataset1$filterable_substances <- dataset1$Abfiltrierbare_Stoffe
dataset1$Abfiltrierbare_Stoffe <- NULL
dataset1$resting_cells <- dataset1$dauerzellen
dataset1$dauerzellen <- NULL
dataset1$COD <- dataset1$CSB
dataset1$CSB <- NULL
dataset1$filament_density <- dataset1$threadiness
dataset1$threadiness <- NULL
dataset1$date <- as.Date(dataset1$sampling_date, format = "%Y-%m-%d")
dataset1$sampling_date <- NULL

# create df for abundance data
abun_dat1 <- dataset1 %>%
  select(-c(date, abbreviation, year, season, pH ,NH4_N,
            TNb, COD, Pges, filterable_substances, Temperature, dataset, Categorisation,
            index, other...9, other...12, other...26, other...31))

# create df for metadata
meta_dat1 <- dataset1 %>%
  select(c(Categorisation, pH ,NH4_N,
           TNb, COD, Pges, filterable_substances, Temperature, season))

meta_dat1 <- as.data.frame(meta_dat1)
abun_dat1<- as.data.frame(abun_dat1)

# set row names and delete empty columns
rownames(meta_dat1) <- meta_dat1$Categorisation
rownames(abun_dat1) <- abun_dat1$Categorisation
abun_dat1$Categorisation <- NULL
meta_dat1$Categorisation <- NULL


#---------------- DATASET 2----------#

file_path3 <- "dataset2.xlsx"
data1<- read_excel(file_path3, sheet = 1) 
data2 <- read_excel(file_path3, sheet = 2)
data3 <- read_excel(file_path3, sheet = 3)
data4 <- read_excel(file_path3, sheet = 4)
data5 <- read_excel(file_path3, sheet = 5)
data6 <- read_excel(file_path3, sheet = 6)

dataset2 <- rbind( data1, data2, data3, data4, data5, data6)
rm(data1, data2, data3, data4, data5, data6)

# add wwtp identifier 
dataset2$dataset <- 'dataset2'

#convert the column year into characters
dataset2$year <- as.character(dataset2$year)

# renaming specific columns 
dataset2$filament_density <- dataset2$Gesamtfädigkeit
dataset2$Gesamtfädigkeit <- NULL
dataset2$rotaria_rotatoria <- dataset2$rotatoria_rotatoria
dataset2$rotatoria_rotatoria <- NULL
dataset2$Temperature <- dataset2$temperature
dataset2$temperature <- NULL
dataset2$date <- as.Date(dataset2$sample_date, format = "%Y-%m-%d")
dataset2$sample_date <- NULL
dataset2 <- subset(dataset2, !is.na(Temperature))


# create df for abundance data
abun_dat2 <- dataset2 %>%
  select(-c( date, abbreviation, year, season, pH, Temperature, 
             location, Nitrit, Nitrat, Ammonium, gesamt_N, N_ges, dataset))

# create df for metadata
meta_dat2 <- dataset2 %>%
  select(c(Categorisation,  pH, Temperature, Nitrit, Nitrat, Ammonium, gesamt_N, N_ges, season))

meta_dat2 <- as.data.frame(meta_dat2)
abun_dat2<- as.data.frame(abun_dat2)

# set row names
rownames(meta_dat2) <- meta_dat2$Categorisation

rownames(abun_dat2) <- abun_dat2$Categorisation
abun_dat2$Categorisation <- NULL
meta_dat2$Categorisation <- NULL 


#---------------- DATASET 3----------#

file_path8 <- "dataset3.xlsx"
data1 <- read_excel(file_path8, sheet = 1)
data2<- read_excel(file_path8, sheet = 2)
data3 <- read_excel(file_path8, sheet = 3)

dataset3 <- rbind(data1, data2, data3)
rm(data1, data2, data3)

# add wwtp identifier 
dataset3$dataset <- 'dataset3'

# renaming 
dataset3$date <- as.Date(dataset3$sampling_date, format = "%Y-%m-%d")
dataset3$sampling_date <- NULL

abun_dat3 <- dataset3 %>%
  select(-c(abbreviation, Temperature, season,date, ISV, dataset))

meta_dat3 <- dataset3 %>%
  select(c( Categorisation, Temperature, season, ISV))

# convert it into a dataframe
meta_dat3 <- as.data.frame(meta_dat3)
abun_dat3<- as.data.frame(abun_dat3)

# set row names
rownames(meta_dat3) <- meta_dat3$Categorisation
rownames(abun_dat3) <- abun_dat3$Categorisation
abun_dat3$Categorisation <- NULL
meta_dat3$Categorisation <- NULL 



#------------------ DATASET 4--------#

file_path10 <- "dataset4.xlsx"
data1 <- read_excel(file_path10, sheet = 1)
data2 <- read_excel(file_path10, sheet = 2)
data3 <- read_excel(file_path10, sheet = 3)

dataset4 <- rbind(data1, data2, data3)
rm(data1, data2, data3)

# add wwtp identifier 
dataset4$dataset <- 'dataset4'

# renaming
dataset4$date <- as.Date(dataset4$sampling_date, format = "%Y-%m-%d")
dataset4$sampling_date <- NULL

abun_dat4 <- dataset4 %>%
  select(-c(abbreviation, Temperature, season, date, ISV, dataset))

meta_dat4 <- dataset4 %>%
  select(c( Categorisation, Temperature, season, ISV))

# convert it into a dataframe
meta_dat4 <- as.data.frame(meta_dat4)
abun_dat4 <- as.data.frame(abun_dat4)

# set row names
rownames(meta_dat4) <- meta_dat4$Categorisation
rownames(abun_dat4) <- abun_dat4$Categorisation
abun_dat4$Categorisation <- NULL
meta_dat4$Categorisation <- NULL
```
## 02 Transforming the data 

Because of the categorical nature of the data, the abundances of Ca. Microthrix parvicella, Arcella spp., rotifers, filament density and type 0041-like bacteria got converted into percent to be able to compare them. 

```
##------- DATASET 1----------######

# max Bacteria abundance 
dataset_1_max <- 4
# transforming the abundance into percent 
dataset1$microthrix_percent <- (dataset1$microthrix_parvicella / dataset_1_max) * 100
dataset1$density_percent <- (dataset1$filament_density / dataset_1_max) * 100
dataset1$typ0041_percent <- (dataset1$type0041_1851/ dataset_1_max) * 100

# max Other organism abundance
dataset_1_abun <- 3
# transforming the abundance into percent 
dataset1$arcella_percent <- (dataset1$shell_amoebas / dataset_1_abun) * 100
dataset1$rotifers_percent <- (dataset1$rotifers/ dataset_1_abun) * 100


##------- DATASET 2----------######

# max Bacteria abundance 
dataset_2_max <- 7

dataset2$microthrix_percent <- (dataset2$microthrix_parvicella / dataset_2_max) * 100
dataset2$density_percent <- (dataset2$filament_density / dataset_2_max) * 100
dataset2$typ_0041_percent <- (dataset2$type_0041 / dataset_2_max) * 100

# max Other organism abundance
dataset_2_abun <- 7

dataset2$arcella_percent <- (dataset2$arcella / dataset_2_abun) * 100
dataset2$rotifers_percent <- (dataset2$rotaria_rotatoria / dataset_2_abun) * 100


##------- DATASET 3----------######

# max ISV filament density 
dataset_3_fil <- 7

dataset3$density_percent <- (dataset3$ISV_fädigkeit / dataset_3_fil) *100

# max Bacteria abundance 
dataset_3_max <- 10

dataset3$microthrix_percent <- (dataset3$microthrix_parvicella / dataset_3_max) * 100
dataset3$typ_0041_percent <- (dataset3$type_0041_0675 / dataset_3_max) * 100

# max Other organism abundance
dataset_3_abun <- 3

dataset3$arcella_percent <- (dataset3$arcella_sp / dataset_3_abun) * 100
dataset3$rotifers_percent <- (dataset3$rotifers / dataset_3_abun) * 100


##------- DATASET 4----------######

# max ISV filament density 
dataset_4_fil <- 7

dataset4$density_percent <- (dataset4$ISV_fädigkeit / dataset_4_fil) *100

# max Bacteria abundance 
dataset_4_max <- 10

dataset4$microthrix_percent <- (dataset4$microthrix_parvicella / dataset_4_max) * 100
dataset4$typ_0041_percent <- (dataset4$type_0041_0675 / dataset_4_max) * 100

# max Other organism abundance
dataset_4_abun <- 3

dataset4$arcella_percent <- (dataset4$arcella_sp / dataset_4_abun) * 100
dataset4$rotifers_percent <- (dataset4$rotifers / dataset_4_abun) * 100
```

## 03 Combining all Datasets

Creating one dataframe with all available samples from all datasets

```
combined_data <- rbind(
  data.frame(
    microthrix_percent = dataset1$microthrix_percent,
    arcella_percent = dataset1$arcella_percent,
    rotifers_percent = dataset1$rotifers_percent,
    density_percent = dataset1$density_percent,
    typ_0041_percent = dataset1$typ0041_percent,
    amphileptid_percent = dataset1$amphileptid_percent,
    nematoda_percent = dataset1$nematoda_percent,
    Temperature = dataset1$Temperature,
    date = dataset1$date,
    dataset = dataset1$dataset,
    season = dataset1$season
  ),
  data.frame(
    microthrix_percent = dataset2$microthrix_percent,
    arcella_percent = dataset2$arcella_percent,
    rotifers_percent = dataset2$rotifers_percent,
    density_percent =dataset2$density_percent,
    typ_0041_percent = dataset2$typ_0041_percent,
    amphileptid_percent = dataset2$amphileptid_percent,
    nematoda_percent = dataset2$nematoda_percent,
    Temperature = dataset2$Temperature,
    date = dataset2$date,
    dataset = dataset2$dataset,
    season = dataset2$season
  ),
  data.frame(
    microthrix_percent = dataset3$microthrix_percent,
    arcella_percent = dataset3$arcella_percent,
    rotifers_percent = dataset3$rotifers_percent,
    density_percent = dataset3$density_percent,
    typ_0041_percent = dataset3$typ_0041_percent,
    amphileptid_percent = dataset3$amphileptid_percent,
    nematoda_percent = dataset3$nematoda_percent,
    Temperature = dataset3$Temperature,
    date = dataset3$date,
    dataset = dataset3$dataset,
    season = dataset3$season
  ),
  data.frame(
    microthrix_percent = dataset4$microthrix_percent,
    arcella_percent = dataset4$arcella_percent,
    rotifers_percent = dataset4$rotifers_percent,
    density_percent = dataset4$density_percent,
    typ_0041_percent = dataset4$typ_0041_percent,
    amphileptid_percent = dataset4$amphileptid_percent,
    nematoda_percent = dataset4$nematoda_percent,
    Temperature = dataset4$Temperature,
    date = dataset4$date,
    dataset = dataset4$dataset,
    season = dataset4$season
  )
)
```

## 04 Creating replicated dataset

To create replicates out of the big dataframe. We combined at least 3 samples, take the mean and take that as monthly value. Also, outliers were removed.

```
# Convert Sampling_date to Date format 
combined_data$date <- as.Date(combined_data$date)

# Extract month and year from date
combined_data$Month <- format(combined_data$date, "%Y-%m")

# List to store the monthly data 
monthly_data_list <- list()

# Get unique months
unique_months <- unique(combined_data$Month)

# Define the column names
column_names <- c("microthrix_percent", "arcella_percent", "rotifers_percent", "density_percent", 
                  "typ_0041_percent", "Temperature", "date", "dataset", "season")
                  

# Filter data by each month and store in the list
# this list has every month of sampling as an entry and in that entry the different data points 
for (month in unique_months) {
  monthly_data <- combined_data[combined_data$Month == month, ]
  # Select the required columns
  monthly_data <- monthly_data[, column_names]
  monthly_data_list[[month]] <- monthly_data
}

# create new list for filtered months 
filtered_monthly_data_list <- list()

for (month in names(monthly_data_list)) {
  monthly_data <- monthly_data_list[[month]]
  # only take months with at least 3 samples 
  if (nrow(monthly_data) >= 3) {
    filtered_monthly_data_list[[month]] <- monthly_data
  }
}

# Combine the filtered monthly data into a single data frame
combined_filtered_data <- do.call(rbind, filtered_monthly_data_list)

# Define rows to remove as 2018 is not represented enough
rows_to_remove <- c(1,2,3,4,5,6) # in the first 6 rows are dates from 2018 

# Remove the specified rows
combined_filtered_data <- combined_filtered_data[-rows_to_remove, ]

# remove outlier temperature 
# Define the sampling dates to remove
dates_to_remove <- as.Date(c("2023-09-25", "2023-01-09", "2023-01-20", "2023-01-30"))

# Remove the specified rows
combined_filtered_data <- combined_filtered_data[!(combined_filtered_data$date %in% dates_to_remove), ]
```

