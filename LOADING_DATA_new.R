# LOADING PACKAGES #########

library(vegan)
library(tidyverse)
library(readxl)
library(dplyr)


#-------------- LOADING DATASETS 1, 4, 8 AND 10 --------------######

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


#---------------- DATASET 4----------#

file_path3 <- "dataset4.xlsx"
kamp18 <- read_excel(file_path3, sheet = 1) 
kamp19 <- read_excel(file_path3, sheet = 2)
kamp20 <- read_excel(file_path3, sheet = 3)
kamp21 <- read_excel(file_path3, sheet = 4)
kamp22 <- read_excel(file_path3, sheet = 5)
kamp23 <- read_excel(file_path3, sheet = 6)

dataset4 <- rbind( kamp18, kamp19, kamp20, kamp21, kamp22, kamp23)
rm(kamp18, kamp19, kamp20, kamp21, kamp22, kamp23)

# add wwtp identifier 
dataset4$dataset <- 'dataset4'

#convert the column year into characters
dataset4$year <- as.character(dataset4$year)

# renaming specific columns 
dataset4$filament_density <- dataset4$Gesamtfädigkeit
dataset4$Gesamtfädigkeit <- NULL
dataset4$rotaria_rotatoria <- dataset4$rotatoria_rotatoria
dataset4$rotatoria_rotatoria <- NULL
dataset4$Temperature <- dataset4$temperature
dataset4$temperature <- NULL
dataset4$date <- as.Date(dataset4$sample_date, format = "%Y-%m-%d")
dataset4$sample_date <- NULL
dataset4 <- subset(dataset4, !is.na(Temperature))


# create df for abundance data
abun_dat4 <- dataset4 %>%
  select(-c( date, abbreviation, year, season, pH, Temperature, 
             location, Nitrit, Nitrat, Ammonium, gesamt_N, N_ges, dataset))

# create df for metadata
meta_dat4 <- dataset4 %>%
  select(c(Categorisation,  pH, Temperature, Nitrit, Nitrat, Ammonium, gesamt_N, N_ges, season))

meta_dat4 <- as.data.frame(meta_dat4)
abun_dat4<- as.data.frame(abun_dat4)

# set row names
rownames(meta_dat4) <- meta_dat4$Categorisation

rownames(abun_dat4) <- abun_dat4$Categorisation
abun_dat4$Categorisation <- NULL
meta_dat4$Categorisation <- NULL 


#---------------- DATASET 8----------#

file_path8 <- "dataset8.xlsx"
data1 <- read_excel(file_path8, sheet = 1)
data2<- read_excel(file_path8, sheet = 2)
data3 <- read_excel(file_path8, sheet = 3)

dataset8 <- rbind(data1, data2, data3)
rm(data1, data2, data3)

# add wwtp identifier 
dataset8$dataset <- 'dataset8'

# renaming 
dataset8$date <- as.Date(dataset8$sampling_date, format = "%Y-%m-%d")
dataset8$sampling_date <- NULL

abun_dat8 <- dataset8 %>%
  select(-c(abbreviation, Temperature, season,date, ISV, dataset))

meta_dat8 <- dataset8 %>%
  select(c( Categorisation, Temperature, season, ISV))

# convert it into a dataframe
meta_dat8 <- as.data.frame(meta_dat8)
abun_dat8<- as.data.frame(abun_dat8)

# set row names
rownames(meta_dat8) <- meta_dat8$Categorisation
rownames(abun_dat8) <- abun_dat8$Categorisation
abun_dat8$Categorisation <- NULL
meta_dat8$Categorisation <- NULL 



#------------------ DATASET 10--------#

file_path10 <- "data_KA3_niers.xlsx"
data1 <- read_excel(file_path10, sheet = 1)
data2 <- read_excel(file_path10, sheet = 2)
data3 <- read_excel(file_path10, sheet = 3)

dataset10 <- rbind(data1, data2, data3)
rm(data1, data2, data3)

# add wwtp identifier 
dataset10$dataset <- 'dataset10'

# renaming
dataset10$date <- as.Date(dataset10$sampling_date, format = "%Y-%m-%d")
dataset10$sampling_date <- NULL

abun_dat10 <- dataset10 %>%
  select(-c(abbreviation, Temperature, season, date, ISV, dataset))

meta_dat10 <- dataset10 %>%
  select(c( Categorisation, Temperature, season, ISV))

# convert it into a dataframe
meta_dat10 <- as.data.frame(meta_dat10)
abun_dat10 <- as.data.frame(abun_dat10)

# set row names
rownames(meta_dat10) <- meta_dat10$Categorisation
rownames(abun_dat10) <- abun_dat10$Categorisation
abun_dat10$Categorisation <- NULL
meta_dat10$Categorisation <- NULL 


#================Transforming Data ===============================#######

# transforming microthrix, arcella, rotifer, filament density and type 0041-like categories into percentages for comparison 
# is dataset specific becasue of specific categories 


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


##------- DATASET 4----------######

# max Bacteria abundance 
dataset_4_max <- 7

dataset4$microthrix_percent <- (dataset4$microthrix_parvicella / dataset_4_max) * 100
dataset4$density_percent <- (dataset4$filament_density / dataset_4_max) * 100
dataset4$typ_0041_percent <- (dataset4$type_0041 / dataset_4_max) * 100

# max Other organism abundance
dataset_4_abun <- 7

dataset4$arcella_percent <- (dataset4$arcella / dataset_4_abun) * 100
dataset4$rotifers_percent <- (dataset4$rotaria_rotatoria / dataset_4_abun) * 100


##------- DATASET 8----------######

# max ISV filament density 
dataset_8_fil <- 7

dataset8$density_percent <- (dataset8$ISV_fädigkeit / dataset_8_fil) *100

# max Bacteria abundance 
dataset_8_max <- 10

dataset8$microthrix_percent <- (dataset8$microthrix_parvicella / dataset_8_max) * 100
dataset8$typ_0041_percent <- (dataset8$type_0041_0675 / dataset_8_max) * 100

# max Other organism abundance
dataset_8_abun <- 3

dataset8$arcella_percent <- (dataset8$arcella_sp / dataset_8_abun) * 100
dataset8$rotifers_percent <- (dataset8$rotifers / dataset_8_abun) * 100


##------- DATASET 10----------######

# max ISV filament density 
dataset_10_fil <- 7

dataset10$density_percent <- (dataset10$ISV_fädigkeit / dataset_10_fil) *100

# max Bacteria abundance 
dataset_10_max <- 10

dataset10$microthrix_percent <- (dataset10$microthrix_parvicella / dataset_10_max) * 100
dataset10$typ_0041_percent <- (dataset10$type_0041_0675 / dataset_10_max) * 100

# max Other organism abundance
dataset_10_abun <- 3

dataset10$arcella_percent <- (dataset10$arcella_sp / dataset_10_abun) * 100
dataset10$rotifers_percent <- (dataset10$rotifers / dataset_10_abun) * 100



#================ Combining Data ===============================#######

# creating one dataframe with all the samples 


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
    microthrix_percent = dataset4$microthrix_percent,
    arcella_percent = dataset4$arcella_percent,
    rotifers_percent = dataset4$rotifers_percent,
    density_percent =dataset4$density_percent,
    typ_0041_percent = dataset4$typ_0041_percent,
    amphileptid_percent = dataset4$amphileptid_percent,
    nematoda_percent = dataset4$nematoda_percent,
    Temperature = dataset4$Temperature,
    date = dataset4$date,
    dataset = dataset4$dataset,
    season = dataset4$season
  ),
  data.frame(
    microthrix_percent = dataset8$microthrix_percent,
    arcella_percent = dataset8$arcella_percent,
    rotifers_percent = dataset8$rotifers_percent,
    density_percent = dataset8$density_percent,
    typ_0041_percent = dataset8$typ_0041_percent,
    amphileptid_percent = dataset8$amphileptid_percent,
    nematoda_percent = dataset8$nematoda_percent,
    Temperature = dataset8$Temperature,
    date = dataset8$date,
    dataset = dataset8$dataset,
    season = dataset8$season
  ),
  data.frame(
    microthrix_percent = dataset10$microthrix_percent,
    arcella_percent = dataset10$arcella_percent,
    rotifers_percent = dataset10$rotifers_percent,
    density_percent = dataset10$density_percent,
    typ_0041_percent = dataset10$typ_0041_percent,
    amphileptid_percent = dataset10$amphileptid_percent,
    nematoda_percent = dataset10$nematoda_percent,
    Temperature = dataset10$Temperature,
    date = dataset10$date,
    dataset = dataset10$dataset,
    season = dataset10$season
  )
)


#================ Creating Replicates ===============================#######

# we want to create replicated data
# we combine at least 3 samples, take the mean and take it as monthly value


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

