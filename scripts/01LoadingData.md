# Loading Datasets into R

Here we show how the raw datasets were loaded into R and how they were preprocessed

## 01 Loading in Datasets 1,2,3,4

```
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
```
