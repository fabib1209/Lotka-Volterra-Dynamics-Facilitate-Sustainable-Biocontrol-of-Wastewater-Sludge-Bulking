# Calculating correlation coefficients and heatmap creation

We show how Spearmans Rank correlation was used to create heatmaps for every dataset on te example of dataset1.

## 01 Spearmans Rank correlation (Organisms vs. Metadata)

We create organism groups based on taxonomy. These are different for every dataset because of differently describes species, this is exemplary for dataset1. Then we calculate the correlation coefficients first between all organisms and the metadata described for the dataset.

```
# Define organism groups from excel data names 
group_a <- c( 'microthrix_parvicella', 'spirillen_spirochaetes', 'type0041_1851',
              'type_021N', 'filament_density', 'nocardioforme_actinomyceten')

group_b <- c('shell_amoebas', 'naked_amoebas')

group_c <- c('carchesium', 'epistylis', 'opercularia', 'vorticella_microstoma', 
             'vorticellae_sp.', 'suctoria', 'heads_tails', 'swarm_cells', 'amphileptiden', 
             'aspidisca_sp.', 'chilodonella_sp.', 'resting_cells', 'euplotes_sp.')

group_d <- c('bodo_sp.', 'eye_flagellates')

group_f <- c('nematoda', 'rotifers')

group_1 <- c(group_a, group_b, group_c, group_d, group_f)
group_2 <- c('pH', 'NH4_N', 'TNb', 'COD', 'Pges', 'filterable_substances', 'Temperature')

# Initialize a matrix to store correlation coefficients
correlation_matrix <- matrix(NA, nrow = length(group_1), ncol = length(group_2),
                             dimnames = list(group_1, group_2))

# Calculate correlations 
for (g1 in group_1) {
  for (g2 in group_2) {
    test <- cor.test(abun_dat1[[g1]], meta_dat1[[g2]], method = "spearman")
    correlation_matrix[g1, g2] <- test$estimate
  }
}
```

## 02 Ordering 

We order the defined organisms for better visibility in plotting later. 

```
# Convert the correlation matrix into a data frame for easier manipulation
correlation_df <- as.data.frame(correlation_matrix)

# Add columns for ordering (Module, Subclass, Name)
correlation_df$Name <- rownames(correlation_df)
correlation_df$Module <- ifelse(correlation_df$Name %in% group_a, "Group A",
                                ifelse(correlation_df$Name %in% group_b, "Group B", 
                                       ifelse(correlation_df$Name %in% group_c, "Group C",
                                              ifelse(correlation_df$Name %in% group_d, "Group D", "Group F"))))

correlation_df$Subclass <- ifelse(correlation_df$Name %in% group_a, "Subclass 1",
                                  ifelse(correlation_df$Name %in% group_b, "Subclass 2", 
                                         ifelse(correlation_df$Name %in% group_c, "Subclass 3",
                                                ifelse(correlation_df$Name %in% group_d, "Subclass 4", "Subclass 5"))))

# Step 1: Order by row means 
correlation_df <- correlation_df[order(rowMeans(correlation_df[, sapply(correlation_df, is.numeric)], na.rm = TRUE), decreasing = FALSE), ]


# Step 2: Order by Module
correlation_df <- correlation_df[order(correlation_df$Module), ]

# Step 3: Order by Subclass
correlation_df <- correlation_df[order(correlation_df$Subclass), ]

# After sorting, convert the Name, Module, and Subclass to factors with ordered levels
correlation_df$Name <- factor(correlation_df$Name, levels = unique(correlation_df$Name))
correlation_df$Module <- factor(correlation_df$Module, levels = unique(correlation_df$Module))
correlation_df$Subclass <- factor(correlation_df$Subclass, levels = unique(correlation_df$Subclass))

# Melt the data frame for ggplot2
melted_data <- melt(correlation_df, id.vars = c("Name", "Module", "Subclass"))

# Replace 0s with NA 
melted_data$value[melted_data$value == 0] <- NA
```

## 03 Plotting 

We plot the ordered correlation coefficients ina heatmap style.

```
g <- ggplot(melted_data, aes(x = variable, y = Name, fill = value)) + 
  geom_tile(color = "black", lwd = 0.5, linetype = 1) +
  labs(x = "Metadata", y = "Organisms", title = "Heatmap by Module and Subclass") +
  scale_fill_gradientn(colours = c("darkred", "white", "skyblue"), na.value = "azure1",
                       values = scales::rescale(c(-0.7, -0.5, 0, 0.5, 0.7))) +
  scale_color_manual(values = c("black", "black"), guide = "none") +
  facet_grid(Subclass + Module ~ ., space = "free", scales = "free", switch = "both") +
  theme_classic() +
  theme(strip.text.y.right = element_text(angle = 0, size = 11),
        strip.background.y = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_text(angle = 45, face = "italic", hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(0.5, "lines")) 

# Print the plot
print(g)

# save the plot
ggsave("heatmap_dat1.pdf", plot = g, width = 10, height = 15)
```

## 04 Spearmans Rank correlation (Prokaryotes vs. Eukaryotes) + Ordering + Plotting

We create organism groups based on prokaryotes or eukaryotes. These are different for every dataset because of differently describes species, this is exemplary for dataset1. Then we calculate the correlation coefficients.

```
# Define organism groups
group_b <- c('shell_amoebas', 'naked_amoebas')

group_c <- c('carchesium', 'epistylis', 'opercularia', 'vorticella_microstoma', 
             'vorticellae_sp.', 'suctoria', 'heads_tails', 'swarm_cells', 'amphileptiden', 
             'aspidisca_sp.', 'chilodonella_sp.', 'resting_cells', 
             'euplotes_sp.')

group_d <- c('bodo_sp.', 'eye_flagellates')

group_f <- c('nematoda', 'rotifers')

# Combine all into Group 1
group_1 <- c(group_b, group_c, group_d, group_f)

# Group 2: Bacteria
group_2 <- c('filament_density', 'microthrix_parvicella', 'spirillen_spirochaetes', 'type0041_1851',
             'nocardioforme_actinomyceten', 'type_021N')

# Initialize a matrix to store correlation coefficients 
correlation_matrix <- matrix(NA, nrow = length(group_1), ncol = length(group_2),
                             dimnames = list(group_1, group_2))

# Calculate correlations 
for (g1 in group_1) {
  for (g2 in group_2) {
    test <- cor.test(abun_dat1[[g1]], abun_dat1[[g2]], method = "spearman")
    correlation_matrix[g1, g2] <- test$estimate
  }
}

# Convert the correlation matrix into a data frame for easier manipulation
correlation_df <- as.data.frame(correlation_matrix)

# Add columns for ordering (Module, Subclass, Name)
correlation_df$Name <- rownames(correlation_df)
correlation_df$Module <- ifelse(correlation_df$Name %in% group_b, "Group B",
                                ifelse(correlation_df$Name %in% group_c, "Group C", 
                                       ifelse(correlation_df$Name %in% group_d, "Group D", "Group F")))

correlation_df$Subclass <- ifelse(correlation_df$Name %in% group_b, "Subclass 1",
                                  ifelse(correlation_df$Name %in% group_c, "Subclass 2", 
                                         ifelse(correlation_df$Name %in% group_d, "Subclass 3", "Subclass 4")))

# Step 1: Order by row means 
correlation_df <- correlation_df[order(rowMeans(correlation_df[, sapply(correlation_df, is.numeric)], na.rm = TRUE), decreasing = FALSE), ]

# Step 2: Order by Module
correlation_df <- correlation_df[order(correlation_df$Module), ]

# Step 3: Order by Subclass
correlation_df <- correlation_df[order(correlation_df$Subclass), ]

# After sorting, convert the Name, Module, and Subclass to factors with ordered levels
correlation_df$Name <- factor(correlation_df$Name, levels = unique(correlation_df$Name))
correlation_df$Module <- factor(correlation_df$Module, levels = unique(correlation_df$Module))
correlation_df$Subclass <- factor(correlation_df$Subclass, levels = unique(correlation_df$Subclass))

# Melt the data frame for ggplot2
melted_data <- melt(correlation_df, id.vars = c("Name", "Module", "Subclass"))

# Replace 0s with NA (if required by your data)
melted_data$value[melted_data$value == 0] <- NA


#---------------- PLOTTING --------------------#######

g <- ggplot(melted_data, aes(x = variable, y = Name, fill = value)) + 
  geom_tile(color = "black", lwd = 0.5, linetype = 1) +
  labs(x = "Metadata", y = "Organisms", title = "Heatmap by Module and Subclass") +
  scale_fill_gradientn(colours = c("darkred", "white", "skyblue"), na.value = "azure1",
                       values = scales::rescale(c(-0.7, -0.5, 0, 0.5, 0.7))) +
  scale_color_manual(values = c("black", "black"), guide = "none") +
  # Facet vertically by Subclass and Module
  facet_grid(Subclass + Module ~ ., space = "free", scales = "free", switch = "both") +
  theme_classic() +
  theme(strip.text.y.right = element_text(angle = 0, size = 11),
        strip.background.y = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_text(angle = 45, face = "italic", hjust = 1),
        plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(0.5, "lines")) 

# Print the plot
print(g)

# Save the plot
ggsave("heatmap_dat1_2.pdf", plot = g, width = 10, height = 15)
```
