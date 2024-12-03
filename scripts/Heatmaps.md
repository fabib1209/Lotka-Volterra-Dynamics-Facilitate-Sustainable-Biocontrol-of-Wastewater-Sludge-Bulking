# Calculating correlation coefficients and heatmap creation

We show how Spearmans Rank correlation was used to create heatmaps for every dataset on te example of dataset1.

## 01 Spearmans Rank correlation

We create organism groups based on taxonomy. These are different for every dataset because of differently describes species, this is exemplary for dataset1. Then we calculate the correlation coefficients first between all organisms and the metadata describes for the dataset.

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
