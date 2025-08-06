# Microcosm experiment

# Load libraries
```
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggpubr)
library(car)

# Load data
file_path <- "Growth Experiment_new.xlsx"
arc_ex <- read_excel(file_path, sheet = "Tabelle1")

# Clean up: remove rows with missing Growth
arc_ex <- arc_ex %>% filter(!is.na(Growth))

# Convert factors
arc_ex$Time <- factor(arc_ex$Time, levels = c("T0", "T6", "T14", "T21"))
arc_ex$Treatment <- factor(arc_ex$Treatment, levels = c("1A", "1B", "2A", "2B"))
arc_ex$Arcella <- factor(arc_ex$Arcella, levels = c("FALSE", "TRUE"))

# Create ID for line connections — based on Well
arc_ex <- arc_ex %>% mutate(Sample_ID = Well)

# create grouping label
arc_ex <- arc_ex %>%
  mutate(Group = ifelse(Arcella == "TRUE", "Arcella", "Control"))
```


## Outlier detection

```
# we need new sample id with treatment number 

arc_ex <- arc_ex %>%
  mutate(Sample_ID = paste(Temperature, Well, Treatment, sep = "_"))


# Split the data for better visuals 
# 13 degrees
arc_13 <- arc_ex %>% filter(Temperature == 13)
# Arcella 
arc_13_arcella <- arc_13 %>% filter(Group == "Arcella")

ggplot(arc_13_arcella, aes(x = Time, y = Growth, group = Sample_ID, color = Sample_ID)) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Growth Trajectories at 13 °C – Arcella",
    x = "Time", y = "Growth"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# visual outliers
# 13_A9_2B
# 13_A11_2B

# Control
arc_13_control <- arc_13 %>% filter(Group == "Control")

ggplot(arc_13_control, aes(x = Time, y = Growth, group = Sample_ID, color = Sample_ID)) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Growth Trajectories at 13 °C – Control",
    x = "Time", y = "Growth"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# visual outliers
# 13_K7_2B
# 13_K8_2B



# for 20 degrees 

arc_20 <- arc_ex %>% filter(Temperature == 20)

# Arcella 
arc_20_arcella <- arc_20 %>% filter(Group == "Arcella")

ggplot(arc_20_arcella, aes(x = Time, y = Growth, group = Sample_ID, color = Sample_ID)) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Growth Trajectories at 20 °C – Arcella",
    x = "Time", y = "Growth"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# visual outliers
# 20_A11_1A

# Control
arc_20_control <- arc_20 %>% filter(Group == "Control")

ggplot(arc_20_control, aes(x = Time, y = Growth, group = Sample_ID, color = Sample_ID)) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Growth Trajectories at 20 °C – Control",
    x = "Time", y = "Growth"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# visual outliers
# 20_K4_1A
# 20_K3_1A
# 20_K5_1A
# 20_K11_1B



## removing the outliers from the dataset
outliers <- c("13_A9_2B", "13_A11_2B", "13_K7_2B", "13_K8_2B", "20_A11_1A", "20_K4_1A", "20_K3_1A", "20_K5_1A", "20_K11_1B")

arc_ex_clean <- arc_ex %>%
  filter(!Sample_ID %in% outliers)

## combinig 1a/B and 2a/b because they are replicates 
# we need new sample id 
arc_ex_clean_new <- arc_ex_clean %>%
  mutate(Sample_ID = paste(Temperature, Well, sep = "_"))
```

## STATS 
We started with testing for narmality and homogeneity. Because of violations in both normailty and homogeneity we discarded the t-test and did Welch's t-test instead.

```
# Use cleaned and restructured data
arc_stats <- arc_ex_clean_new

# Shapiro tests (normality) per timepoint & group
for (t in c("T6", "T14", "T21")) {
  cat("\nShapiro test - Control at", t, ":\n")
  print(shapiro.test(arc_stats$Growth[arc_stats$Time == t & arc_stats$Group == "Control"]))
  
  cat("Shapiro test - Arcella at", t, ":\n")
  print(shapiro.test(arc_stats$Growth[arc_stats$Time == t & arc_stats$Group == "Arcella"]))
}

# Levene tests (homogeneity of variances)
for (t in c("T6", "T14", "T21")) {
  cat("\nLevene Test at", t, ":\n")
  print(leveneTest(Growth ~ Group, data = arc_stats[arc_stats$Time == t, ]))
}

# T-tests // BUT WE DONT DO THAT BECAUSE OF VIOLATIONS
for (t in c("T6", "T14", "T21")) {
  cat("\nT-test at", t, ":\n")
  print(t.test(Growth ~ Group, data = arc_stats[arc_stats$Time == t, ], var.equal = TRUE))
}

# WE DO THE WELCH T TEST 
for (t in c("T6", "T14", "T21")) {
  cat("\nWelch's T-test at", t, ":\n")
  print(t.test(Growth ~ Group, data = arc_stats[arc_stats$Time == t, ], var.equal = FALSE))
}
```

## MANOVA 
The MANOVA tested for differences in control and arcella groups as well in connection to temperature.

```
# Keep only timepoints of interest
arc_cleaned_manova <- arc_stats %>%
  filter(Time %in% c("T6", "T14", "T21")) %>%
  group_by(Sample_ID, Group, Temperature, Time) %>%
  summarise(Growth = mean(Growth), .groups = "drop")

# Reshape to wide format
arc_wide <- arc_cleaned_manova %>%
  pivot_wider(names_from = Time, values_from = Growth)

# Drop incomplete cases
arc_wide <- na.omit(arc_wide)

# Check structure
str(arc_wide)

# Create growth matrix
growth_matrix <- as.matrix(arc_wide[, c("T6", "T14", "T21")])

# Fit MANOVA
manova_result <- manova(growth_matrix ~ Group * Temperature, data = arc_wide)

# Results
cat("\nMANOVA result (Wilks):\n")
summary(manova_result, test = "Wilks")

cat("\nUnivariate ANOVAs:\n")
summary.aov(manova_result)
```

## Creating Lineplot with standard deviation

To show how the both groups differ from one another at both temperatures a lineplot with half of the standard deviation was created. 


```
# Summary stats
arc_summary <- arc_ex_clean_new %>%
  group_by(Temperature, Group, Time) %>%
  summarise(
    mean_growth = mean(Growth, na.rm = TRUE),
    sd_growth = sd(Growth, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Time_num = as.numeric(factor(Time, levels = c("T6", "T14", "T21"))),
    sd_growth = sd_growth / 2  # halve the SD here
  )

# Raw data with numeric time
arc_ex_clean_new <- arc_ex_clean_new %>%
  mutate(Time_num = as.numeric(factor(Time, levels = c("T6", "T14", "T21"))))

# Plot with ribbons and error bars
g <- ggplot() +
  
  # Soft ribbon 
  geom_ribbon(
    data = arc_summary,
    aes(
      x = Time_num,
      ymin = mean_growth - sd_growth,
      ymax = mean_growth + sd_growth,
      fill = as.factor(Temperature),
      group = interaction(Temperature, Group)
    ),
    alpha = 0.2,
  )+
  
  # Error bars
  geom_errorbar(
    data = arc_summary,
    aes(
      x = Time_num,
      ymin = mean_growth - sd_growth,
      ymax = mean_growth + sd_growth,
      color = as.factor(Temperature),
      linetype = Group
    ),
    width = 0.2,
    position = position_dodge(width = 0.08)
  ) +
  
  # Raw data points with jitter 
  geom_jitter(
    data = arc_ex_clean_new,
    aes(
      x = Time_num,
      y = Growth,
      color = as.factor(Temperature),
      shape = Group
    ),
    width = 0.08,
    alpha = 0.8,
    size = 2
  ) +
  
  # Regression lines
  geom_smooth(
    data = arc_ex_clean_new,
    aes(
      x = Time_num,
      y = Growth,
      color = as.factor(Temperature),
      linetype = Group
    ),
    method = "lm",
    se = FALSE,
    linewidth = 1
  ) +
  
  # Axis and theme
  scale_color_manual(values = c("13" = "skyblue", "20" = "darkred")) +
  scale_fill_manual(values = c("13" = "skyblue", "20" = "darkred")) +
  scale_linetype_manual(values = c("Arcella" = "solid", "Control" = "dotdash")) +
  scale_shape_manual(values = c("Arcella" = 16, "Control" = 17)) +
  scale_x_continuous(breaks = 1:3, labels = c("T6", "T14", "T21")) +
  labs(
    x = "Time", y = "Growth",
    color = "Temperature (°C)",
    fill = "Temperature (°C)",
    linetype = "Treatment",
    shape = "Treatment"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right",
    panel.background = element_rect(fill = "grey99", color = NA)
  )

# Display
print(g)
# Save plot
ggsave("lineplot_combined_half_sd_.pdf", plot = g, width = 10, height = 6)

```
