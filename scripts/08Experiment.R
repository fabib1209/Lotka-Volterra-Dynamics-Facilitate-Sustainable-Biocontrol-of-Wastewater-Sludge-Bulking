# Microcosm experiment 

## Experiment boxplots 

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(car)

# load data 
file_path1 <- "Experiment_Arcella.xlsx"
arc_ex <- read_excel(file_path1, sheet = 1)

dev.off()


# with dataset without oulier samples 
# Remove rows
arc_ex <- arc_ex[-c(4, 14, 24, 7, 17, 27 ), ]

arc_ex$Time <- factor(arc_ex$Time, levels = c("T6", "T14", "T19"))


# with lines connecting samples 
# Add an ID column to identify individual samples
arc_ex <- arc_ex %>%
  group_by(Time) %>%  # Group by Time
  mutate(Sample_ID = row_number()) %>%  # Assign a unique ID for each sample within each Time group
  ungroup()

    
# Recreate the boxplot with colored lines connecting samples
boxplot <- ggplot(arc_ex, aes(x = interaction(Treatment, Time), y = Growth, fill = Treatment)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "white", outlier.color = "black") +  # Boxplot
  geom_line(aes(group = Sample_ID, color = Treatment), alpha = 0.8, size = 0.5) +  # Connect samples by Sample_ID with matching colors
  labs(
    title = "Growth Comparison ",
    x = "Treatment and Time",
    y = "Growth"
  ) +
  scale_fill_manual(values = c("Arcella" = "orange1", "Control" = "brown")) +  # Custom fill colors
  scale_color_manual(values = c("Arcella" = "orange1", "Control" = "brown")) +  # Matching line colors
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better visibility
    plot.title = element_text(hjust = 0.5)
  )

# Print the updated boxplot
print(boxplot)

# Save the boxplot
ggsave("Growth_Boxplot.pdf", plot = boxplot, width = 10, height = 6)

# Modify the boxplot
boxplot <- ggplot(arc_ex, aes(x = interaction(Treatment, Time), y = Growth, fill = Treatment)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "white", outlier.color = "black") +  # Boxplot
  geom_line(aes(group = Sample_ID, color = Treatment), alpha = 0.8, size = 0.5) +  # Connect samples by Sample_ID with matching colors
  labs(
    title = "Growth Comparison",
    x = "Treatment and Time",
    y = "Growth"
  ) +
  scale_fill_manual(values = c("Arcella" = "orange1", "Control" = "brown")) +  # Custom fill colors
  scale_color_manual(values = c("Arcella" = "orange1", "Control" = "brown")) +  # Matching line colors
  theme_minimal(base_size = 14) +  # Adjust text size
  theme(
    panel.background = element_rect(fill = "grey95", color = NA),  # Slightly grey background
    panel.grid.major = element_line(color = "grey80", size = 0.5),  # Light gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)  # Center and style the title
  ) +
  scale_y_continuous(breaks = seq(-25, 25, by = 5))  # Y-axis with 5-step intervals

# Print the updated boxplot
print(boxplot)

# Save the boxplot
ggsave("Growth_Boxplot_Updated.pdf", plot = boxplot, width = 10, height = 6)


## t-test

# shapiro test to test for normality 
shapiro.test(arc_ex$Growth[arc_ex$Treatment == "Control" & arc_ex$Time == "T6"])
shapiro.test(arc_ex$Growth[arc_ex$Treatment == "Control" & arc_ex$Time == "T14"])
shapiro.test(arc_ex$Growth[arc_ex$Treatment == "Control" & arc_ex$Time == "T19"])
# all data is normally distributed so t test is ok 

# levene test to test for homogeneity
leveneTest(Growth ~ Treatment, data = arc_ex[arc_ex$Time == "T6", ])
# is at 0.49
leveneTest(Growth ~ Treatment, data = arc_ex[arc_ex$Time == "T14", ])
leveneTest(Growth ~ Treatment, data = arc_ex[arc_ex$Time == "T19", ])
# the other ones are not significant 

# do the t test 
t.test(Growth ~ Treatment, data = arc_ex[arc_ex$Time == "T6", ], var.equal = TRUE)
# p = 0.1643 
t.test(Growth ~ Treatment, data = arc_ex[arc_ex$Time == "T14", ], var.equal = TRUE)
# p = 0.1087
t.test(Growth ~ Treatment, data = arc_ex[arc_ex$Time == "T19", ], var.equal = TRUE)
# p = 0.04682



## MANOVA 

# Pivot data to wide format
arc_ex_wide <- arc_ex %>%
  pivot_wider(names_from = Time, values_from = Growth) %>%
  arrange(Treatment, Sample_ID)

# Check the structure of the wide-format data
print(arc_ex_wide)

# Create the matrix of dependent variables (Growth at each time point)
growth_matrix <- as.matrix(arc_ex_wide[, c("T6", "T14", "T19")])

# Fit the MANOVA model
manova_result <- manova(growth_matrix ~ Treatment, data = arc_ex_wide)

# Summarize the MANOVA results
summary(manova_result, test = "Wilks")  # Other options: "Pillai", "Hotelling-Lawley", "Roy"

# pr(>f) 0.0491
# wilks test is very low so the model explains most of the variance 
