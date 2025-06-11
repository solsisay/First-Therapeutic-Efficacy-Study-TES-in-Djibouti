library(readxl)
library(lubridate)
library(gtsummary)
library(tidycmprsk)
library(tidyverse)
library(ggprism)
library(ggpubr)
library(gridExtra)
library(ggplot2)
library(tidyr)
library(ggfortify)
library(dplyr)
library(haven)
Djibuti_21 = read.xlsx ("D:\\Pv_TES2023\\final out put_Tes23_24\\Plot\\Djibuti\\all documents of Djibouti\\Data\\fig3C gam profile.xlsx")
# make it as numeric
Djibuti_21$mic_dens <- as.numeric(gsub(",", "", Djibuti_21$mic_dens))
Djibuti_21$pf_par_ul <- as.numeric(gsub(",", "", Djibuti_21$pf_par_ul))
Djibuti_21$gam_dens <- as.numeric(gsub(",", "", Djibuti_21$gam_dens))
# Filter out NA values in count_gametocyte_expert
Djibuti <- Djibuti_21 %>%
  filter(!is.na(gam_dens)) %>%
  mutate(size = case_when(
    gam_dens == 0 ~ "0",
    gam_dens > 0 & gam_dens <= 50 ~ "1-50",
    gam_dens > 50 & gam_dens <= 100 ~ "50-100",
    gam_dens > 100 ~ ">100"
  )) %>%
  mutate(size = factor(size, levels = c("0", "1-50", "50-100", ">100")))
# custom size labels
Djibuti <- Djibuti %>% 
  filter(day %in% c(0, 1, 2, 3, 7, 14, 21, 28))
Djibuti$day <- factor(Djibuti$day, levels = c(0, 1, 2, 3, 7, 14, 21, 28))

###
# Convert 'size' to numeric (assuming categories like "0", "1-50", "50-100", etc.)
Djibuti$size_numeric1 <- as.numeric(factor(Djibuti$size, levels = c("0", "1-50", "50-100", ">100")))
table(Djibuti$size_numeric1)
# Define size mapping (adjust these values as needed)
size_mapping <- c( 0.75, 1, 1.25, 1.5)  # Corresponding to "0", "1-50", "50-100", "100+"

Djibuti_gam <- ggplot(Djibuti, aes(x = id, y = day, color = gam_status, shape = mic_status)) +
  geom_point(aes(size = size_numeric1), fill = "black", stroke = 0.1) +
  # Map discrete sizes to numeric values
  scale_size_continuous(
    range = c(0.75, 1.5),  # Adjust min/max sizes
    breaks = c(1, 2, 3, 4),  # Corresponding to factor levels
    labels = c("0", "1-50", "50-100", "100+")  # Original labels
  ) +
  # Match legend keys to the new sizes
  guides(
    size = guide_legend(override.aes = list(size = size_mapping))  # Legend icons for each category
  ) +
  # Preserve your existing scales and themes
  scale_shape_manual(values = c("Negative" = 1, "Positive" = 16), labels = c("Negative", "Positive")) +
  scale_color_manual(values = c("Negative" = "#0072b5", "Positive" = "#BC3C29FF"), labels = c("Negative", "Positive")) +
  theme_pubr() +
  labs(
    x = "ID", y = "Days",
    color = "Gameteocyte (Mic)", shape = "Asexual parasite", size = "Gameteocyte density"
  ) +
  ggtitle("") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 4, family = "Times New Roman"),
    axis.text.y = element_text(size = 4, family = "Times New Roman"),
    axis.title = element_text(size = 4, family = "Times New Roman"),
    plot.title = element_text(size = 4, face = "bold", family = "Times New Roman"),
    axis.ticks = element_line(size = 0.15),
    axis.ticks.length = unit(0.01, "cm"),
    axis.line = element_line(size = 0.1),
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 4,
                              family = "Times New Roman"),
    legend.title = element_text(size = 4, family = "Times New Roman"),
    
    legend.key.size = unit(0.2, "cm"),
    legend.spacing = unit(0.05, "cm"),
    # NEW: Reduce space between legend and x-axis
    legend.margin = margin(t = -0.1, unit = "cm"),  # Negative margin pulls legend up
    legend.box.spacing = unit(0.1, "cm")  # Tightens spacing around legend box
)
# Print the plot
Djibuti_gam



