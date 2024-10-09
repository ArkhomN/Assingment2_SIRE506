---
title: "Assignment 2: Data Analysis on Titanic Dataset"
author: "Niramai Arkhom"
date: "2024-09-11"
output: html_document
---

# Load necessary libraries
library(readr)
library(gtsummary)
library(gt)
library(dplyr)
library(ggplot2)

# Define the directory where your data is located
data_dir <- "C:/Users/User/Desktop/SIRE506/"

# Import a local CSV file (train.csv, Titanic dataset)
train <- read_csv(paste0(data_dir, "train.csv"))
View(train)  # Optional: View the dataset in a separate window

# Create a summary table using gtsummary
summary_table <- train %>%
  select(Survived, Age, Fare, SibSp, Parch, Sex, Pclass) %>%
  tbl_summary(
    by = Survived,
    statistic = list(
      all_continuous() ~ c("{median} ({IQR})"),  
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  modify_header(label = "**Characteristic**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Survival Status**") %>%
  as_gt() %>%
  gt::tab_header(
    title = "Table 1: Summary statistics of Titanic Passenger Data by Survival Status"
  ) %>%
  gt::tab_footnote(
    footnote = "0 = Not survived, 1 = Survived",
    locations = gt::cells_title(groups = "title")
  )

# Save the summary table as an HTML file
gtsave(summary_table, paste0(data_dir, "summary_table.html"))

# Create a function for generating boxplots
generate_boxplot <- function(df, x_var, y_var, x_labels, title, outlier_col, fill_col, outlier_shape, outlier_size) {
  ggplot(df, aes_string(x = x_var, y = y_var)) +
    geom_boxplot(outlier.colour = outlier_col, fill = fill_col, outlier.shape = outlier_shape, outlier.size = outlier_size, color = "black") +
    scale_x_discrete(labels = x_labels) +
    labs(x = "Survival Status", y = y_var, title = title) +
    theme_minimal()  # Optional: change the theme for a cleaner look
}

# Generate the boxplot for Age
boxplot_age <- generate_boxplot(
  train,
  "factor(Survived)",
  "Age",
  c("0" = "Not Survived", "1" = "Survived"),
  "Boxplot 1: Age Distribution by Survival Status",
  outlier_col = "red",       # Change outlier color
  fill_col = "lightblue",    # Change box fill color
  outlier_shape = 17,
  outlier_size = 3
)

# Generate the boxplot for Fare
boxplot_fare <- generate_boxplot(
  train,
  "factor(Survived)",
  "Fare",
  c("0" = "Not Survived", "1" = "Survived"),
  "Boxplot 2: Fare Distribution by Survival Status",
  outlier_col = "orange",    # Change outlier color
  fill_col = "lightgreen",    # Change box fill color
  outlier_shape = 17,
  outlier_size = 3
)

# Save the boxplots in the same directory
ggsave(paste0(data_dir, "boxplot_age.png"), plot = boxplot_age, width = 8, height = 6, dpi = 300)
ggsave(paste0(data_dir, "boxplot_fare.png"), plot = boxplot_fare, width = 8, height = 6, dpi = 300)

# Optionally, save the original dataset as a CSV file
write.csv(train, paste0(data_dir, "titanic_data.csv"), row.names = FALSE)

# End of script
