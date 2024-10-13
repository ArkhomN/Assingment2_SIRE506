---
title: "Assignment2"
author: "Niramai Arkhom"
output: html_document
---

# Load required packages, install if necessary
if (!require("gtsummary")) install.packages("gtsummary")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("gt")) install.packages("gt")

# Load libraries
library(gtsummary)
library(ggplot2)
library(dplyr)
library(readr)
library(gt)

# Read data: Make path relative so it can work on other machines
titanic_data <- read_csv("../data/train.csv")  
View(titanic_data)

# Create a summary table grouped by 'Survived'
summary_table <- titanic_data %>%
  select(Survived, Age, Fare, SibSp, Parch, Sex, Pclass) %>%
  tbl_summary(
    by = Survived,
    statistic = list(
      all_continuous() ~ "{median} ({IQR})",  
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  modify_header(label = "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Survival Status**") %>%
  as_gt() %>%
  tab_header(
    title = "Table 1: Summary of Titanic Data by Survival Status"
  ) %>%
  tab_footnote(
    footnote = "0 = Not Survived, 1 = Survived",
    locations = cells_title(groups = "title")
  )

# Display the summary table in RStudio viewer
summary_table

# Function to generate boxplots
generate_boxplot <- function(df, x_var, y_var, x_labels, title, outlier_col, fill_col, outlier_shape, outlier_size) {
  ggplot(df, aes_string(x = x_var, y = y_var)) +
    geom_boxplot(outlier.colour = outlier_col, fill = fill_col, outlier.shape = outlier_shape, outlier.size = outlier_size, color = "black") +
    scale_x_discrete(labels = x_labels) +
    labs(x = "Survival Status", y = y_var, title = title) +
    theme_minimal()  
}

# Create boxplot for Age vs. Survival Status
boxplot_age <- generate_boxplot(
  titanic_data,
  "factor(Survived)",
  "Age",
  c("0" = "Not Survived", "1" = "Survived"),
  "Boxplot 1: Age Distribution by Survival Status",
  outlier_col = "red",       
  fill_col = "lightblue",    
  outlier_shape = 17,
  outlier_size = 3
)

# Create boxplot for Fare vs. Survival Status
boxplot_fare <- generate_boxplot(
  titanic_data,
  "factor(Survived)",
  "Fare",
  c("0" = "Not Survived", "1" = "Survived"),
  "Boxplot 2: Fare Distribution by Survival Status",
  outlier_col = "orange",    
  fill_col = "lightgreen",   
  outlier_shape = 17,
  outlier_size = 3
)

# Display boxplots in RStudio viewer
boxplot_age
boxplot_fare
