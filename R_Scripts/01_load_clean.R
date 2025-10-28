# =========================================
# 01_load_clean.R
# Purpose: Load & check cleaned datasets for Employee Turnover Pakistan Project
# Ensures datasets appear in Environment pane
# =========================================

# ------------------------------
# 1. Install missing packages
# ------------------------------
packages <- c(
  "tidyverse", "ggplot2", "dplyr", "corrplot",
  "caret", "randomForest", "ggpubr", "reshape2",
  "ROCR", "scales", "lubridate", "lattice"
)

installed <- packages %in% rownames(installed.packages())
if(any(!installed)){
  install.packages(packages[!installed], dependencies = TRUE)
}

# ------------------------------
# 2. Load libraries
# ------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(randomForest)
library(ggpubr)
library(reshape2)
library(ROCR)
library(scales)
library(lubridate)
library(lattice)

# ------------------------------
# 3. Set file paths
# ------------------------------
ibm_path <- "C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/IBM HR Cleaned.csv"
rozee_path <- "C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/Pakistan Job Market Cleaned.csv"
employment_path <- "C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/Pakistan Employment Cleaned.csv"

# ------------------------------
# 4. Load datasets as base data.frames
# ------------------------------
ibm <- as.data.frame(read_csv(ibm_path))
rozee <- as.data.frame(read_csv(rozee_path))
employment <- as.data.frame(read_csv(employment_path))

# ------------------------------
# 5. Quick overview for each dataset
# ------------------------------
cat("\n--- IBM HR Analytics Dataset ---\n")
str(ibm)
summary(ibm)
head(ibm, 5)

cat("\n--- Pakistan Job Market (Rozee.pk) ---\n")
str(rozee)
summary(rozee)
head(rozee, 5)

cat("\n--- Pakistan Employment Dataset 2023 ---\n")
str(employment)
summary(employment)
head(employment, 5)

# ------------------------------
# 6. Check missing values (optional)
# ------------------------------
cat("\n--- Missing Values Check ---\n")
sapply(ibm, function(x) sum(is.na(x)))
sapply(rozee, function(x) sum(is.na(x)))
sapply(employment, function(x) sum(is.na(x)))

# ------------------------------
# 7. Save R environment snapshot (optional)
# ------------------------------
save.image(file = "C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/R_Scripts/Env_Stage1.RData")

cat("\n✅ All datasets loaded successfully, appear in Environment pane, and environment saved.\n")
