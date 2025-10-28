# =========================================
# 05_summary_report.R
# Purpose: Auto-generate insights summary from integrated analysis
# =========================================

library(tidyverse)

data <- read_csv("C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/integrated_analysis.csv")

# ---- Top insights ----
cat("\n📊 Summary Insights — Employee Turnover Pakistan Project\n")

cat("\n1️⃣ Top 5 Departments with Highest Attrition:\n")
data %>%
  arrange(desc(AttritionPercent)) %>%
  select(Department, AttritionPercent) %>%
  head(5) %>%
  print()

cat("\n2️⃣ Top 5 Departments with Best Retention Readiness:\n")
data %>%
  arrange(desc(RetentionReadinessIndex)) %>%
  select(Department, RetentionReadinessIndex) %>%
  head(5) %>%
  print()

cat("\n3️⃣ Departments with Highest Remote Work Potential:\n")
data %>%
  arrange(desc(RemoteWorkPotentialIndex)) %>%
  select(Department, RemoteWorkPotentialIndex) %>%
  head(5) %>%
  print()

cat("\n✅ Insight Generation Complete.\n")
