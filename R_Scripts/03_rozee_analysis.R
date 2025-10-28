# =========================================
# 03_rozee_analysis.R
# Purpose: Pakistan Job Market Analysis (Cleaned Rozee.pk Dataset)
# Robust: Auto-detects all required columns, Remote_Status optional
# =========================================

# ------------------------------
# 1. Load libraries
# ------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lattice)
library(scales)
library(reshape2)
library(lubridate)
library(ggpubr)

# ------------------------------
# 2. Load Pakistan Job Market dataset
# ------------------------------
rozee_path <- "C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/Pakistan Job Market Cleaned.csv"
rozee <- as.data.frame(read_csv(rozee_path))

# ------------------------------
# 3. Robust column detection
# ------------------------------

# Industry / Sector
possible_industry_cols <- c("FunctionalArea", "Functional_Area", "Functional Area", "Industry", "Sector")
industry_col <- possible_industry_cols[possible_industry_cols %in% colnames(rozee)]
if(length(industry_col) == 1){
  colnames(rozee)[colnames(rozee) == industry_col] <- "Industry"
} else {
  stop("No valid Industry column found. Check column names using colnames(rozee).")
}

# Job Location / Province / City
possible_location_cols <- c("Job_Location", "Job Location", "Location", "Province", "City")
location_col <- possible_location_cols[possible_location_cols %in% colnames(rozee)]
if(length(location_col) == 1){
  colnames(rozee)[colnames(rozee) == location_col] <- "Job_Location"
} else {
  stop("No valid Job_Location column found. Check column names using colnames(rozee).")
}

# Career Level / Experience
possible_career_cols <- c("Career_Level", "Career Level", "Experience_Level", "Level")
career_col <- possible_career_cols[possible_career_cols %in% colnames(rozee)]
if(length(career_col) == 1){
  colnames(rozee)[colnames(rozee) == career_col] <- "Career_Level"
} else {
  stop("No valid Career_Level column found. Check column names using colnames(rozee).")
}

# Remote Status (optional)
possible_remote_cols <- c("Remote_Status", "Remote", "WorkFromHome", "WFH")
remote_col <- possible_remote_cols[possible_remote_cols %in% colnames(rozee)]
if(length(remote_col) == 1){
  colnames(rozee)[colnames(rozee) == remote_col] <- "Remote_Status"
  rozee$Remote_Status <- as.factor(rozee$Remote_Status)
  remote_available <- TRUE
  cat("✅ Remote_Status column found:", remote_col, "\n")
} else {
  cat("⚠️ No Remote_Status column found. Remote-based plots and metrics will be skipped.\n")
  remote_available <- FALSE
}

# Salary
possible_salary_cols <- c("Salary", "Salary_PKR", "MonthlySalary")
salary_col <- possible_salary_cols[possible_salary_cols %in% colnames(rozee)]
if(length(salary_col) == 1){
  colnames(rozee)[colnames(rozee) == salary_col] <- "Salary"
  # Convert to numeric if needed
  if(!is.numeric(rozee$Salary)){
    rozee$Salary <- as.numeric(gsub(",", "", rozee$Salary))
  }
} else {
  stop("No valid Salary column found. Check column names using colnames(rozee).")
}

# ------------------------------
# 4. Descriptive Analysis
# ------------------------------

cat("\n--- Total Job Postings ---\n")
n_total <- nrow(rozee)
print(n_total)

# Remote vs On-site counts
if(remote_available){
  remote_table <- table(rozee$Remote_Status)
  print(remote_table)
  cat("Remote Jobs Percentage:", round(prop.table(remote_table)*100,2), "%\n")
}

# Jobs by Industry
job_industry <- if(remote_available){
  rozee %>% group_by(Industry) %>%
    summarise(Count = n(),
              RemoteCount = sum(Remote_Status=="Remote", na.rm=TRUE),
              RemotePercent = RemoteCount/Count*100) %>%
    arrange(desc(Count))
} else {
  rozee %>% group_by(Industry) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count))
}
print(job_industry)

# Jobs by Location
job_location <- if(remote_available){
  rozee %>% group_by(Job_Location) %>%
    summarise(Count = n(),
              RemoteCount = sum(Remote_Status=="Remote", na.rm=TRUE),
              RemotePercent = RemoteCount/Count*100) %>%
    arrange(desc(Count))
} else {
  rozee %>% group_by(Job_Location) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count))
}
print(job_location)

# Jobs by Career Level
job_career <- if(remote_available){
  rozee %>% group_by(Career_Level) %>%
    summarise(Count = n(),
              RemoteCount = sum(Remote_Status=="Remote", na.rm=TRUE),
              RemotePercent = RemoteCount/Count*100)
} else {
  rozee %>% group_by(Career_Level) %>%
    summarise(Count = n())
}
print(job_career)

# Salary summary
salary_summary <- if(remote_available){
  rozee %>% group_by(Remote_Status) %>%
    summarise(MinSalary = min(Salary, na.rm=TRUE),
              MaxSalary = max(Salary, na.rm=TRUE),
              MeanSalary = mean(Salary, na.rm=TRUE),
              MedianSalary = median(Salary, na.rm=TRUE))
} else {
  rozee %>% summarise(MinSalary = min(Salary, na.rm=TRUE),
                      MaxSalary = max(Salary, na.rm=TRUE),
                      MeanSalary = mean(Salary, na.rm=TRUE),
                      MedianSalary = median(Salary, na.rm=TRUE))
}
print(salary_summary)

# ------------------------------
# 5. Visualizations
# ------------------------------

# Bar chart: Jobs by Industry
barchart(Count ~ Industry, data=job_industry,
         main="Job Postings by Industry", xlab="Count", ylab="Industry")

# Bar chart: Jobs by Location
barchart(Count ~ Job_Location, data=job_location,
         main="Job Postings by Location", xlab="Count", ylab="Location")

# Boxplot: Salary vs Remote_Status
if(remote_available){
  bwplot(Salary ~ Remote_Status, data=rozee,
         main="Salary Comparison: Remote vs On-site", xlab="Remote Status", ylab="Salary (PKR)")
}

# Optional: Time trend if Apply_Before exists
if("Apply_Before" %in% colnames(rozee)){
  rozee$Apply_Before <- as.Date(rozee$Apply_Before, format="%Y-%m-%d")
  jobs_by_month <- if(remote_available){
    rozee %>% mutate(Month=format(Apply_Before, "%Y-%m")) %>%
      group_by(Month, Remote_Status) %>% summarise(JobCount=n())
  } else {
    rozee %>% mutate(Month=format(Apply_Before, "%Y-%m")) %>%
      group_by(Month) %>% summarise(JobCount=n())
  }
  
  ggplot(jobs_by_month, aes(x=Month, y=JobCount, color=if(remote_available) Remote_Status else NULL, group=if(remote_available) Remote_Status else 1)) +
    geom_line(size=1) + geom_point() +
    theme_minimal() +
    labs(title="Monthly Job Posting Trend", x="Month", y="Number of Jobs") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
}

# ------------------------------
# 6. Key Insights / Metrics
# ------------------------------
if(remote_available){
  top_remote_industries <- job_industry %>% arrange(desc(RemoteCount)) %>% head(5)
  print(top_remote_industries)
  remote_percent_total <- sum(rozee$Remote_Status=="Remote", na.rm=TRUE)/n_total*100
  cat("\nTotal Remote Jobs Percentage:", round(remote_percent_total,2), "%\n")
  
  # Average salary difference
  avg_salary_remote <- salary_summary$MeanSalary[salary_summary$Remote_Status=="Remote"]
  avg_salary_onsite <- salary_summary$MeanSalary[salary_summary$Remote_Status=="On-site"]
  cat("\nAverage Salary - Remote:", round(avg_salary_remote,0),
      "PKR | On-site:", round(avg_salary_onsite,0), "PKR\n")
}

# ------------------------------
# 7. Save Summary Metrics
# ------------------------------
summary_metrics <- list(
  TotalJobs = n_total,
  RemotePercent = if(remote_available) remote_percent_total else NA,
  TopRemoteIndustries = if(remote_available) top_remote_industries else NA,
  SalarySummary = salary_summary
)

saveRDS(summary_metrics, "C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/rozee_summary_metrics.rds")

cat("\n✅ Pakistan Job Market analysis completed successfully.\n")
