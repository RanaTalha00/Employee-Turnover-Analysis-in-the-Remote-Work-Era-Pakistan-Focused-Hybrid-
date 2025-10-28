# =========================================
# 04_integrated_analysis.R
# Purpose: Integrated Employee Turnover Analysis (IBM HR + Rozee + Pakistan Employment)
# Robust: Handles missing Remote_Status, Sector, small datasets
# =========================================

# ------------------------------
# 1. Load libraries
# ------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lattice)
library(reshape2)
library(scales)
library(ggpubr)

# ------------------------------
# 2. Load cleaned datasets
# ------------------------------
ibm_path <- "C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/IBM HR Cleaned.csv"
rozee_path <- "C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/Pakistan Job Market Cleaned.csv"
employment_path <- "C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/Pakistan Employment Cleaned.csv"

ibm <- read_csv(ibm_path)
rozee <- read_csv(rozee_path)
employment <- read_csv(employment_path)

# ------------------------------
# 3. Column mapping
# ------------------------------

# IBM
if(!("Remote_Status" %in% colnames(ibm))) ibm$Remote_Status <- NA
if(!("OverTime" %in% colnames(ibm))) ibm$OverTime <- NA

# Rozee
possible_industry_cols <- c("FunctionalArea","Functional_Area","Functional Area","Industry","Sector")
industry_col <- possible_industry_cols[possible_industry_cols %in% colnames(rozee)]
if(length(industry_col)==1) colnames(rozee)[colnames(rozee)==industry_col] <- "Industry"

possible_remote_cols <- c("Remote_Status","Remote","WorkFromHome","WFH")
remote_col <- possible_remote_cols[possible_remote_cols %in% colnames(rozee)]
remote_available <- FALSE
if(length(remote_col)==1){
  colnames(rozee)[colnames(rozee)==remote_col] <- "Remote_Status"
  rozee$Remote_Status <- as.factor(rozee$Remote_Status)
  remote_available <- TRUE
}

# Employment
possible_sector_cols <- c("Sector","Industry","FunctionalArea","Functional_Area","Functional Area")
sector_col <- possible_sector_cols[possible_sector_cols %in% colnames(employment)]
sector_available <- FALSE
if(length(sector_col)==1){
  colnames(employment)[colnames(employment)==sector_col] <- "Sector"
  sector_available <- TRUE
  cat("✅ Sector column found in Employment dataset:", sector_col, "\n")
} else {
  cat("⚠️ No Sector column found in Employment dataset. Sector-based metrics will be skipped.\n")
}

# ------------------------------
# 4. Summaries
# ------------------------------
# Rozee
rozee_summary <- if(remote_available){
  rozee %>% group_by(Industry) %>%
    summarise(TotalJobs = n(),
              RemoteJobs = sum(Remote_Status=="Remote", na.rm=TRUE),
              RemotePercent = RemoteJobs/TotalJobs*100)
} else {
  rozee %>% group_by(Industry) %>%
    summarise(TotalJobs = n(),
              RemoteJobs = NA,
              RemotePercent = NA)
}

# IBM HR
ibm_summary <- ibm %>% group_by(Department) %>%
  summarise(TotalEmployees = n(),
            AttritionCount = sum(Attrition=="Yes", na.rm=TRUE),
            AttritionPercent = AttritionCount/TotalEmployees*100,
            AvgJobSatisfaction = mean(JobSatisfaction, na.rm=TRUE),
            OvertimeRate = mean(ifelse(OverTime=="Yes",1,0), na.rm=TRUE))

# Employment
if(sector_available){
  employment_summary <- employment %>% group_by(Sector) %>%
    summarise(EmploymentShare = sum(EmploymentCount, na.rm=TRUE)/sum(employment$EmploymentCount, na.rm=TRUE)*100)
} else {
  employment_summary <- data.frame(Sector=NA, EmploymentShare=NA)
}

# ------------------------------
# 5. Merge datasets safely
# ------------------------------
merged <- ibm_summary %>%
  left_join(rozee_summary, by=c("Department"="Industry"))

if(sector_available){
  merged <- merged %>%
    left_join(employment_summary, by=c("Department"="Sector"))
} else {
  merged$EmploymentShare <- NA
}

# ------------------------------
# 6. Derived Indicators
# ------------------------------
merged <- merged %>%
  mutate(RemoteWorkPotentialIndex = ifelse(!is.na(RemotePercent) & !is.na(EmploymentShare),
                                           RemotePercent * EmploymentShare /100, NA),
         AttritionRiskIndex = ifelse(!is.na(AttritionPercent) & !is.na(AvgJobSatisfaction) & !is.na(OvertimeRate),
                                     (AttritionPercent / AvgJobSatisfaction) * OvertimeRate, NA),
         RetentionReadinessIndex = 100 - AttritionRiskIndex)

# ------------------------------
# 7. Visualization - KPI Dashboard
# ------------------------------

# 7a. Scatter: Attrition vs RemoteWorkPotential
ggplot(merged, aes(x=RemoteWorkPotentialIndex, y=AttritionPercent, label=Department)) +
  geom_point(color="steelblue", size=4) +
  geom_text(vjust=-0.8, size=3) +
  theme_minimal() +
  labs(title="Attrition vs Remote Work Potential Index",
       x="Remote Work Potential Index (%)",
       y="Attrition %")

# 7b. Heatmap: Attrition Risk Index (only if enough rows/columns)
valid_heatmap <- merged %>% filter(!is.na(AttritionRiskIndex))
if(nrow(valid_heatmap) >= 2){
  heat_matrix <- valid_heatmap %>% select(Department, AttritionRiskIndex) %>% column_to_rownames("Department")
  if(ncol(heat_matrix) >=1){
    heatmap(as.matrix(heat_matrix), main="Attrition Risk Index by Department", col=heat.colors(20), scale="column")
  } else {
    cat("⚠️ Heatmap skipped: Not enough columns.\n")
  }
} else {
  cat("⚠️ Heatmap skipped: Not enough data rows.\n")
}

# 7c. Top 5 Retention Ready Departments
top_retention <- merged %>% arrange(desc(RetentionReadinessIndex)) %>% head(5)
print(top_retention)

# ------------------------------
# 8. Save merged dataset & metrics
# ------------------------------
write_csv(merged, "C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/integrated_analysis.csv")
saveRDS(merged, "C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/integrated_analysis.rds")

cat("\n✅ Integrated analysis completed successfully. Metrics and visualizations are ready.\n")
