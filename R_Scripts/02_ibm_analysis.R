# =========================================
# 02_ibm_analysis.R
# Purpose: IBM HR Analytics Employee Attrition Analysis
# Updated: Auto-save plots as PNG, robust Remote_Status & Overtime
# =========================================

# ------------------------------
# 1. Load libraries
# ------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(randomForest)
library(reshape2)
library(ROCR)
library(scales)
library(lattice)

# ------------------------------
# 2. Create plots folder
# ------------------------------
if(!dir.exists("Plots")) dir.create("Plots")

# ------------------------------
# 3. Load IBM dataset
# ------------------------------
ibm_path <- "C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/IBM HR Cleaned.csv"
ibm <- as.data.frame(read_csv(ibm_path))

# ------------------------------
# 4. Detect Remote_Status
# ------------------------------
possible_remote_cols <- c("Remote", "is_remote", "WorkFromHome", "WFH", "RemoteStatus")
remote_col <- possible_remote_cols[possible_remote_cols %in% colnames(ibm)]

if(length(remote_col) == 1){
  colnames(ibm)[colnames(ibm) == remote_col] <- "Remote_Status"
  if(is.numeric(ibm$Remote_Status)){
    ibm$Remote_Status <- factor(ibm$Remote_Status, levels=c(0,1), labels=c("Onsite","Remote"))
  }
  remote_available <- TRUE
} else {
  cat("⚠️ No valid Remote_Status column found. Remote-based plots and models will be skipped.\n")
  remote_available <- FALSE
}

# ------------------------------
# 5. Detect Overtime
# ------------------------------
possible_overtime_cols <- c("OverTime", "OvertimeHours", "OT", "ExtraHours")
overtime_col <- possible_overtime_cols[possible_overtime_cols %in% colnames(ibm)]

if(length(overtime_col) == 1){
  colnames(ibm)[colnames(ibm) == overtime_col] <- "Overtime"
  if(is.numeric(ibm$Overtime)){
    ibm$Overtime <- factor(ibm$Overtime, levels=c(0,1), labels=c("No","Yes"))
  }
  overtime_available <- TRUE
} else {
  cat("⚠️ No valid Overtime column found. Overtime variable will be skipped in models and plots.\n")
  overtime_available <- FALSE
}

# ------------------------------
# 6. Ensure Attrition is factor
# ------------------------------
ibm$Attrition <- as.factor(ibm$Attrition)

# ------------------------------
# 7. Descriptive Analysis
# ------------------------------
cat("\n--- Total Employees ---\n")
n_total <- nrow(ibm)
print(n_total)

cat("\n--- Attrition Count and Percentage ---\n")
table_attr <- table(ibm$Attrition)
print(table_attr)
print(round(prop.table(table_attr) * 100, 2))

# ------------------------------
# 8. Exploratory Data Visualization
# ------------------------------

# --- Bar: Attrition by Department ---
png("Plots/Attrition_by_Department.png", width=1400, height=900)
barchart(Attrition ~ Department, groups=Attrition, data=ibm,
         stack=FALSE, auto.key=list(columns=2),
         main="Attrition by Department", xlab="Department", ylab="Count")
dev.off()

# --- Bar: Attrition by Job Role ---
png("Plots/Attrition_by_JobRole.png", width=1400, height=900)
barchart(Attrition ~ JobRole, groups=Attrition, data=ibm,
         stack=FALSE, auto.key=list(columns=2),
         main="Attrition by Job Role", xlab="Job Role", ylab="Count")
dev.off()

# --- Bar: Attrition by Remote Status ---
if(remote_available){
  png("Plots/Attrition_by_RemoteStatus.png", width=800, height=600)
  barchart(Attrition ~ Remote_Status, groups=Attrition, data=ibm,
           stack=FALSE, auto.key=list(columns=2),
           main="Attrition by Remote Status", xlab="Remote Status", ylab="Count")
  dev.off()
}

# --- Boxplot: MonthlyIncome vs Attrition ---
png("Plots/MonthlyIncome_vs_Attrition.png", width=800, height=600)
bwplot(MonthlyIncome ~ Attrition, data=ibm,
       main="Monthly Income vs Attrition")
dev.off()

# --- Boxplot: YearsAtCompany vs Attrition ---
png("Plots/YearsAtCompany_vs_Attrition.png", width=800, height=600)
bwplot(YearsAtCompany ~ Attrition, data=ibm,
       main="Years at Company vs Attrition")
dev.off()

# --- Correlation heatmap ---
numeric_vars <- ibm %>% select_if(is.numeric)
corr <- cor(numeric_vars, use="complete.obs")
png("Plots/Correlation_Heatmap.png", width=1200, height=1000)
corrplot(corr, method="color", tl.cex=0.7)
dev.off()

# ------------------------------
# 9. Statistical Analysis
# ------------------------------
if(remote_available){
  chisq.test(table(ibm$Attrition, ibm$Remote_Status))
}

t.test(MonthlyIncome ~ Attrition, data=ibm)

aov_model <- aov(JobSatisfaction ~ Attrition, data=ibm)
summary(aov_model)

ibm$Attrition_num <- ifelse(ibm$Attrition=="Yes",1,0)
cor(numeric_vars, ibm$Attrition_num, use="complete.obs")

# ------------------------------
# 10. Predictive Modeling
# ------------------------------
model_vars <- c("Age", "MonthlyIncome", "JobSatisfaction")
if(remote_available) model_vars <- c(model_vars, "Remote_Status")
if(overtime_available) model_vars <- c(model_vars, "Overtime")

formula_str <- paste("Attrition_num ~", paste(model_vars, collapse=" + "))

# Logistic Regression
log_model <- glm(as.formula(formula_str), data=ibm, family=binomial)
summary(log_model)

# Random Forest
rf_model <- randomForest(as.formula(paste("as.factor(Attrition) ~", paste(model_vars, collapse=" + "))),
                         data=ibm, ntree=500, importance=TRUE)
rf_model
png("Plots/RandomForest_VarImp.png", width=1000, height=800)
varImpPlot(rf_model)
dev.off()

# ------------------------------
# 11. ROC Curve
# ------------------------------
pred_probs <- predict(log_model, type="response")
pred <- prediction(pred_probs, ibm$Attrition_num)
perf <- performance(pred, "tpr", "fpr")
png("Plots/ROC_Curve_Logistic.png", width=800, height=600)
plot(perf, col="blue", main="ROC Curve - Logistic Regression")
abline(a=0, b=1, lty=2, col="red")
dev.off()
auc <- performance(pred, measure="auc")
auc@y.values[[1]]

cat("\n✅ IBM HR Analytics analysis completed successfully. All plots saved in 'Plots/' folder.\n")
