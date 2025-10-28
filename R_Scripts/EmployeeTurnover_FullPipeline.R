# ===============================================================
# Employee Turnover Analysis in Remote Work Era (Pakistan Hybrid)
# Full R Script: Descriptive, Visual, Statistical, Predictive, Integration
# ===============================================================

# -----------------------------
# 0. Load required packages
# -----------------------------
install.packages(c("tidyverse", "ggplot2", "dplyr", "readr", "corrplot",
                   "caret", "randomForest", "ggthemes", "reshape2",
                   "ggpubr", "ROCR", "scales"))

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

# -----------------------------
# 1. Load datasets
# -----------------------------
ibm <- read_csv("C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/IBM HR Cleaned.csv")
rozee <- read_csv("C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/Pakistan Job Market Cleaned.csv")
employment <- read_csv("C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/Pakistan Employment Cleaned.csv")

# -----------------------------
# 2. IBM HR Analytics: Descriptive & EDA
# -----------------------------
# Descriptive
summary(ibm)
table(ibm$Attrition)
prop.table(table(ibm$Attrition))

# Attrition by Department, Gender, Job Role, Education
ibm %>% group_by(Department) %>% summarise(AttritionRate = mean(Attrition=="Yes"))
ibm %>% group_by(JobRole) %>% summarise(AttritionRate = mean(Attrition=="Yes"))
ibm %>% group_by(Gender) %>% summarise(AttritionRate = mean(Attrition=="Yes"))
ibm %>% group_by(EducationField) %>% summarise(AttritionRate = mean(Attrition=="Yes"))

# Numeric summary
ibm %>% summarise(AvgSalary = mean(MonthlyIncome),
                  AvgSatisfaction = mean(JobSatisfaction),
                  AvgAge = mean(Age),
                  AvgYearsAtCompany = mean(YearsAtCompany))

# Visualization: Bar charts
ggplot(ibm, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "fill") + theme_minimal() +
  labs(y = "Proportion", title = "Attrition by Department")

ggplot(ibm, aes(x = JobRole, fill = Attrition)) +
  geom_bar(position = "fill") + theme_minimal() +
  coord_flip() + labs(title = "Attrition by Job Role")

ggplot(ibm, aes(x = Remote_Status, fill = Attrition)) +
  geom_bar(position = "fill") + theme_minimal() +
  labs(y = "Proportion", title = "Attrition by Remote Status")

# Boxplots
ggplot(ibm, aes(x = Attrition, y = MonthlyIncome)) + geom_boxplot() + theme_classic()
ggplot(ibm, aes(x = Attrition, y = YearsAtCompany)) + geom_boxplot() + theme_classic()

# Heatmap: correlation matrix
num_vars <- ibm %>% select_if(is.numeric)
corr <- cor(num_vars, use="complete.obs")
corrplot(corr, method="color", tl.cex=0.6)

# Statistical tests
chisq.test(table(ibm$Attrition, ibm$Remote_Status))
t.test(MonthlyIncome ~ Attrition, data=ibm)

# -----------------------------
# 3. Predictive Modeling: IBM
# -----------------------------
# Prepare data
ibm <- ibm %>% mutate(AttritionFlag = ifelse(Attrition=="Yes",1,0))
set.seed(123)
train_index <- createDataPartition(ibm$AttritionFlag, p=0.8, list=FALSE)
train <- ibm[train_index,]
test <- ibm[-train_index,]

# Logistic Regression
log_model <- glm(AttritionFlag ~ Age + MonthlyIncome + JobSatisfaction + Overtime + Remote_Status,
                 family=binomial, data=train)
summary(log_model)

# Random Forest
rf_model <- randomForest(as.factor(AttritionFlag) ~ Age + MonthlyIncome + JobSatisfaction + Overtime + Remote_Status,
                         data=train, ntree=500)
varImpPlot(rf_model)

# Model evaluation: ROC
pred <- predict(rf_model, test, type="prob")[,2]
pred_obj <- prediction(pred, test$AttritionFlag)
perf <- performance(pred_obj, "tpr", "fpr")
plot(perf, col="blue", main="ROC Curve - Random Forest")
abline(a=0, b=1, lty=2, col="red")

# -----------------------------
# 4. Pakistan Job Market (Rozee.pk)
# -----------------------------
# Descriptive
summary(rozee)
table(rozee$Job_Type)
rozee %>% group_by(Functional_Area) %>% summarise(RemotePercent = mean(Remote_Capable))

# Visuals
ggplot(rozee, aes(x = Functional_Area, fill = Job_Type)) +
  geom_bar(position="dodge") + theme_minimal() + coord_flip() +
  labs(title="Remote vs On-site Jobs by Industry")

ggplot(rozee, aes(x = Job_Type, y = Avg_Salary)) +
  geom_boxplot() + theme_minimal() + labs(title="Salary by Job Type")

# Statistical summary
rozee %>% group_by(Job_Type) %>%
  summarise(AvgSalary=mean(Avg_Salary, na.rm=TRUE),
            Count=n())

# -----------------------------
# 5. Pakistan Employment Dataset
# -----------------------------
summary(employment)
employment %>% group_by(Province) %>%
  summarise(EmploymentRate=mean(EmploymentRate, na.rm=TRUE))

# Visual: Employment by Province
ggplot(employment, aes(x = Province, y = EmploymentRate, fill=Gender)) +
  geom_bar(stat="identity", position="dodge") + theme_minimal() +
  labs(title="Employment Rate by Province and Gender")

# -----------------------------
# 6. Integration: Derived Indicators
# -----------------------------
# IBM attrition summary by Department
ibm_summary <- ibm %>%
  group_by(Department) %>%
  summarise(AttritionRate = mean(AttritionFlag))

# Rozee: Remote job % by Functional Area
rozee_summary <- rozee %>%
  group_by(Role_Group) %>%
  summarise(RemotePercent = mean(Remote_Capable))

# Employment: Sector employment share
employment_summary <- employment %>%
  group_by(Sector) %>%
  summarise(EmploymentShare = mean(EmploymentRate, na.rm=TRUE))

# Merge summaries
combined <- merge(ibm_summary, rozee_summary, by.x="Department", by.y="Role_Group", all=TRUE)
combined <- merge(combined, employment_summary, by.x="Department", by.y="Sector", all=TRUE)

# Derived metrics
combined <- combined %>%
  mutate(RemoteWorkPotential = RemotePercent * EmploymentShare,
         AttritionRisk = AttritionRate / (RemoteWorkPotential + 0.01),  # avoid division by zero
         RetentionReadiness = 100 - (AttritionRisk*100))

# Final visuals
ggplot(combined, aes(x=Department, y=RetentionReadiness, fill=AttritionRisk)) +
  geom_col() + coord_flip() + theme_minimal() +
  labs(title="Retention Readiness vs Attrition Risk by Department")

# Heatmap of metrics
num_comb <- combined %>% select(AttritionRate, RemotePercent, EmploymentShare, AttritionRisk)
corrplot(cor(num_comb, use="complete.obs"), method="number")

# -----------------------------
# 7. Export final combined metrics for Power BI
# -----------------------------
write_csv(combined, "C:/Users/PMLS/Downloads/Project/Employee Turnover Pakistan/Cleaned/final_combined_metrics.csv")
print("✅ Final combined metrics exported for Power BI!")
