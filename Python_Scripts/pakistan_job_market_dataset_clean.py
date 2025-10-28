import pandas as pd
import os

# === File Paths ===
input_path = r"C:\Users\PMLS\Downloads\Project\Employee Turnover Pakistan\Raw Data\Pakistan Job Market Dataset (Rozee.pk).csv"
output_dir = r"C:\Users\PMLS\Downloads\Project\Employee Turnover Pakistan\Clean Data"
os.makedirs(output_dir, exist_ok=True)  # ✅ Create folder if it doesn’t exist
output_path = os.path.join(output_dir, "Pakistan_Job_Market_Cleaned.csv")

# === Step 1: Load Dataset ===
df = pd.read_csv(input_path, encoding='utf-8')

# === Step 2: Basic Cleanup ===
# Strip extra whitespace from column names
df.columns = df.columns.str.strip()

# Keep only relevant columns
keep_cols = [
    'Title', 'Salary', 'Job Type', 'Job Location',
    'Functional Area', 'Career Level', 'Minimum Experience',
    'Minimum Education', 'Gender', 'Skills'
]
df = df[keep_cols]

# === Step 3: Handle Missing or Irrelevant Data ===
df = df.drop_duplicates()
df = df.dropna(subset=['Title', 'Job Type', 'Job Location'])

# === Step 4: Standardize Job Type ===
df['Job Type'] = df['Job Type'].str.lower().str.strip()

# Create new column for remote classification
df['Remote_Friendly'] = df['Job Type'].apply(
    lambda x: 1 if 'remote' in x or 'hybrid' in x else 0
)

# === Step 5: Clean Salary Column ===
# Remove 'PKR', commas, etc.
df['Salary'] = df['Salary'].astype(str).str.replace('PKR', '', regex=False)
df['Salary'] = df['Salary'].str.replace(',', '').str.extract(r'(\d+)').astype(float)

# === Step 6: Clean Experience Column ===
df['Minimum Experience'] = df['Minimum Experience'].astype(str).str.extract(r'(\d+)').astype(float)

# === Step 7: Clean Education ===
df['Minimum Education'] = df['Minimum Education'].str.strip().replace({
    'Bachelors': 'Bachelor',
    'Masters': 'Master',
    'Intermediate/A-Level': 'Intermediate',
    'Matriculation/O-Level': 'Matriculation'
})

# === Step 8: Drop Irrelevant or Empty Salary Values ===
df = df[df['Salary'].notnull()]

# === Step 9: Save Cleaned Dataset ===
df.to_csv(output_path, index=False, encoding='utf-8')

# === Step 10: Summary Output ===
print("✅ Dataset cleaned and saved successfully!")
print(f"📁 Saved at: {output_path}")
print("\n📊 Summary:")
print(df.info())
print("\nTop 5 rows:\n", df.head())
