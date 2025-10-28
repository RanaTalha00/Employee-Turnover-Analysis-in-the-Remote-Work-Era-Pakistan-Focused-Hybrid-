import pandas as pd
import os
import glob

# ✅ Step 1: Define folders
raw_folder = r"C:\Users\PMLS\Downloads\Project\Employee Turnover Pakistan\Raw Data"
clean_folder = r"C:\Users\PMLS\Downloads\Project\Employee Turnover Pakistan\Clean Data"

# ✅ Step 2: Auto-detect CSV file in the Raw Data folder
csv_files = glob.glob(os.path.join(raw_folder, "*.csv"))
if not csv_files:
    raise FileNotFoundError("❌ No CSV files found in the Raw Data folder. Please check the filename.")
else:
    path = csv_files[0]
    print(f"✅ Using file: {path}")

# ✅ Step 3: Create Clean Data folder if it doesn’t exist
os.makedirs(clean_folder, exist_ok=True)

# ✅ Step 4: Load dataset
df = pd.read_csv(path)

# ✅ Step 5: Drop irrelevant or constant columns
drop_cols = ["EmployeeCount", "StandardHours", "Over18", "EmployeeNumber"]
df = df.drop(columns=drop_cols, errors='ignore')

# ✅ Step 6: Clean & simplify column names (optional)
df.columns = df.columns.str.strip().str.replace(" ", "_")

# ✅ Step 7: Handle missing values (if any)
df = df.dropna(how='all')  # remove empty rows

# ✅ Step 8: Convert categorical columns to consistent format
categorical_cols = df.select_dtypes(include=['object']).columns
for col in categorical_cols:
    df[col] = df[col].str.strip().str.title()

# ✅ Step 9: Save cleaned file
clean_path = os.path.join(clean_folder, "ibm_hr_cleaned.csv")
df.to_csv(clean_path, index=False)

print(f"\n🎯 Cleaned dataset saved successfully to:\n{clean_path}")
print("\n✅ Preview of cleaned data:")
print(df.head())
