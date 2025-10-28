import pandas as pd
import os

# File path
path = r"C:\Users\PMLS\Downloads\Project\Employee Turnover Pakistan\Raw Data"
file = "Pakistan Employment Dataset 2023.csv"   # ✅ fix applied here

df = pd.read_csv(os.path.join(path, file))

# ---- Basic cleaning ----
df.columns = df.columns.str.strip().str.lower().str.replace(" ", "_")

# Keep only employment-related indicators
employment_df = df[df['indicator'].str.contains('employ', case=False, na=False)].copy()

# Drop rows with missing total values
employment_df = employment_df.dropna(subset=['total'])

# --- Safe numeric conversion ---
def safe_to_float(x):
    """Convert numeric strings safely; return NaN for non-numeric symbols"""
    try:
        x = str(x).replace(',', '').strip()
        if x in ['', '-', '–', '—', 'n/a', 'N/A', 'None']:
            return pd.NA
        return float(x)
    except Exception:
        return pd.NA

for col in ['total', 'male', 'female']:
    employment_df[col] = employment_df[col].apply(safe_to_float)

# Drop rows where total is still missing or invalid
employment_df = employment_df.dropna(subset=['total'])

# Compute shares
employment_df['female_share'] = employment_df['female'] / employment_df['total']
employment_df['male_share'] = employment_df['male'] / employment_df['total']

# Aggregate to province x area_type level
prov_summary = (
    employment_df.groupby(['province', 'area_type'])
    .agg({'total': 'sum', 'male': 'sum', 'female': 'sum'})
    .reset_index()
)
prov_summary['female_share'] = prov_summary['female'] / prov_summary['total']
prov_summary['male_share'] = prov_summary['male'] / prov_summary['total']

# Export cleaned dataset
clean_path = os.path.join(path, "Pakistan_Employment_Clean.csv")
prov_summary.to_csv(clean_path, index=False)

print("✅ Cleaned dataset saved to:", clean_path)
print(prov_summary.head())
