# DataPro

## Description  
DataPro is an R package providing 23 functions to simplify data preprocessing tasks — from data cleaning, transformation, to visualization. In addition to the R package, a web interface is included so users who don't use R can also access and use the functionality. This project was created as my final year project.

The 23 functions support various preprocessing needs, such as:

- Checking category frequency  
- Detecting duplicates  
- Detecting missing values  
- Detecting outliers (using Z‑Score, IQR, or Percentile)  
- Encoding categorical variables (one‑hot, label, ordinal, dummy variables)  
- Removing duplicates  
- Removing missing values  
- Removing outliers  
- Imputing missing values (using mean, median, or mode)  
- Advanced missing‑value imputation (using K‑Nearest Neighbors, MICE, Random Forest, or Hot Deck)  
- Imputing outliers (using mean, median, or mode)  
- Advanced outlier imputation (using Winsorizing, Random Forest, K‑Nearest Neighbors, or IRMI)  
- Inverse normalization of data  
- Inverse transformation of data  
- Data type conversion  
- Data normalization (using Min‑Max scaling, Z‑score standardization, or robust scaling)  
- Plotting outliers  
- Plotting normalization results  
- Plotting transformations  
- Dimensionality reduction (using PCA or LDA, with plots for statistical analysis)  
- Categorical standardization  
- General data transformation (using log, sqrt, Box‑Cox, Yeo‑Johnson, exp, or power transformation)  
- Category recoding  

## Getting Started

### Installation  
To install and use DataPro, clone the repository and install directly from GitHub in R:

```bash
git clone https://github.com/annisaindriani/datapro.git
```

Then in R:
```bash
# install devtools if not installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# install DataPro from local folder
devtools::load_all("path/to/datapro")

# or install from GitHub (if configured)
# devtools::install_github("annisaindriani/datapro")
```

### Usage
After installation, you can load the package in R and start cleaning or transforming your data:
```bash
library(datapro)

# example
df_clean <- hapus_nilai_hilang(data, opsi = "baris")
df_imputed <- imputasi_nilai_hilang(df_clean, metode = "mean")
df_norm <- normalisasi_data(df_imputed, metode = "z-score")
plot_normalisasi(df_imputed, df_norm)
```

### Note
For non‑R users, open the web interface provided below and follow the instructions there to upload and preprocess data via the web UI.

Link: https://annisaindriani.shinyapps.io/dataproshiny/

## Contact
Annisa Indriani - indrianiannisa8@gmail.com - [LinkedIn](https://www.linkedin.com/in/annisaindriani)
