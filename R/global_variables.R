# Daftar global variables yang dipakai di seluruh package
utils::globalVariables(c(
  "Component", "Variable", "Value", "Predicted", "Actual", "Freq",
  "PC_num", "ExplainedVar", "PC1", "PC2", "Warna", "PC1_scaled", "PC2_scaled",
  "Iterasi", "KL_Divergence", ".data", "x", "nilai_outlier", "data", "nilai", "tipe",
  "max_unique"
))

.onLoad <- function(libname, pkgname) {
}
