% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imputasi_nilai_hilang.R
\name{imputasi_nilai_hilang}
\alias{imputasi_nilai_hilang}
\title{Imputasi nilai yang hilang dalam sebuah dataset}
\usage{
imputasi_nilai_hilang(
  data,
  metode = c("mean", "median", "modus"),
  kolom = NULL,
  round = FALSE,
  digit = 0,
  numerik = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{data}{Sebuah data frame atau tibble dengan nilai yang hilang untuk diimputasi}

\item{metode}{Metode yang digunakan untuk imputasi. Pilihan yang tersedia:
\itemize{
\item "mean": Mengganti nilai yang hilang dengan rata-rata kolom.
\item "median": Mengganti nilai yang hilang dengan median kolom.
\item "modus": Mengganti nilai yang hilang dengan modus kolom.
}}

\item{kolom}{Vektor karakter dari nama kolom yang akan diimputasi. Jika NULL (default),
semua kolom dengan nilai yang hilang akan diimputasi.}

\item{round}{Logis, apakah akan membulatkan nilai numerik yang diimputasi (default FALSE)}

\item{digit}{Integer yang menunjukkan jumlah tempat desimal untuk pembulatan ketika
bulatkan = TRUE (default 0)}

\item{numerik}{Logis, apakah hanya mengimputasi kolom numerik (default FALSE)}

\item{verbose}{Logis, apakah akan mencetak informasi tentang nilai yang diimputasi (default TRUE)}
}
\value{
Sebuah data frame dengan nilai yang hilang diimputasi sesuai dengan metode yang ditentukan.
Jika tidak ditemukan nilai yang hilang, akan mengembalikan pesan.
}
\description{
Fungsi ini mengganti nilai yang hilang dalam dataset menggunakan salah satu dari beberapa metode:
mean, median, atau modus.
}
\examples{
# Membuat sampel data
data <- data.frame(
  id = 1:5,
  umur = c(25, 30, NA, NA, 28),
  skor = c(NA, 85.5, 90.2, 78.7, 92.1),
  kategori = c("A", "B", NA, "B", "A")
)

# Imputasi semua kolom menggunakan mean
df_impute_all <- imputasi_nilai_hilang(data, metode = "mean")
df_impute_all

# Imputasi hanya kolom 'umur' menggunakan mean dan bulatkan ke 0 tempat desimal
df_impute_umur <- imputasi_nilai_hilang(data, metode = "mean", kolom = "umur", round = TRUE)
df_impute_umur

# Imputasi kolom 'skor' menggunakan median, pertahankan tempat desimal
df_impute_skor <- imputasi_nilai_hilang(data, metode = "median", kolom = "skor")

# Imputasi hanya data kategorikal menggunakan modus
df_impute_kategori <- imputasi_nilai_hilang(data, metode = "modus", kolom = "kategori")
df_impute_kategori

# Imputasi beberapa kolom menggunakan mean
df_impute <- imputasi_nilai_hilang(data, metode = "mean",
                                  kolom = c("umur", "skor"), round = TRUE)
df_impute

}
\seealso{
\code{\link{imputasi_nilai_hilang_lanjutan}} untuk fungsi imputasi nilai yang hilang dalam dataset menggunakan metode lanjutan.
\code{\link{deteksi_nilai_hilang}} untuk fungsi identifikasi baris dengan nilai yang hilang dari dataset.
\code{\link{hapus_nilai_hilang}} untuk fungsi menghapus nilai yang hilang dalam dataset.
}
