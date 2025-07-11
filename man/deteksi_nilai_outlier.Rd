% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deteksi_nilai_outlier.R
\name{deteksi_nilai_outlier}
\alias{deteksi_nilai_outlier}
\title{Deteksi nilai outlier dalam sebuah dataset}
\usage{
deteksi_nilai_outlier(
  data,
  metode = c("z-score", "iqr", "percentile"),
  ambang_batas = NULL,
  persentil_bawah = 0.05,
  persentil_atas = 0.95
)
}
\arguments{
\item{data}{Vektor numerik dari nilai-nilai.}

\item{metode}{Metode yang digunakan untuk deteksi nilai outlier. Pilihan yang tersedia:
\itemize{
\item: "z-score": Menggunakan standar deviasi dari mean.
\item "iqr": Menggunakan rentang IQR.
\item "percentile": Menggunakan ambang batas persentil khusus.
}}

\item{ambang_batas}{Untuk metode Z-Score, nilai ambang batas (default adalah 3).
Untuk metode IQR, pengali untuk IQR (default adalah 1.5).}

\item{persentil_bawah}{Ambang batas bawah untuk metode percentile (default adalah 0.05).}

\item{persentil_atas}{Ambang batas atas untuk metode percentile (default adalah 0.95).}
}
\value{
Sebuah list yang berisi:
\item{data}{Data frame dengan nilai asli dan status nilai outlier mereka.}
\item{nilai_outlier}{Data frame dengan indeks dan nilai dari nilai outlier.}
\item{jumlah_nilai_outlier}{Jumlah nilai outlier yang terdeteksi.}
}
\description{
Fungsi ini mengidentifikasi nilai outlier dalam vektor numerik menggunakan metode Z-Score,
IQR (Interquartile Range), atau Percentile. Fungsi ini akan menampilkan ringkasan dari
nilai outlier yang terdeteksi dan mengembalikan informasi secara detail.
}
\examples{
# Membuat sampel data
data <- data.frame(
  ID = c(10, 11, 12, 12, 13, 14, 15),
  Nama = c("Alice", "Bob", "Charlie", "Charlie", "Diana", "Evan", "Frank"),
  Umur = c(22, 28, 33, 33, 38, 42, 85)  # 85 bisa jadi nilai outlier
)

# Deteksi nilai outlier menggunakan metode z-score
data_outlier <- deteksi_nilai_outlier(data$Umur, metode = "z-score")

# Deteksi nilai outlier menggunakan metode 1.5 IQR
data_outlier <- deteksi_nilai_outlier(data$Umur, metode = "iqr")

# Deteksi nilai outlier menggunakan metode percentile
data_outlier <- deteksi_nilai_outlier(data$Umur, metode = "percentile")

}
\seealso{
\code{\link{hapus_nilai_outlier}} untuk fungsi menghapus nilai outlier dari dataset.
\code{\link{imputasi_nilai_outlier}} untuk fungsi imputasi nilai outlier dalam dataset menggunakan metode dasar.
\code{\link{imputasi_nilai_outlier_lanjutan}} untuk fungsi imputasi nilai outlier dalam dataset menggunakan metode lanjutan.
}
