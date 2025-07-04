% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_transformasi.R
\name{plot_transformasi}
\alias{plot_transformasi}
\title{Plot distribusi sebelum dan sesudah transformasi data}
\usage{
plot_transformasi(
  data_asli,
  data_transformasi,
  kolom = NULL,
  maks_kolom = 2,
  metode = NULL,
  jenis_plot = c("histogram", "density", "boxplot", "qq"),
  bin_histogram = 30,
  cetak_statistik = TRUE
)
}
\arguments{
\item{data_asli}{Data frame atau vektor numerik yang berisi data asli.}

\item{data_transformasi}{Data frame atau vektor numerik yang berisi data hasil transformasi.}

\item{kolom}{Vektor karakter dari nama kolom yang akan diplot. Jika NULL (default), plot semua
kolom numerik yang sama antara kedua dataset.}

\item{maks_kolom}{Jumlah maksimal kolom yang ditampilkan per baris dalam plot grid (default 2).}

\item{metode}{String karakter yang menunjukkan metode transformasi yang digunakan (untuk judul plot).}

\item{jenis_plot}{String karakter yang menentukan jenis plot. Pilihan adalah:
\itemize{
\item "histogram" (default): Plot histogram untuk memvisualisasikan distribusi.
\item "density": Plot density untuk menunjukkan distribusi yang dihaluskan.
\item "boxplot": Boxplot untuk menunjukkan sebaran dan outlier.
\item "qq": Plot quantile-quantile untuk membandingkan distribusi dengan distribusi normal.
}}

\item{bin_histogram}{Integer yang menentukan jumlah bin untuk histogram (default 30).}

\item{cetak_statistik}{Logis, apakah akan mencetak statistik ringkasan (default TRUE).}
}
\value{
Objek ggplot2 yang berisi visualisasi untuk penggunaan di Shiny.
}
\description{
Fungsi ini membuat visualisasi distribusi data sebelum dan sesudah menerapkan
berbagai metode transformasi. Memungkinkan perbandingan efek transformasi
pada bentuk dan properti data.
}
\examples{
# Membuat sampel data
data <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(10, 20, 30, 40, 50)
)

# Transformasi data
data_transform <- transformasi_data(data, metode = "log")

# Visualisasi hasil transformasi
plot_transformasi(data, data_transform)


}
\seealso{
\code{\link{transformasi_data}} untuk fungsi transformasi data.
\code{\link{plot_nilai_outlier}} untuk fungsi visualisasi data setelah menangani outlier.
\code{\link{plot_normalisasi}} untuk fungsi visualisasi efek normalisasi.
}
