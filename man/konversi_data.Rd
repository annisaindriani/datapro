% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/konversi_data.R
\name{konversi_data}
\alias{konversi_data}
\title{Konversi tipe data kolom dalam dataset}
\usage{
konversi_data(data, kolom, jenis_target, verbose = TRUE)
}
\arguments{
\item{data}{Sebuah data frame atau tibble yang akan dikonversi.}

\item{kolom}{Vektor karakter berisi nama kolom yang akan dikonversi.}

\item{jenis_target}{Tipe data target untuk konversi. Pilihan yang tersedia:
\itemize{
\item "character": Mengkonversi ke karakter.
\item "factor": Mengkonversi ke faktor.
\item "numeric": Mengkonversi ke numerik.
\item "logical": Mengkonversi ke logikal.
\item "date": Mengkonversi ke tanggal.
}}

\item{verbose}{Logis, apakah akan mencetak informasi detail (default TRUE).}
}
\value{
Data frame dengan kolom yang telah dikonversi ke tipe data yang diinginkan.
}
\description{
Fungsi ini akan melakukan konversi tipe data kolom yang dipilih pengguna ke tipe data target.
Fungsi ini dapat digunakan untuk memperbaiki tipe data yang salah dibaca saat import file CSV atau
format lainnya, serta mempersiapkan data untuk analisis.
}
\examples{
# Membuat sampel data
data <- data.frame(
  kategori = c("A", "B", "C", "A", "B"),
  status = c("1", "0", "1", "0", "1"),
  nilai_char = c("10", "20", "30", "40", "50"),
  tanggal = c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05"),
  stringsAsFactors = FALSE
)

# Melihat struktur data
str(data)

# Melakukan konversi kolom kategori ke factor
data_faktor <- konversi_data(data, kolom = "kategori", jenis_target = "factor")
str(data_faktor)

# Melakukan konversi beberapa kolom sekaligus
data_multi <- konversi_data(data, kolom = c("status", "nilai_char"), jenis_target = "numeric")
str(data_multi)

# Melakukan konversi kolom tanggal
data_tanggal <- konversi_data(data, kolom = "tanggal", jenis_target = "date")
str(data_tanggal)

}
\seealso{
\code{\link{cek_frekuensi_kategori}} untuk fungsi analisis frekuensi kategori.
\code{\link{standarisasi_kategori}} untuk fungsi standarisasi format kategori.
\code{\link{encode_kategori}} untuk fungsi melakukan encoding pada suatu kolom kategorikal.
\code{\link{ubah_kategori}} untuk fungsi mengubah nilai kategorikal dalam suatu kolom.
}
