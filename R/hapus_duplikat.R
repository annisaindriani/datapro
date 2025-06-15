#' Hapus data duplikat dari dataset
#'
#' Fungsi ini akan menghapus baris data duplikat dari dataset dan memberikan informasi
#' tentang jumlah baris yang dihapus.
#'
#' @param data Sebuah data frame atau tibble yang memiliki data duplikat yang akan dihapus
#' @param simpan String karakter yang menentukan kemunculan elemen duplikat mana yang akan disimpan. Pilihan yang tersedia: "first" (default) untuk menyimpan instance pertama
#'        dari duplikat, atau "last" untuk menyimpan instance terakhir.
#' @param verbose Logis, apakah akan mencetak pesan tentang jumlah
#'        baris data duplikat yang dihapus dan dimensi akhir dataset (default TRUE).
#'
#' @return Sebuah data frame dengan baris data duplikat yang telah dihapus.
#'
#' @examples
#' # Membuat sampel data
#' data <- data.frame(
#'   nama = c("John", "Alice", "John", "Bob", "Alice", "Emma"),
#'   umur = c(25, 30, 25, 28, 30, 35),
#'   kota = c("London", "New York", "London", "Paris", "New York", "Tokyo")
#' )
#'
#' data_bersih <- hapus_duplikat(data)
#' data_bersih
#'
#' @seealso
#' \code{\link{deteksi_duplikat}} untuk fungsi mengidentifikasi baris data duplikat dari dataset.
#' \code{\link{hapus_nilai_hilang}} untuk fungsi menghapus nilai yang hilang dalam dataset.
#' \code{\link{hapus_nilai_outlier}} untuk fungsi menghapus outlier dari dataset.
#'
#' @export

hapus_duplikat <- function(data, simpan = "first", verbose = TRUE) {

  # Untuk melihat apakah inputnya sudah berupa data frame atau tibble
  if (!is.data.frame(data)) {
    stop("Input harus berupa data frame atau tibble")
  }

  # Apabila tidak ada data
  if (nrow(data) == 0) {
    cat("Dataset kosong. Tidak ada baris data duplikat yang dihapus.")
    return(data)
  }

  # Untuk menyimpan jumlah baris pada data lama
  baris_sebelum <- nrow(data)

  # Untuk menghapus data duplikat
  data_bersih <- data[!duplicated(data, fromLast = (simpan == "last")), ]

  # Menghitung jumlah baris yang dihapus
  baris_dihapus <- baris_sebelum - nrow(data_bersih)

  # Memberikan pesan tentang hasil penghapusan
  if (verbose) {
    if (baris_dihapus > 0) {
      cat(sprintf("Menghapus %d baris dengan data duplikat dari total %d baris (%.1f%%)\n",
                      baris_dihapus, baris_sebelum, 100 * baris_dihapus / baris_sebelum))
      cat(sprintf("Dimensi akhir: %d baris x %d kolom\n", nrow(data_bersih), ncol(data_bersih)))
    } else {
      cat("Tidak ditemukan baris data duplikat dalam dataset. Tidak ada baris data duplikat yang dihapus.\n")
    }
  }

  return(invisible(data_bersih))
}
