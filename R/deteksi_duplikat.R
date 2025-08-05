#' Deteksi data duplikat dalam sebuah dataset
#'
#' Fungsi ini mengidentifikasi baris-baris di mana semua nilai sama di seluruh kolom kemudian
#' mengembalikan baris-baris tersebut beserta jumlah kemunculannya.
#'
#' @param data Sebuah data frame atau tibble yang ingin dideteksi apakah terdapat baris duplikat.
#'
#' @return Sebuah data frame yang menampilkan baris unik yang memiliki duplikat,
#'   dengan kolom tambahan yang menunjukkan jumlah masing-masing baris duplikat.
#'   Jika tidak ditemukan duplikat, maka fungsi akan mengembalikan pesan.
#'
#' @examples
#' # Membuat sampel data
#' data <- data.frame(
#'   nama = c("John", "Alice", "John", "Bob", "Alice", "Emma"),
#'   umur = c(25, 30, 25, 28, 30, 35),
#'   kota = c("London", "New York", "London", "Paris", "New York", "Tokyo")
#' )
#'
#' deteksi_duplikat(data)
#'
#' @seealso
#' \code{\link{hapus_duplikat}} untuk fungsi menghapus baris duplikat dari dataset.
#' \code{\link{deteksi_nilai_hilang}} untuk fungsi mengidentifikasi baris yang memiliki nilai yang hilang.
#' \code{\link{deteksi_nilai_outlier}} untuk fungsi mengidentifikasi nilai outlier dalam dataset.
#'
#' @importFrom utils head
#'
#' @export

deteksi_duplikat <- function(data) {

  # Untuk melihat apakah inputnya sudah berupa data frame atau tibble
  if (!is.data.frame(data)) {
    stop("Input harus berupa data frame atau tibble.")
  }

  # Apabila tidak ada data
  if (nrow(data) == 0) {
    cat("Dataset kosong. Tidak ada baris duplikat yang dapat dideteksi.")
    return(invisible(NULL))
  }

  # Untuk mengidentifikasi baris yang merupakan duplikat (output akan berupa TRUE atau FALSE)
  baris_duplikat <- duplicated(data) | duplicated(data, fromLast = TRUE)

  # Apabila tidak ada baris duplikat
  if (!any(baris_duplikat)) {
    cat("Tidak ditemukan baris data duplikat dalam dataset.")
    return(invisible(NULL))
  }

  # Untuk mengambil semua baris yang merupakan duplikat
  data_duplikat <- data[baris_duplikat, , drop = FALSE]

  # Untuk mengambil hanya baris unik dari data duplikat
  baris_unik <- unique(data_duplikat)
  hasil <- baris_unik

  # Untuk menghitung jumlah kemunculan tiap baris unik dalam dataset asli
  hasil$Jumlah <- sapply(1:nrow(baris_unik), function(i) {
    baris_dicari <- baris_unik[i, , drop = FALSE]
    cocok <- sapply(1:nrow(data), function(j) {
      all(data[j, ] == baris_dicari[1, ])
    })
    sum(cocok)
  })

  hasil <- as.data.frame(hasil)

  # Untuk menghitung total baris duplikat
  total_duplikat <- sum(baris_duplikat)

  # Untuk print total baris duplikat beserta datanya
  cat("Jumlah baris dengan data duplikat:", total_duplikat, "dari", nrow(data),
      sprintf("(%.2f%%)\n\n", 100 * total_duplikat / nrow(data)))

  if (total_duplikat > 0) {
    cat("Baris dengan data duplikat")
    if (total_duplikat > 5) {
      cat(sprintf(" (menampilkan 5 baris pertama dari %d):\n", total_duplikat))
      print(head(hasil, 5))
    } else {
      cat(":\n")
      print(hasil)
    }
  }

  return(invisible(hasil))
}
