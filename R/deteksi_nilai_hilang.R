#' Deteksi nilai yang hilang dalam sebuah dataset
#'
#' Fungsi ini mengidentifikasi nilai yang hilang dalam sebuah dataset dan memberikan informasi
#' terkait total baris yang memiliki nilai yang hilang, total baris yang memiliki nilai yang hilang
#' berdasarkan kolom, serta mengidentifikasi baris tertentu yang mengandung nilai yang hilang.
#'
#' @param data Sebuah data frame atau tibble yang akan dianalisis untuk nilai yang hilang.
#' @param print_rows Logikal, apakah baris yang mengandung nilai yang hilang akan dicetak (default TRUE).
#' @param max_rows Jumlah maksimum baris yang mengandung nilai yang hilang untuk ditampilkan (default 10).
#'
#' @return Sebuah list yang berisi tiga elemen:
#'   \item{total_baris_dengan_na}{Jumlah total baris yang mengandung nilai yang hilang.}
#'   \item{na_berdasarkan_kolom}{Vektor bernama yang berisi jumlah nilai yang hilang di setiap kolom.}
#'   \item{baris_dengan_na}{Sebuah data frame yang berisi baris-baris dengan nilai yang hilang.}
#'   Apabila tidak ditemukan nilai yang hilang, akan mengembalikan pesan.
#'
#' @examples
#' # Membuat sampel data
#' data <- data.frame(
#'    x = c(1, 2, NA, 4),
#'    y = c("a", NA, "c", NA),
#'    z = c(TRUE, FALSE, TRUE, FALSE)
#' )
#'
#' deteksi_nilai_hilang(data, print_rows = FALSE)
#' df_bersih <- deteksi_nilai_hilang(data, max_rows = 5)
#'
#' @seealso
#' \code{\link{hapus_nilai_hilang}} untuk fungsi menghapus baris dengan nilai yang hilang dari dataset.
#' \code{\link{imputasi_nilai_hilang}} untuk fungsi imputasi nilai yang hilang dalam dataset menggunakan metode dasar.
#' \code{\link{imputasi_nilai_hilang_lanjutan}} untuk fungsi imputasi nilai yang hilang dalam dataset menggunakan metode lanjutan.
#' \code{\link{deteksi_nilai_outlier}} untuk fungsi mengidentifikasi nilai outlier dalam dataset.
#' \code{\link{deteksi_duplikat}} untuk fungsi mengidentifikasi baris data duplikat dalam dataset.
#'
#' @importFrom utils head
#'
#' @export

deteksi_nilai_hilang <- function(data, print_rows = TRUE, max_rows = 5) {

  # Apabila dataset kosong
  if (nrow(data) == 0) {
    cat("Dataset kosong. Tidak ditemukan baris dengan nilai yang hilang.\n")
    return(invisible(NULL))
  }

  # Fungsi untuk deteksi nilai hilang (NA, NULL, dan string kosong)
  is_missing <- function(x) {
    is.na(x) | is.null(x) | (is.character(x) & x == "")
  }

  # Cek apakah ada nilai yang hilang (termasuk string kosong)
  has_missing <- any(apply(data, 2, function(col) any(is_missing(col))))

  # Apabila tidak ada nilai yang hilang
  if (!has_missing) {
    cat("Tidak ditemukan baris atau kolom dengan nilai yang hilang dalam dataset.\n")
    return(invisible(NULL))
  }

  # Menghitung jumlah baris dengan nilai yang hilang
  baris_dengan_na <- which(apply(data, 1, function(row) any(is_missing(row))))
  total_baris_dengan_na <- length(baris_dengan_na)

  # Menghitung jumlah baris dengan nilai yang hilang pada setiap kolom
  na_berdasarkan_kolom <- sapply(data, function(col) sum(is_missing(col)))

  # Mengambil baris unik yang mengandung nilai yang hilang
  data_baris_dengan_na <- data[baris_dengan_na, , drop = FALSE]

  # Mencetak hasil
  cat("Jumlah baris unik dengan nilai yang hilang:", total_baris_dengan_na, "dari", nrow(data),
      sprintf("(%.2f%%)\n\n", 100 * total_baris_dengan_na / nrow(data)))

  na_df <- data.frame(
    Kolom = names(na_berdasarkan_kolom),
    Hilang = unname(na_berdasarkan_kolom),
    Persentase = sprintf("%.2f%%", 100 * na_berdasarkan_kolom / nrow(data))
  )
  print(na_df, row.names = FALSE)
  cat("\n")

  if (print_rows && total_baris_dengan_na > 0) {
    cat("Baris unik dengan nilai yang hilang")
    if (total_baris_dengan_na > max_rows) {
      cat(sprintf(" (menampilkan %d baris pertama dari %d):\n", max_rows, total_baris_dengan_na))
      print(head(data_baris_dengan_na, max_rows))
    } else {
      cat(":\n")
      print(data_baris_dengan_na)
    }
  }

  # Mengembalikan hasil
  return(invisible(list(
    total_baris_dengan_na = total_baris_dengan_na,
    na_berdasarkan_kolom = na_berdasarkan_kolom,
    baris_dengan_na = data_baris_dengan_na
  )))
}
