#' Standarisasi format nilai kategorikal dalam kolom dataset
#'
#' Fungsi ini akan melakukan standarisasi dengan membersihkan dan menyeragamkan format nilai kategorikal dengan
#' mengubah case huruf, membersihkan spasi berlebih, dan menghapus simbol yang tidak perlu agar data lebih konsisten sebelum analisis.
#'
#' @param data Sebuah data frame atau tibble yang akan distandarisasi nilai kategorinya.
#' @param kolom Nama kolom yang akan distandarisasi (harus berupa character string tunggal).
#' @param format_huruf Format huruf yang diinginkan. Pilihan yang tersedia: "lower" (huruf kecil),
#'   "upper" (huruf besar), "title" (huruf pertama besar), atau "none" (tidak diubah).
#'   Default "lower".
#' @param hapus_spasi Logis, apakah akan membersihkan whitespace berlebih
#'   di awal, akhir, dan tengah (default TRUE).
#' @param hapus_simbol Logis, apakah akan menghapus simbol dan karakter khusus
#'   kecuali huruf, angka, dan spasi (default TRUE).
#' @param verbose Logis, apakah akan mencetak informasi perubahan (default TRUE).
#'
#' @return Data frame dengan kolom yang telah distandarisasi.
#'
#' @examples
#' # Membuat sampel data
#' data <- data.frame(
#'   id = 1:8,
#'   kategori = c("  AKTIF  ", "aktif", "Aktif!!!", "tidak aktif",
#'                "TIDAK_AKTIF", " Non Aktif ", "non-aktif", "  AKTIF  "),
#'   status = c("OK@#", "ok", "OK ", " not ok ", "NOT OK", "ok!!!", "OK", "not_ok"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Melakukan standarisasi dengan format huruf kecil (default)
#' data_bersih <- standarisasi_kategori(data, "kategori")
#'
#' # Melakukan standarisasi untuk menghapus spasi saja
#' data_bersih <- standarisasi_kategori(data, "kategori", format_huruf = "none",
#'                                     hapus_spasi = TRUE, hapus_simbol = FALSE)
#'
#' @seealso
#' \code{\link{cek_frekuensi_kategori}} untuk fungsi analisis frekuensi kategori.
#' \code{\link{konversi_data}} untuk fungsi mengubah tipe data suatu kolom.
#' \code{\link{ubah_kategori}} untuk fungsi mengubah nilai kategorikal dalam suatu kolom.
#' \code{\link{encode_kategori}} untuk fungsi melakukan encoding pada suatu kolom kategorikal.
#'
#' @export

standarisasi_kategori <- function(data,
                                  kolom,
                                  format_huruf = c("lower", "upper", "title", "none"),
                                  hapus_spasi = TRUE,
                                  hapus_simbol = TRUE,
                                  verbose = TRUE) {

  # Untuk melihat apakah inputnya sudah berupa data frame atau tibble
  if (!is.data.frame(data)) {
    stop("Input must be a data frame or tibble.")
  }

  # Untuk validasi parameter kolom
  if (length(kolom) != 1 || !is.character(kolom)) {
    stop("Parameter 'kolom' harus berupa character string tunggal.")
  }

  # Untuk validasi kolom ada dalam data
  if (!kolom %in% names(data)) {
    stop("Kolom '", kolom, "' tidak ditemukan dalam dataset.")
  }

  # Untuk validasi argumen format_huruf yang diberikan user
  format_huruf <- match.arg(format_huruf)

  # Apabila tidak ada data
  if (nrow(data) == 0) {
    message("Dataset kosong. Tidak ada standarisasi yang dilakukan.")
    return(invisible(data))
  }

  # Untuk mengambil data kolom dan menyimpan nilai asli
  col_data <- data[[kolom]]
  original_data <- col_data

  # Untuk validasi kolom berupa character
  if (!is.character(col_data)) {
    col_data <- as.character(col_data)
  }

  # Untuk menghitung statistik awal
  total_rows <- length(col_data)
  non_na_count <- sum(!is.na(col_data))

  # Untuk melakukan standarisasi
  if (non_na_count > 0) {

    # Untuk membersihkan spasi berlebihan jika diminta
    if (hapus_spasi) {
      col_data <- trimws(col_data)
      col_data <- gsub("\\s+", " ", col_data)
    }

    # Untuk menghapus simbol jika diminta
    if (hapus_simbol) {
      col_data <- gsub("[^A-Za-z0-9\\s]", "", col_data)
    }

    # Untuk mengubah format huruf
    if (format_huruf == "lower") {
      col_data <- tolower(col_data)
    } else if (format_huruf == "upper") {
      col_data <- toupper(col_data)
    } else if (format_huruf == "title") {
      col_data <- tools::toTitleCase(tolower(col_data))
    }
  }

  # Untuk menyimpan hasil ke data
  data[[kolom]] <- col_data

  # Untuk menghitung berapa banyak nilai yang berubah
  changed_count <- sum(original_data != col_data, na.rm = TRUE)

  # Untuk print informasi perubahan jika verbose = TRUE
  if (verbose) {
    cat("Melakukan standarisasi kategori pada kolom '", kolom, "'.\n", sep = "")

    processes <- c()
    if (format_huruf != "none") {
      processes <- c(processes, paste("format huruf", format_huruf))
    }
    if (hapus_spasi) {
      processes <- c(processes, "penghapusan spasi berlebih")
    }
    if (hapus_simbol) {
      processes <- c(processes, "penghapusan simbol")
    }

    cat("Total baris yang diubah adalah ", changed_count, " dari ", total_rows,
        " baris (", round(changed_count / total_rows * 100, 2), "%).\n", sep = "")

    if (length(processes) > 0) {
      cat("\nKriteria standarisasi: ", paste(processes, collapse = ", "), ".\n", sep = "")
    }

  }

  return(invisible(data))
}
