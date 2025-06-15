#' Ubah nilai kategorikal dalam kolom dataset
#'
#' Fungsi ini mengubah nilai tertentu dalam kolom kategorikal menjadi nilai baru
#' yang diinginkan.
#'
#' @param data Sebuh data frame atau tibble yang akan diubah nilai kategorinya.
#' @param kolom Nama kolom yang akan diubah (harus berupa character string tunggal).
#' @param value_awal Nilai asli yang akan diubah. Harus sama persis dengan nilai dalam data.
#' @param value_akhir Nilai baru sebagai pengganti.
#' @param verbose Logical, apakah akan mencetak informasi perubahan (default TRUE).
#'
#' @return Data frame dengan nilai yang telah diubah pada kolom yang ditentukan.
#'
#' @examples
#' # Membuat sampel data
#' data <- data.frame(
#'   id = 1:10,
#'   status = c("aktif", "Aktif", "AKTIF", "tidak aktif", "nonaktif",
#'              "aktif", "Tidak Aktif", "aktif", "TIDAK AKTIF", "aktif"),
#'   kategori = c("A", "B", "C", "A", "B", "A", "C", "B", "A", "C"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Ubah "aktif" menjadi "Aktif"
#' data_baru <- ubah_kategori(data, "status", "aktif", "Aktif")
#'
#' # Ubah kategori A menjadi Kategori_1
#' data_baru <- ubah_kategori(data, "kategori", "A", "Kategori_1")
#'
#' @seealso
#' \code{\link{cek_frekuensi_kategori}} untuk fungsi analisis frekuensi kategori.
#' \code{\link{konversi_data}} untuk fungsi mengubah tipe data suatu kolom.
#' \code{\link{standarisasi_kategori}} untuk fungsi standarisasi format kategori.
#' \code{\link{encode_kategori}} untuk fungsi melakukan encoding pada suatu kolom kategorikal.
#'
#' @export

ubah_kategori <- function(data,
                          kolom,
                          value_awal,
                          value_akhir,
                          verbose = TRUE) {

  # Untuk melihat apakah inputnya sudah berupa data frame atau tibble
  if (!is.data.frame(data)) {
    stop("Input harus berupa data frame atau tibble.")
  }

  # Untuk validasi parameter kolom
  if (length(kolom) != 1 || !is.character(kolom)) {
    stop("Parameter 'kolom' harus berupa character string tunggal.")
  }

  # Untuk validasi bahwa kolom ada dalam data
  if (!kolom %in% names(data)) {
    stop("Kolom '", kolom, "' tidak ditemukan dalam dataset.")
  }

  # Apabila tidak ada data
  if (nrow(data) == 0) {
    message("Dataset kosong. Tidak ada perubahan yang dilakukan.")
    return(invisible(data))
  }

  # Untuk mengambil data kolom
  col_data <- data[[kolom]]

  # Untuk menyimpan informasi perubahan
  total_rows <- length(col_data)

  # Untuk menentukan indeks yang akan diubah
  matches <- which(col_data == value_awal)
  original_matches <- length(matches)

  # Apabila tidak ada nilai yang cocok
  if (original_matches == 0) {
    if (verbose) {
      cat("Tidak ada nilai '", as.character(value_awal), "' yang ditemukan pada kolom '", kolom, "'.\n", sep = "")
      cat("Tidak ada perubahan yang dilakukan.\n")
    }
    return(invisible(data))
  }

  # Untuk melakukan perubahan
  data[[kolom]][matches] <- value_akhir

  # Untuk print informasi perubahan jika verbose = TRUE
  if (verbose) {
    cat("Mengubah ", original_matches, " nilai '", as.character(value_awal),
        "' pada kolom '", kolom, "' menjadi '", as.character(value_akhir), "'.\n", sep = "")

    # Untuk memberikan informasi tambahan tentang persentase
    percentage <- round(original_matches / total_rows * 100, 2)
    cat("Total baris yang diubah adalah ", original_matches, " dari ", total_rows,
        " baris (", percentage, "%).\n", sep = "")
  }

  return(invisible(data))
}
