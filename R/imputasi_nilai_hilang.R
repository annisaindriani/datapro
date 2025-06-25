#' Imputasi nilai yang hilang dalam sebuah dataset
#'
#' Fungsi ini mengganti nilai yang hilang dalam dataset menggunakan salah satu dari beberapa metode:
#' mean, median, atau modus.
#'
#' @param data Sebuah data frame atau tibble dengan nilai yang hilang untuk diimputasi
#' @param metode Metode yang digunakan untuk imputasi. Pilihan yang tersedia:
#'   \itemize{
#'      \item "mean": Mengganti nilai yang hilang dengan rata-rata kolom.
#'      \item "median": Mengganti nilai yang hilang dengan median kolom.
#'      \item "modus": Mengganti nilai yang hilang dengan modus kolom.
#'   }
#' @param kolom Vektor karakter dari nama kolom yang akan diimputasi. Jika NULL (default),
#'   semua kolom dengan nilai yang hilang akan diimputasi.
#' @param round Logis, apakah akan membulatkan nilai numerik yang diimputasi (default FALSE)
#' @param digit Integer yang menunjukkan jumlah tempat desimal untuk pembulatan ketika
#'   bulatkan = TRUE (default 0)
#' @param numerik Logis, apakah hanya mengimputasi kolom numerik (default FALSE)
#' @param verbose Logis, apakah akan mencetak informasi tentang nilai yang diimputasi (default TRUE)
#'
#' @return Sebuah data frame dengan nilai yang hilang diimputasi sesuai dengan metode yang ditentukan.
#'   Jika tidak ditemukan nilai yang hilang, akan mengembalikan pesan.
#'
#' @examples
#' # Membuat sampel data
#' data <- data.frame(
#'   id = 1:5,
#'   umur = c(25, 30, NA, NA, 28),
#'   skor = c(NA, 85.5, 90.2, 78.7, 92.1),
#'   kategori = c("A", "B", NA, "B", "A")
#' )
#'
#' # Imputasi semua kolom menggunakan mean
#' df_impute_all <- imputasi_nilai_hilang(data, metode = "mean")
#' df_impute_all
#'
#' # Imputasi hanya kolom 'umur' menggunakan mean dan bulatkan ke 0 tempat desimal
#' df_impute_umur <- imputasi_nilai_hilang(data, metode = "mean", kolom = "umur", round = TRUE)
#' df_impute_umur
#'
#' # Imputasi kolom 'skor' menggunakan median, pertahankan tempat desimal
#' df_impute_skor <- imputasi_nilai_hilang(data, metode = "median", kolom = "skor")
#'
#' # Imputasi hanya data kategorikal menggunakan modus
#' df_impute_kategori <- imputasi_nilai_hilang(data, metode = "modus", kolom = "kategori")
#' df_impute_kategori
#'
#' # Imputasi beberapa kolom menggunakan mean
#' df_impute <- imputasi_nilai_hilang(data, metode = "mean",
#'                                   kolom = c("umur", "skor"), round = TRUE)
#' df_impute
#'
#' @seealso
#' \code{\link{imputasi_nilai_hilang_lanjutan}} untuk fungsi imputasi nilai yang hilang dalam dataset menggunakan metode lanjutan.
#' \code{\link{deteksi_nilai_hilang}} untuk fungsi identifikasi baris dengan nilai yang hilang dari dataset.
#' \code{\link{hapus_nilai_hilang}} untuk fungsi menghapus nilai yang hilang dalam dataset.
#'
#' @importFrom stats median
#'
#' @export

imputasi_nilai_hilang <- function(data, metode = c("mean", "median", "modus"),
                                  kolom = NULL, round = FALSE, digit = 0,
                                  numerik = FALSE, verbose = TRUE) {

  # Untuk melihat apakah inputnya sudah berupa data frame atau tibble
  if (!is.data.frame(data)) {
    stop("Input harus berupa data frame atau tibble.")
  }

  # Untuk memvalidasi dan mencocokkan argumen metode yang diberikan user sesuai dengan pilihan yang ada
  metode <- match.arg(metode)

  # Apabila tidak ada data
  if (nrow(data) == 0) {
    cat("Dataset kosong. Tidak ada imputasi yang dilakukan.")
    return(invisible(data))
  }

  # Fungsi untuk deteksi nilai hilang (NA, NULL, dan string kosong)
  is_missing <- function(x) {
    is.na(x) | is.null(x) | (is.character(x) & x == "")
  }

  # Cek apakah ada nilai yang hilang (termasuk string kosong)
  has_missing <- any(apply(data, 2, function(col) any(is_missing(col))))

  # Apabila tidak ada nilai yang hilang sama sekali
  if (!has_missing) {
    cat("Tidak ditemukan baris atau kolom dengan nilai yang hilang dalam dataset. Tidak ada imputasi yang dilakukan.")
    return(invisible(data))
  }

  # Untuk menyimpan data lama pada variabel baru
  hasil <- data

  # Untuk melakukan validasi pada parameter kolom
  if (!is.null(kolom)) {
    kolom_tidak_valid <- setdiff(kolom, names(data))
    if (length(kolom_tidak_valid) > 0) {
      stop("Kolom berikut tidak ada dalam dataset: ",
           paste(kolom_tidak_valid, collapse = ", "))
    }
    kolom_diproses <- kolom
  } else {
    kolom_diproses <- names(data)
  }

  # Untuk menghitung modus
  hitung_modus <- function(x) {
    x_tanpa_na <- x[!is_missing(x)]

    if (length(x_tanpa_na) == 0) {
      return(NA)  # Return NA jika semua value bernilai NA
    }

    # Untuk menghitung jumlah dari unique value
    nilai_unik <- unique(x_tanpa_na)
    jumlah <- tabulate(match(x_tanpa_na, nilai_unik))

    # Untuk mengembalikan nilai modus berdasarkan unique value
    return(nilai_unik[which.max(jumlah)])
  }

  info_imputasi <- list()

  for (nama_kolom in kolom_diproses) {
    kolom_data <- hasil[[nama_kolom]]

    # Skip jika kolom tidak memiliki nilai yang hilang
    if (!any(is_missing(kolom_data))) {
      next
    }

    # Skip kolom non-numerik jika parameter numerik is TRUE
    if (numerik && !is.numeric(kolom_data)) {
      if (verbose) {
        cat(sprintf("Melewati kolom non-numerik '%s'\n", nama_kolom))
      }
      next
    }

    # Untuk menghitung jumlah baris dengan nilai yang hilang
    jumlah_na <- sum(is_missing(kolom_data))

    # Untuk memproses kolom numerik
    if (is.numeric(kolom_data)) {
      # Hitung dengan nilai non-missing
      nilai_bersih <- kolom_data[!is_missing(kolom_data)]

      if (metode == "mean") {
        nilai_imputasi <- mean(nilai_bersih, na.rm = TRUE)
      } else if (metode == "median") {
        nilai_imputasi <- stats::median(nilai_bersih, na.rm = TRUE)
      } else if (metode == "modus") {
        nilai_imputasi <- hitung_modus(kolom_data)
      }

      # Untuk membulatkan hasil jika parameter round is TRUE
      if (round && !is.na(nilai_imputasi)) {
        nilai_imputasi <- round(nilai_imputasi, digit)
      }
    }
    # Untuk memproses kolom kategorikal
    else if (is.factor(kolom_data) || is.character(kolom_data) || is.logical(kolom_data)) {
      nilai_imputasi <- hitung_modus(kolom_data)

      if (metode != "modus" && verbose) {
        cat(sprintf("Catatan: Tidak dapat menggunakan imputasi mean/median pada kolom kategorikal '%s'\n", nama_kolom))
      }
    } else {
      # Skip jika tipe data kolom bukan numerik/kategorikal
      if (verbose) {
        cat(sprintf("Melewati kolom '%s' dengan tipe data yang tidak didukung\n", nama_kolom))
      }
      next
    }

    # Apabila nilai imputasi NA
    if (is.na(nilai_imputasi) || (is.numeric(nilai_imputasi) && is.nan(nilai_imputasi))) {
      if (verbose) {
        cat(sprintf("Tidak dapat mengimputasi kolom '%s': semua nilai adalah NA atau imputasi menghasilkan NA/NaN\n", nama_kolom))
      }
      next
    }

    # Untuk melakukan imputasi pada baris dengan nilai yang hilang
    hasil[[nama_kolom]][is_missing(kolom_data)] <- nilai_imputasi

    # Untuk menyimpan informasi yang akan ditampilkan
    info_imputasi[[nama_kolom]] <- list(
      jumlah = jumlah_na,
      persen = 100 * jumlah_na / length(kolom_data),
      nilai = nilai_imputasi,
      metode = if (is.factor(kolom_data) || is.character(kolom_data) || is.logical(kolom_data)) "modus" else metode,
      dibulatkan = round && is.numeric(nilai_imputasi)
    )
  }

  # Print summary jika verbose
  if (verbose && length(info_imputasi) > 0) {
    for (nama_kolom in names(info_imputasi)) {
      info <- info_imputasi[[nama_kolom]]

      # Untuk mengubah nilai imputasi menjadi numerik/string pada saat display
      if (is.numeric(info$nilai)) {
        nilai_str <- sprintf("%.4g", info$nilai)
      } else {
        nilai_str <- as.character(info$nilai)
      }

      info_pembulatan <- if (info$dibulatkan) sprintf(" (dibulatkan ke %d tempat desimal)", digit) else ""

      cat(sprintf("Kolom '%s': Melakukan imputasi %d nilai yang hilang (%.1f%%) dengan nilai %s = %s%s\n",
                  nama_kolom, info$jumlah, info$persen, info$metode, nilai_str, info_pembulatan))
    }
  } else if (verbose && length(info_imputasi) == 0) {
    cat("Tidak ada nilai yang diimputasi. Tidak ada nilai yang hilang atau tidak dapat diimputasi dengan metode yang dipilih.\n")
  }

  return(invisible(hasil))
}
