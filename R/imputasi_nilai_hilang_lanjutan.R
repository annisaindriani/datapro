#' Imputasi nilai yang hilang dalam dataset menggunakan metode lanjutan
#'
#' Fungsi ini mengganti nilai yang hilang dalam dataset menggunakan metode imputasi lanjutan
#' seperti K-Nearest Neighbors, MICE, Random Forest, dan Hot Deck.
#'
#' @param data Data frame atau tibble yang memiliki nilai yang hilang untuk diimputasi.
#' @param metode Metode yang digunakan untuk imputasi. Pilihan yang tersedia:
#'   \itemize{
#'      \item "knn": Imputasi K-nearest Neighbors.
#'      \item "mice": Imputasi MICE.
#'      \item "rf": Imputasi Random Forest.
#'      \item "hotdeck": Imputasi Hot deck.
#'      }
#' @param kolom Vektor karakter dari nama kolom yang akan diimputasi. Jika NULL (default),
#'   semua kolom dengan nilai yang hilang akan diimputasi.
#' @param k Integer, jumlah neighbors yang dipertimbangkan untuk imputasi KNN (default 5).
#' @param m Integer, jumlah multiple imputation untuk MICE (default 5).
#' @param maxit Integer, jumlah maksimum iterasi untuk MICE (default 5).
#' @param method Metode imputasi untuk MICE. Jika NULL (default), MICE akan menggunakan metode default.
#'   Pilihan yang tersedia: "pmm" (predictive mean matching), "norm" (normal), "logreg" (logistic regression),
#'   "polyreg" (polytomous regression), "polr" (proportional odds).
#' @param ntree Integer, jumlah trees untuk imputasi random forest (default 100).
#' @param seed Integer, random seed untuk reproduktibilitas (default 123).
#' @param round Logis, apakah nilai numerik yang diimputasi akan dibulatkan (default FALSE).
#' @param digit Integer yang menunjukkan jumlah angka desimal untuk pembulatan ketika
#'   bulatkan = TRUE (default 0)
#' @param verbose Logis, apakah akan mencetak informasi tentang nilai yang diimputasi (default TRUE).
#'
#' @return Data frame dengan nilai yang hilang diimputasi sesuai dengan metode yang ditentukan.
#'   Jika tidak ada nilai yang hilang yang ditemukan, mengembalikan data asli dengan pesan.
#'
#' @examples
#' # Membuat sampel data
#' data <- data.frame(
#'   umur = c(25, 30, NA, NA, 28),
#'   pendapatan = c(NA, 85000, 90000, 78000, 92000),
#'   pendidikan = c(16, 12, NA, 14, 16),
#'   jenis_kelamin = factor(c("L", "P", NA, "P", "L"))
#' )
#'
#' # Imputasi semua kolom menggunakan KNN
#' data_imputasi_knn <- imputasi_nilai_hilang_lanjutan(data, metode = "knn")
#' data_imputasi_knn
#'
#' # Imputasi semua kolom menggunakan MICE
#' data_imputasi_mice <- imputasi_nilai_hilang_lanjutan(data, metode = "mice")
#'
#' # Imputasi hanya kolom numerik menggunakan Random Forest
#' data_imputasi_rf <- imputasi_nilai_hilang_lanjutan(data, metode = "rf",
#'                                         kolom = c("umur", "pendapatan", "pendidikan"))
#'
#' # Imputasi semua kolom menggunakan Hot deck
#' data_imputasi_hotdeck <- imputasi_nilai_hilang_lanjutan(data, metode = "hotdeck")
#'
#' @seealso
#' \code{\link{imputasi_nilai_hilang}} untuk fungsi imputasi nilai yang hilang menggunakan metode biasa.
#' \code{\link{deteksi_nilai_hilang}} untuk fungsi identifikasi baris dengan nilai yang hilang dari dataset.
#' \code{\link{hapus_nilai_hilang}} untuk fungsi menghapus nilai yang hilang dalam dataset.
#'
#' @importFrom stats cor lm predict complete.cases median sd quantile
#' @importFrom VIM kNN hotdeck
#' @importFrom mice mice complete
#' @importFrom missForest missForest
#'
#' @export

imputasi_nilai_hilang_lanjutan <- function(data,
                                           metode = c("knn", "mice", "rf", "hotdeck"),
                                           kolom = NULL,
                                           k = 5,
                                           m = 5,
                                           maxit = 5,
                                           method = NULL,
                                           ntree = 100,
                                           seed = 123,
                                           round = FALSE,
                                           digit = 0,
                                           verbose = TRUE) {

  # Untuk melihat apakah input sudah berupa data frame atau tibble
  if (!is.data.frame(data)) {
    stop("Input harus berupa data frame atau tibble.")
  }

  # Untuk validasi argumen metode yang diberikan pengguna sesuai dengan pilihan yang tersedia
  metode <- match.arg(metode)

  # Apabila tidak ada data
  if (nrow(data) == 0) {
    cat("Dataset kosong. Tidak ada imputasi yang dilakukan.\n")
    return(invisible(data))
  }

  # Fungsi untuk deteksi nilai hilang (NA, NULL, dan string kosong)
  is_missing <- function(x) {
    is.na(x) | is.null(x) | (is.character(x) & x == "")
  }

  # Fungsi untuk convert string kosong ke NA untuk kompatibilitas dengan library imputasi
  convert_empty_to_na <- function(x) {
    if (is.character(x)) {
      x[x == ""] <- NA
    }
    return(x)
  }

  # Convert semua string kosong ke NA untuk kompatibilitas dengan library
  data_converted <- data
  for (i in seq_along(data_converted)) {
    data_converted[[i]] <- convert_empty_to_na(data_converted[[i]])
  }

  # Cek apakah ada nilai yang hilang (termasuk string kosong yang sudah diconvert)
  has_missing <- any(apply(data_converted, 2, function(col) any(is.na(col))))

  # Apabila tidak ada nilai yang hilang sama sekali
  if (!has_missing) {
    cat("Tidak ditemukan baris atau kolom dengan nilai yang hilang dalam dataset. Tidak ada imputasi yang dilakukan.\n")
    return(invisible(data))
  }

  # Untuk menyimpan data asli sebelum conversion
  data_asli <- data

  # Set seed untuk reproduktibilitas
  set.seed(seed)

  # Untuk validasi pada parameter kolom
  if (!is.null(kolom)) {
    kolom_tidak_valid <- setdiff(kolom, names(data_converted))
    if (length(kolom_tidak_valid) > 0) {
      stop("Kolom berikut tidak ada dalam dataset: ",
           paste(kolom_tidak_valid, collapse = ", "))
    }
    kolom_diproses <- kolom
  } else {
    kolom_diproses <- names(data_converted)
  }

  # Untuk melakukan imputasi berdasarkan metode yang dipilih
  tryCatch({
    if (metode == "knn") {
      if (verbose) cat("Melakukan imputasi menggunakan KNN...\n")
      hasil_imputasi <- VIM::kNN(data_converted, k = k, trace = FALSE)

      # Hapus kolom flag imputasi yang dihasilkan oleh VIM
      hasil_imputasi <- hasil_imputasi[, !grepl("_imp$", names(hasil_imputasi))]

    } else if (metode == "mice") {
      if (verbose) cat("Melakukan imputasi menggunakan MICE...\n")
      mice_result <- mice::mice(data_converted, m = m, maxit = maxit,
                                method = method, printFlag = verbose, seed = seed)
      hasil_imputasi <- mice::complete(mice_result)

    } else if (metode == "rf") {
      if (verbose) cat("Melakukan imputasi menggunakan Random Forest...\n")
      rf_result <- missForest::missForest(data_converted, ntree = ntree,
                                          verbose = verbose, parallelize = "no")
      hasil_imputasi <- rf_result$ximp

    } else if (metode == "hotdeck") {
      if (verbose) cat("Melakukan imputasi menggunakan Hot Deck...\n")
      hasil_imputasi <- VIM::hotdeck(data_converted, trace = FALSE)
      hasil_imputasi <- hasil_imputasi[, !grepl("_imp$", names(hasil_imputasi))]
    }

    # Semisal hanya impute kolom tertentu
    hasil_final <- data_converted
    if (!is.null(kolom)) {
      hasil_final[, kolom_diproses] <- hasil_imputasi[, kolom_diproses]
    } else {
      hasil_final <- hasil_imputasi
    }

    # Untuk melakukan pembulatan jika diperlukan
    if (round) {
      for (nama_kolom in names(hasil_final)) {
        if (is.numeric(hasil_final[[nama_kolom]])) {
          hasil_final[[nama_kolom]] <- round(hasil_final[[nama_kolom]], digit)
        }
      }
    }

    if (verbose) {
      for (nama_kolom in kolom_diproses) {
        jumlah_missing_asli <- sum(is_missing(data_asli[[nama_kolom]]))
        if (jumlah_missing_asli > 0) {
          persen_missing <- 100 * jumlah_missing_asli / nrow(data_asli)
          cat(sprintf("Kolom '%s': Melakukan imputasi %d nilai yang hilang (%.1f%%) menggunakan metode %s\n",
                      nama_kolom, jumlah_missing_asli, persen_missing, metode))
        }
      }
    }

    return(invisible(hasil_final))

  }, error = function(e) {
    cat("Terjadi kesalahan saat melakukan imputasi:", e$message, "\n")
    cat("Mengembalikan data asli tanpa imputasi.\n")
    return(invisible(data_asli))
  })
}
