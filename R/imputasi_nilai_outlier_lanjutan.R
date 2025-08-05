#' Imputasi nilai outlier dalam dataset menggunakan metode lanjutan
#'
#' Fungsi ini mengganti nilai outlier dalam kolom data frame
#' dengan nilai imputasi menggunakan metode lanjutan seperti Winsorisasi,
#' Random Forest, K-Nearest Neighbors, atau IRMI.
#'
#' @param data Sebuah data frame.
#' @param kolom Jika data adalah dataframe, nama kolom yang akan diproses.
#' @param metode Metode yang digunakan untuk deteksi nilai outlier. Pilihan:
#'    \itemize{
#'          \item "z-score": Menggunakan standar deviasi dari rata-rata.
#'          \item "iqr": Menggunakan rentang interkuartil.
#'          \item "percentile": Menggunakan ambang batas persentil khusus.
#'    }
#' @param ambang_batas Ambang batas untuk Z-Score (default 3) atau IQR (default 1.5).
#' @param persentil_bawah Ambang batas persentil bawah (default 0.05).
#' @param persentil_atas Ambang batas persentil atas (default 0.95).
#' @param metode_imputasi Metode yang digunakan untuk imputasi. Pilihan:
#'   \itemize{
#'        \item \code{winsorisasi}: Mengganti nilai outlier dengan nilai pada persentil yang ditentukan.
#'        \item \code{knn}: Mengganti nilai outlier dengan nilai yang diprediksi oleh algoritma K-nearest neighbors.
#'        \item \code{rf}: Menggunakan algoritma Random Forest untuk memprediksi nilai pengganti.
#'        \item \code{irmi}: Menggunakan metode Iterative Robust Model-based Imputation.
#'   }
#' @param k Integer, jumlah neighbor untuk imputasi KNN (default 5).
#' @param ntree Integer, jumlah pohon untuk imputasi random forest (default 100).
#' @param round Logikal, apakah akan membulatkan nilai numerik yang diimputasi (default FALSE).
#' @param digit Jumlah tempat desimal untuk pembulatan nilai imputasi (default 0).
#' @param seed Integer, seed acak untuk reprodusibilitas (default 123).
#' @param verbose Logikal, apakah akan menPrint informasi tentang nilai terimputasi (default TRUE).
#'
#' @return Jika input adalah dataframe, mengembalikan dataframe dengan nilai terimputasi.
#'
#' @examples
#' # Membuat sampel data
#' df <- data.frame(
#'             id = 1:14,
#'             nilai1 = c(5, 7, 6, 8, 6, 7, 5, 8, 9, 50, 7, 6, 8, 100),
#'             nilai2 = c(10, 12, 9, 11, 10, 11, 9, 11, 12, 11, 10, 9, 80, 12),
#'             grup = factor(rep(c("A", "B"), each = 7))
#'        )
#'
#' # Imputasi menggunakan KNN
#' df_imputasi_knn <- imputasi_nilai_outlier_lanjutan(df, kolom = "nilai1",
#'                                      metode = "iqr",
#'                                      metode_imputasi = "knn")
#' df_imputasi_knn
#'
#' @seealso
#' \code{\link{imputasi_nilai_outlier}} untuk fungsi imputasi nilai outlier menggunakan metode biasa.
#' \code{\link{plot_nilai_outlier}} untuk fungsi visualisasi data setelah menangani nilai outlier.
#' \code{\link{deteksi_nilai_outlier}} untuk fungsi identifikasi nilai outlier dalam dataset.
#'
#' @importFrom stats predict median quantile sd
#' @importFrom missForest missForest
#' @importFrom VIM irmi KNN
#' @importFrom moments skewness
#'
#' @export

imputasi_nilai_outlier_lanjutan <- function(data, kolom = NULL,
                                            metode = c("z-score", "iqr", "percentile"),
                                            ambang_batas = NULL,
                                            persentil_bawah = 0.05,
                                            persentil_atas = 0.95,
                                            metode_imputasi = c("winsorisasi", "knn", "rf", "irmi"),
                                            k = 5,
                                            ntree = 100,
                                            round = FALSE,
                                            digit = 0,
                                            seed = 123,
                                            verbose = TRUE) {

  # Untuk validasi dan mencocokkan argumen metode yang diberikan user sesuai dengan pilihan yang ada
  metode <- match.arg(metode)
  metode_imputasi <- match.arg(metode_imputasi)

  set.seed(seed)

  # Untuk melihat apakah input datanya sudah sesuai
  if (is.data.frame(data)) {
    if (is.null(kolom)) {
      stop("Nama kolom harus ditentukan ketika input adalah dataframe.")
    }
    if (!kolom %in% names(data)) {
      stop("Kolom yang ditentukan tidak ditemukan dalam dataframe.")
    }
    if (!is.numeric(data[[kolom]])) {
      stop("Kolom yang ditentukan harus numerik.")
    }

    if (any(is.na(data[[kolom]]))) {
      stop("Terdapat nilai yang hilang dalam data. Imputasi tidak bisa dilakukan.")
    }

    # Untuk mengecek nilai outlier dengan fungsi deteksi_nilai_outlier
    info_outlier <- deteksi_nilai_outlier(data[[kolom]],
                                      metode = metode,
                                      ambang_batas = ambang_batas,
                                      persentil_bawah = persentil_bawah,
                                      persentil_atas = persentil_atas)

    # Apabila tidak ditemukan nilai outlier
    if (info_outlier$jumlah_nilai_outlier == 0) {
      if (verbose) {
        cat("\nTidak ditemukan nilai outlier yang terdeteksi dalam kolom yang ditentukan. Tidak ada imputasi yang dilakukan.\n")
      }
      return(invisible(data))
    }

    # Untuk mengambil indeks data yang merupakan nilai outlier
    # Menggunakan kolom status outlier dari hasil deteksi
    status_outlier <- info_outlier$data$adalah_nilai_outlier
    indeks_outlier <- which(status_outlier)

    # Untuk menyimpan data asli sebelum dilakukan imputasi
    hasil <- data
    y <- data[[kolom]]

    # Untuk menyimpan nilai outlier
    nilai_outlier <- y[indeks_outlier]

    if (metode_imputasi == "winsorisasi") {
      # Menentukan batas atas dan batas bawah berdasarkan percentile
      batas_bawah <- stats::quantile(y, persentil_bawah, na.rm = TRUE)
      batas_atas <- stats::quantile(y, persentil_atas, na.rm = TRUE)

      # Untuk mengganti nilai outlier sesuai dengan batas yang telah ditentukan
      nilai_terimputasi <- y
      outlier_bawah <- indeks_outlier[y[indeks_outlier] < batas_bawah]
      outlier_atas <- indeks_outlier[y[indeks_outlier] > batas_atas]

      if (length(outlier_bawah) > 0) {
        nilai_terimputasi[outlier_bawah] <- batas_bawah
      }
      if (length(outlier_atas) > 0) {
        nilai_terimputasi[outlier_atas] <- batas_atas
      }

      hasil[[kolom]] <- nilai_terimputasi

      if (verbose) {
        cat(sprintf("\nKolom '%s': Melakukan imputasi %d nilai outlier dengan menggunakan metode Winsorisasi\n",
                    kolom, info_outlier$jumlah_nilai_outlier))
        if (length(outlier_bawah) > 0) {
          cat(sprintf("  - %d nilai outlier bawah diganti dengan %.4g\n",
                      length(outlier_bawah), batas_bawah))
        }
        if (length(outlier_atas) > 0) {
          cat(sprintf("  - %d nilai outlier atas diganti dengan %.4g\n",
                      length(outlier_atas), batas_atas))
        }
      }
    } else if (metode_imputasi == "knn") {
      if (requireNamespace("VIM", quietly = TRUE)) {
        tryCatch({
          # Membuat dataset untuk KNN dengan menandai nilai outlier sebagai NA
          data_knn <- data
          data_knn[indeks_outlier, kolom] <- NA

          hasil_imputasi <- VIM::kNN(data_knn, k = k, trace = FALSE)

          # Hapus kolom flag imputasi yang dihasilkan oleh VIM
          hasil_imputasi <- hasil_imputasi[, !grepl("_imp$", names(hasil_imputasi))]

          # Ambil nilai hasil imputasi
          nilai_terimputasi <- y
          nilai_terimputasi[indeks_outlier] <- hasil_imputasi[[kolom]][indeks_outlier]

          # Untuk membulatkan angka jika round TRUE
          if (round && !is.na(nilai_terimputasi)) {
            nilai_terimputasi[indeks_outlier] <- round(nilai_terimputasi[indeks_outlier], digit)
          }

          hasil[[kolom]] <- nilai_terimputasi

          if (verbose) {

            cat(sprintf("\nKolom '%s': Melakukan imputasi %d nilai outlier menggunakan metode KNN (k=%d)\n",
                        kolom, info_outlier$jumlah_nilai_outlier, k))
          }
        }, error = function(e) {
          warning("Imputasi KNN gagal: ", e$message, ". Menggunakan Winsorisasi sebagai metode cadangan.")

          # Apabila KNN gagal imputasi
          # Menentukan batas atas dan batas bawah berdasarkan percentile
          batas_bawah <- stats::quantile(y, persentil_bawah, na.rm = TRUE)
          batas_atas <- stats::quantile(y, persentil_atas, na.rm = TRUE)

          nilai_terimputasi <- y
          outlier_bawah <- indeks_outlier[y[indeks_outlier] < batas_bawah]
          outlier_atas <- indeks_outlier[y[indeks_outlier] > batas_atas]

          if (length(outlier_bawah) > 0) {
            nilai_terimputasi[outlier_bawah] <- batas_bawah
          }
          if (length(outlier_atas) > 0) {
            nilai_terimputasi[outlier_atas] <- batas_atas
          }

          hasil[[kolom]] <- nilai_terimputasi

          if ("indeks_temp" %in% names(hasil)) {
            hasil$indeks_temp <- NULL
          }

          if (verbose) {
            cat(sprintf("\nKolom '%s': Melakukan imputasi %d nilai outlier menggunakan metode Winsorisasi (metode cadangan)\n",
                        kolom, info_outlier$jumlah_nilai_outlier))
          }
        })
      } else {
        stop("Paket 'VIM' diperlukan untuk imputasi KNN. Silakan install dengan: install.packages('FNN')")
      }
    } else if (metode_imputasi == "rf") {
      if (requireNamespace("missForest", quietly = TRUE)) {
        tryCatch({
          # Membuat dataset untuk RF dengan menandai nilai outlier sebagai NA
          data_rf <- data
          data_rf[indeks_outlier, kolom] <- NA

          rf_result <- missForest::missForest(data_rf, ntree = ntree,
                                              verbose = verbose, parallelize = "no")
          hasil_imputasi <- rf_result$ximp

          # Ambil nilai hasil imputasi
          nilai_terimputasi <- y
          nilai_terimputasi[indeks_outlier] <- hasil_imputasi

          # Untuk membulatkan angka jika round TRUE
          if (round && !is.na(nilai_terimputasi)) {
            nilai_terimputasi[indeks_outlier] <- round(nilai_terimputasi[indeks_outlier], digit)
          }

          hasil[[kolom]] <- nilai_terimputasi

          # Membersihkan kolom sementara jika dibuat
          if ("indeks_temp" %in% names(hasil)) {
            hasil$indeks_temp <- NULL
          }

          if (verbose) {
            cat(sprintf("\nKolom '%s': Melakukan imputasi %d nilai outlier menggunakan metode Random Forest (ntree=%d)\n",
                        kolom, info_outlier$jumlah_nilai_outlier, ntree))
          }
        }, error = function(e) {
          warning("Imputasi Random Forest gagal: ", e$message, ". Menggunakan Winsorisasi sebagai metode cadangan")

          # Apabila RF gagal imputasi
          # Menentukan batas atas dan batas bawah berdasarkan percentile
          batas_bawah <- stats::quantile(y, persentil_bawah, na.rm = TRUE)
          batas_atas <- stats::quantile(y, persentil_atas, na.rm = TRUE)

          nilai_terimputasi <- y
          outlier_bawah <- indeks_outlier[y[indeks_outlier] < batas_bawah]
          outlier_atas <- indeks_outlier[y[indeks_outlier] > batas_atas]

          if (length(outlier_bawah) > 0) {
            nilai_terimputasi[outlier_bawah] <- batas_bawah
          }
          if (length(outlier_atas) > 0) {
            nilai_terimputasi[outlier_atas] <- batas_atas
          }

          hasil[[kolom]] <- nilai_terimputasi

          if ("indeks_temp" %in% names(hasil)) {
            hasil$indeks_temp <- NULL
          }

          if (verbose) {
            cat(sprintf("\nKolom '%s': Melakukan imputasi %d nilai outlier menggunakan metode Winsorisasi (metode cadangan)\n",
                        kolom, info_outlier$jumlah_nilai_outlier))
          }
        })
      } else {
        stop("Paket 'missForest' diperlukan untuk imputasi Random Forest. Silakan install dengan: install.packages('randomForest')")
      }
    } else if (metode_imputasi == "irmi") {
      if (requireNamespace("VIM", quietly = TRUE)) {
        tryCatch({
          # Membuat dataset untuk IRMI dengan menandai nilai outlier sebagai NA
          data_irmi <- data
          data_irmi[indeks_outlier, kolom] <- NA

          # Melakukan imputasi IRMI
          hasil_irmi <- suppressWarnings(VIM::irmi(data_irmi))

          # Untuk mengganti nilai outlier sesuai dengan hasil imputasi IRMI
          nilai_terimputasi <- y
          nilai_terimputasi[indeks_outlier] <- hasil_irmi[indeks_outlier, kolom]

          # Untuk membulatkan angka jika round TRUE
          if (round && !is.na(nilai_terimputasi)) {
            nilai_terimputasi[indeks_outlier] <- round(nilai_terimputasi[indeks_outlier], digit)
          }

          hasil[[kolom]] <- nilai_terimputasi

          if (verbose) {
            cat(sprintf("\nKolom '%s': Melakukan imputasi %d nilai outlier menggunakan metode IRMI\n",
                        kolom, info_outlier$jumlah_nilai_outlier))
          }
        }, error = function(e) {
          warning("Imputasi IRMI gagal: ", e$message, ". Menggunakan Winsorisasi sebagai metode cadangan")

          # Apabila IRMI gagal imputasi
          # Menentukan batas atas dan batas bawah berdasarkan percentile
          batas_bawah <- stats::quantile(y, persentil_bawah, na.rm = TRUE)
          batas_atas <- stats::quantile(y, persentil_atas, na.rm = TRUE)

          nilai_terimputasi <- y
          outlier_bawah <- indeks_outlier[y[indeks_outlier] < batas_bawah]
          outlier_atas <- indeks_outlier[y[indeks_outlier] > batas_atas]

          if (length(outlier_bawah) > 0) {
            nilai_terimputasi[outlier_bawah] <- batas_bawah
          }
          if (length(outlier_atas) > 0) {
            nilai_terimputasi[outlier_atas] <- batas_atas
          }

          hasil[[kolom]] <- nilai_terimputasi

          if (verbose) {
            cat(sprintf("\nKolom '%s': Melakukan imputasi %d nilai outlier menggunakan metode Winsorisasi (metode cadangan)\n",
                        kolom, info_outlier$jumlah_nilai_outlier))
          }
        })
      } else {
        stop("Paket 'VIM' diperlukan untuk imputasi IRMI. Silakan install dengan: install.packages('VIM')")
      }
    }
  }
  # Print hasil
  if (verbose) {
    cat(sprintf("\nKolom: %s\n", kolom))
    orig_stats <- data.frame(
      "Statistik" = c("Count", "Mean", "SD", "Min", "Max", "Median", "Skewness"),
      `Sebelum Penanganan` = c(
        length(data[[kolom]]),
        round(mean(data[[kolom]], na.rm = TRUE), 4),
        round(sd(data[[kolom]], na.rm = TRUE), 4),
        round(min(data[[kolom]], na.rm = TRUE), 4),
        round(max(data[[kolom]], na.rm = TRUE), 4),
        round(median(data[[kolom]], na.rm = TRUE), 4),
        round(moments::skewness(data[[kolom]], na.rm = TRUE), 4)
      ),
      `Setelah Penanganan` = c(
        length(hasil[[kolom]]),
        round(mean(hasil[[kolom]], na.rm = TRUE), 4),
        round(sd(hasil[[kolom]], na.rm = TRUE), 4),
        round(min(hasil[[kolom]], na.rm = TRUE), 4),
        round(max(hasil[[kolom]], na.rm = TRUE), 4),
        round(median(hasil[[kolom]], na.rm = TRUE), 4),
        round(moments::skewness(hasil[[kolom]], na.rm = TRUE), 4)
      )
    )

    print(orig_stats)

  }

  return(invisible(hasil))

}
