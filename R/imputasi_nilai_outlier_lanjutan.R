#' Imputasi nilai outlier dalam dataset menggunakan metode lanjutan
#'
#' Fungsi ini mengganti nilai outlier dalam vektor numerik atau kolom data frame
#' dengan nilai imputasi menggunakan metode lanjutan seperti Winsorisasi,
#' Random Forest, K-Nearest Neighbors, atau IRMI.
#'
#' @param data Vektor numerik atau dataframe.
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
#' @return Jika input adalah vektor, mengembalikan vektor dengan nilai terimputasi.
#'         Jika input adalah dataframe, mengembalikan dataframe dengan nilai terimputasi.
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
#' @importFrom FNN knn.reg
#' @importFrom randomForest randomForest
#' @importFrom VIM irmi
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
    x <- seq_along(data[[kolom]])
    y <- data[[kolom]]

    # Untuk menyimpan nilai outlier
    nilai_outlier <- y[indeks_outlier]

    # Untuk membuat vektor berisi data non-outlier untuk training model
    x_bersih <- x[-indeks_outlier]
    y_bersih <- y[-indeks_outlier]

    if (metode_imputasi == "winsorisasi") {
      if (metode == "percentile") {
        batas_bawah <- stats::quantile(y_bersih, persentil_bawah)
        batas_atas <- stats::quantile(y_bersih, persentil_atas)
      } else {
        batas_bawah <- min(y_bersih)
        batas_atas <- max(y_bersih)
      }

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
      if (requireNamespace("FNN", quietly = TRUE)) {
        # Untuk menentukan prediktor numerik
        prediktor <- setdiff(names(data), kolom)
        prediktor_numerik <- names(data)[sapply(data, is.numeric)]
        prediktor_numerik <- setdiff(prediktor_numerik, kolom)

        if (length(prediktor_numerik) == 0) {
          # Apabila tidak ada prediktor numerik
          cat("Tidak ada prediktor numerik yang tersedia untuk KNN. Akan membuat prediktor indeks.")
          data$indeks_temp <- seq_len(nrow(data))
          prediktor_numerik <- "indeks_temp"
        }

        tryCatch({
          # Split data menjadi training (non-outlier) dan test (outlier)
          data_train <- data[-indeks_outlier, prediktor_numerik, drop = FALSE]
          target_train <- data[-indeks_outlier, kolom]
          data_test <- data[indeks_outlier, prediktor_numerik, drop = FALSE]

          # Untuk melakukan scaling data agar algoritma tidak bias terhadap variabel dengan rentang nilai lebih besar
          rata_rata <- colMeans(data_train)
          simpangan_baku <- apply(data_train, 2, stats::sd)

          # Untuk menghindari pembagian dengan nol
          simpangan_baku[simpangan_baku == 0] <- 1

          data_train_scaled <- scale(data_train, center = rata_rata, scale = simpangan_baku)
          data_test_scaled <- scale(data_test, center = rata_rata, scale = simpangan_baku)

          hasil_knn <- suppressWarnings(FNN::knn.reg(train = data_train_scaled,
                                    test = data_test_scaled,
                                    y = target_train,
                                    k = min(k, nrow(data_train))))

          # Untuk mengganti nilai outlier sesuai dengan hasil prediksi
          nilai_terimputasi <- y
          nilai_terimputasi[indeks_outlier] <- hasil_knn$pred

          # Untuk membulatkan angka jika round TRUE
          if (round && !is.na(nilai_terimputasi)) {
            nilai_terimputasi[indeks_outlier] <- round(nilai_terimputasi[indeks_outlier], digit)
          }

          hasil[[kolom]] <- nilai_terimputasi

          # Untuk menghapus kolom temp jika ada
          if ("indeks_temp" %in% names(hasil)) {
            hasil$indeks_temp <- NULL
          }

          if (verbose) {

            cat(sprintf("\nKolom '%s': Melakukan imputasi %d nilai outlier menggunakan metode KNN (k=%d)\n",
                        kolom, info_outlier$jumlah_nilai_outlier, min(k, nrow(data_train))))
          }
        }, error = function(e) {
          warning("Imputasi KNN gagal: ", e$message, ". Menggunakan Winsorisasi sebagai metode cadangan.")

          # Apabila KNN gagal imputasi
          batas_bawah <- min(y_bersih)
          batas_atas <- max(y_bersih)

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
        stop("Paket 'FNN' diperlukan untuk imputasi KNN. Silakan install dengan: install.packages('FNN')")
      }
    } else if (metode_imputasi == "rf") {
      if (requireNamespace("randomForest", quietly = TRUE)) {
        # Untuk menentukan prediktor numerik
        prediktor <- setdiff(names(data), kolom)
        prediktor_numerik <- names(data)[sapply(data, is.numeric)]
        prediktor_numerik <- setdiff(prediktor_numerik, kolom)

        if (length(prediktor_numerik) == 0) {
          # Apabila tidak ada prediktor numerik
          warning("Tidak ada prediktor numerik yang tersedia untuk Random Forest. Membuat prediktor indeks.\n")
          data$indeks_temp <- seq_len(nrow(data))
          prediktor_numerik <- "indeks_temp"
        }

        tryCatch({
          # Split data menjadi training (non-outlier) dan test (outlier)
          data_train <- data[-indeks_outlier, prediktor_numerik, drop = FALSE]
          target_train <- data[-indeks_outlier, kolom]
          data_test <- data[indeks_outlier, prediktor_numerik, drop = FALSE]

          model_rf <- suppressWarnings(
            randomForest::randomForest(
              x = data_train,
              y = target_train,
              ntree = ntree
            )
          )

          # Untuk mengganti nilai outlier sesuai dengan hasil prediksi
          nilai_prediksi <- stats::predict(model_rf, newdata = data_test)
          nilai_terimputasi <- y
          nilai_terimputasi[indeks_outlier] <- nilai_prediksi

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
          batas_bawah <- min(y_bersih)
          batas_atas <- max(y_bersih)

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
        stop("Paket 'randomForest' diperlukan untuk imputasi Random Forest. Silakan install dengan: install.packages('randomForest')")
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
          batas_bawah <- min(y_bersih)
          batas_atas <- max(y_bersih)

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
          round(skewness(data[[kolom]], na.rm = TRUE), 4)
        ),
        `Setelah Penanganan` = c(
          length(hasil[[kolom]]),
          round(mean(hasil[[kolom]], na.rm = TRUE), 4),
          round(sd(hasil[[kolom]], na.rm = TRUE), 4),
          round(min(hasil[[kolom]], na.rm = TRUE), 4),
          round(max(hasil[[kolom]], na.rm = TRUE), 4),
          round(median(hasil[[kolom]], na.rm = TRUE), 4),
          round(skewness(hasil[[kolom]], na.rm = TRUE), 4)
        )
      )

      print(orig_stats)

    }

    return(invisible(hasil))

  } else {
    # Apabila input berupa vector
    if (!is.numeric(data)) {
      stop("Vektor input harus numerik.")
    }

    if (any(is.na(data))) {
      stop("Terdapat nilai yang hilang dalam data. Imputasi tidak bisa dilakukan.")
    }

    # Untuk mengecek nilai outlier dengan fungsi deteksi_nilai_outlier
    info_outlier <- deteksi_nilai_outlier(data,
                                      metode = metode,
                                      ambang_batas = ambang_batas,
                                      persentil_bawah = persentil_bawah,
                                      persentil_atas = persentil_atas)

    # Apabila tidak ditemukan nilai outlier
    if (info_outlier$jumlah_nilai_outlier == 0) {
      if (verbose) {
        cat("Tidak ditemukan nilai outlier yang terdeteksi dalam kolom yang ditentukan. Tidak ada imputasi yang dilakukan.\n")
      }
      return(data)
    }

    # Untuk mengambil indeks data yang merupakan nilai outlier
    # Menggunakan kolom status outlier dari hasil deteksi
    status_outlier <- info_outlier$data$adalah_nilai_outlier
    indeks_outlier <- which(status_outlier)

    # Untuk menyimpan data asli sebelum dilakukan imputasi
    hasil <- data
    x <- seq_along(data)
    y <- data

    # Untuk menyimpan nilai outlier
    nilai_outlier <- y[indeks_outlier]

    # Untuk membuat vektor berisi data non-outlier untuk training model
    x_bersih <- x[-indeks_outlier]
    y_bersih <- y[-indeks_outlier]

    if (metode_imputasi == "winsorisasi") {
      if (metode == "percentile") {
        batas_bawah <- stats::quantile(y_bersih, persentil_bawah)
        batas_atas <- stats::quantile(y_bersih, persentil_atas)
      } else {
        batas_bawah <- min(y_bersih)
        batas_atas <- max(y_bersih)
      }

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

      hasil <- nilai_terimputasi

      if (verbose) {
        cat(sprintf("\nMelakukan imputasi %d nilai outlier menggunakan metode Winsorisasi\n",
                    info_outlier$jumlah_nilai_outlier))
        if (length(outlier_bawah) > 0) {
          cat(sprintf("  - %d nilai outlier bawah diganti dengan %.4g\n",
                      length(outlier_bawah), batas_bawah))
        }
        if (length(outlier_atas) > 0) {
          cat(sprintf("  - %d nilai outlier atas diganti dengan %.4g\n",
                      length(outlier_atas), batas_atas))
        }
      }

    } else if (metode_imputasi %in% c("knn", "rf", "irmi")) {
      # Apabila input vector maka KNN, RF, dan IRMI tidak bisa dilakukan karena membutuhkan prediktor
      warning(paste(metode_imputasi, "membutuhkan beberapa prediktor tetapi input adalah vektor.",
                    "Menggunakan Winsorisasi sebagai metode cadangan."))

      batas_bawah <- min(y_bersih)
      batas_atas <- max(y_bersih)

      nilai_terimputasi <- y
      outlier_bawah <- indeks_outlier[y[indeks_outlier] < batas_bawah]
      outlier_atas <- indeks_outlier[y[indeks_outlier] > batas_atas]

      if (length(outlier_bawah) > 0) {
        nilai_terimputasi[outlier_bawah] <- batas_bawah
      }
      if (length(outlier_atas) > 0) {
        nilai_terimputasi[outlier_atas] <- batas_atas
      }

      hasil <- nilai_terimputasi

      if (verbose) {
        cat(sprintf("\nMelakukan imputasi %d nilai outlier menggunakan metode Winsorisasi (metode cadangan)\n",
                    info_outlier$jumlah_nilai_outlier))
      }
    }

    # Print hasil
    if (verbose) {
      cat(sprintf("\nKolom: %s\n", deparse(substitute(data))))
      orig_stats <- data.frame(
        "Statistik" = c("Count", "Mean", "SD", "Min", "Max", "Median", "Skewness"),
        `Sebelum Penanganan` = c(
          length(data),
          round(mean(data, na.rm = TRUE), 4),
          round(sd(data, na.rm = TRUE), 4),
          round(min(data, na.rm = TRUE), 4),
          round(max(data, na.rm = TRUE), 4),
          round(median(data, na.rm = TRUE), 4),
          round(skewness(data, na.rm = TRUE), 4)
        ),
        `Setelah Penanganan` = c(
          length(hasil),
          round(mean(hasil, na.rm = TRUE), 4),
          round(sd(hasil, na.rm = TRUE), 4),
          round(min(hasil, na.rm = TRUE), 4),
          round(max(hasil, na.rm = TRUE), 4),
          round(median(hasil, na.rm = TRUE), 4),
          round(skewness(hasil), 4)
        )
      )

      print(orig_stats)

    }

    return(invisible(hasil))
  }
}
