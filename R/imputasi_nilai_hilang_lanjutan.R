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

  # Apabila tidak ada nilai yang hilang sama sekali
  if (!any(is.na(data))) {
    cat("Tidak ditemukan baris atau kolom dengan nilai yang hilang dalam dataset. Tidak ada imputasi yang dilakukan.\n")
    return(invisible(data))
  }

  set.seed(seed)

  # Untuk validasi parameter kolom
  if (!is.null(kolom)) {
    kolom_tidak_ada <- setdiff(kolom, names(data))
    if (length(kolom_tidak_ada) > 0) {
      stop("Kolom berikut tidak ditemukan dalam dataset: ",
           paste(kolom_tidak_ada, collapse = ", "))
    }
    kolom_untuk_diproses <- kolom
  } else {
    kolom_untuk_diproses <- names(data)
  }

  # Jika terdapat kolom yang semuanya berisi nilai yang hilang
  kolom_semua_na <- kolom_untuk_diproses[sapply(data[kolom_untuk_diproses], function(x) all(is.na(x)))]
  if (length(kolom_semua_na) > 0) {
    if (verbose) {
      cat("Kolom berikut memiliki semua nilai yang hilang dan tidak dapat diimputasi: ",
          paste(kolom_semua_na, collapse = ", "), "\n")
    }
    kolom_untuk_diproses <- setdiff(kolom_untuk_diproses, kolom_semua_na)
  }

  # Jika tidak ada kolom yang bisa diimputasi
  if (length(kolom_untuk_diproses) == 0) {
    cat("Tidak ada kolom yang valid untuk diimputasi setelah penyaringan.\n")
    return(invisible(data))
  }

  # Untuk menghitung modus
  hitung_modus <- function(x) {
    x_tanpa_na <- x[!is.na(x)]

    if (length(x_tanpa_na) == 0) return(NA)

    nilai_unik <- unique(x_tanpa_na)
    nilai_unik[which.max(tabulate(match(x_tanpa_na, nilai_unik)))]
  }

  # Untuk menyimpan data asli
  hasil <- data

  info_imputasi <- list()

  if (metode == "knn") {
    if (requireNamespace("VIM", quietly = TRUE)) {
      # Jika terdapat package eksternal yang dibutuhkan
      if (verbose) {
        cat("Melakukan imputasi KNN...\n")
      }

      # Untuk menyimpan nama kolom dan jumlah nilai yang hilang
      jumlah_na_kolom <- sapply(data[kolom_untuk_diproses], function(x) sum(is.na(x)))

      # Ambil kolom yang memiliki nilai yang hilang untuk proses imputasi
      kolom_dengan_nilai_hilang <- names(jumlah_na_kolom[jumlah_na_kolom > 0])

      if (length(kolom_dengan_nilai_hilang) > 0) {
        data_imputasi <- VIM::kNN(data, variable = kolom_untuk_diproses, k = k)

        # Hapus kolom flag imputasi yang dihasilkan oleh VIM
        data_imputasi <- data_imputasi[, !grepl("_imp$", names(data_imputasi))]

        hasil <- data_imputasi

        for (kol in kolom_dengan_nilai_hilang) {
          info_imputasi[[kol]] <- list(
            jumlah = jumlah_na_kolom[kol],
            persen = 100 * jumlah_na_kolom[kol] / nrow(data),
            metode = "KNN",
            k = k
          )
        }
      }
    } else {
      stop("Package 'VIM' diperlukan untuk imputasi KNN. Silakan install dengan: install.packages('VIM')")
    }
  } else if (metode == "mice") {
    if (requireNamespace("mice", quietly = TRUE)) {
      if (verbose) {
        cat("Melakukan imputasi MICE...\n")
      }

      # Untuk menyimpan nama kolom dan jumlah nilai yang hilang
      jumlah_na_kolom <- sapply(data[kolom_untuk_diproses], function(x) sum(is.na(x)))
      kolom_dengan_nilai_hilang <- names(jumlah_na_kolom[jumlah_na_kolom > 0])

      if (length(kolom_dengan_nilai_hilang) > 0) {

        # Konversi character ke factor untuk data kategorikal (unique < 50)
        for (kol in names(data)) {
          if (is.character(data[[kol]])) {
            unique_vals <- length(unique(data[[kol]][!is.na(data[[kol]])]))
            if (unique_vals < 50) {
              data[[kol]] <- factor(data[[kol]])
            } else {
              cat(sprintf("Kolom '%s' tidak diubah menjadi faktor karena memiliki %d nilai unik (> %d).\nApabila imputasi gagal, ubah kolom ini menjadi faktor terlebih dahulu.\n",
                          kol, unique_vals, max_unique))
            }
          }
        }

        tryCatch({
          set.seed(seed)
          data_mice <- mice::mice(data, m = m, maxit = maxit, printFlag = verbose)
          hasil <- mice::complete(data_mice)

          # Update info
          for (kol in kolom_dengan_nilai_hilang) {
            info_imputasi[[kol]] <- list(data,
              jumlah = jumlah_na_kolom[kol],
              persen = 100 * jumlah_na_kolom[kol] / nrow(data),
              metode = "MICE",
              iterasi = maxit
            )
          }

          cat("\n")

        }, error = function(e) {
          warning("MICE gagal melakukan imputasi: ", e$message, ". Menggunakan nilai median untuk data numerik dan modus untuk data kategorik.\n")

          # Fallback median untuk numerik dan modus untuk kategorik
          hasil <- data
          for (kol in kolom_dengan_nilai_hilang) {
            na_idx <- is.na(hasil[[kol]])
            if (any(na_idx)) {
              if (is.numeric(hasil[[kol]])) {
                hasil[[kol]][na_idx] <- median(hasil[[kol]], na.rm = TRUE)
                metode_used <- "Median (Fallback)"
              } else {
                mode_val <- hitung_modus(hasil[[kol]])
                hasil[[kol]][na_idx] <- mode_val
                metode_used <- "Modus (Fallback)"
              }
            }
          }

          cat("\n")

          for (kol in kolom_dengan_nilai_hilang) {
            info_imputasi[[kol]] <- list(
              jumlah = jumlah_na_kolom[kol],
              persen = 100 * jumlah_na_kolom[kol] / nrow(data),
              metode = metode_used,
              iterasi = 0
            )
          }
        })
      }
    } else {
      stop("Package 'mice' diperlukan untuk imputasi MICE. Silakan install dengan: install.packages('mice')")
    }
  } else if (metode == "rf") {
    if (requireNamespace("missForest", quietly = TRUE)) {
      if (verbose) cat("Melakukan imputasi Random Forest...\n")

      if (length(kolom_untuk_diproses) < 2) {
        stop("Random Forest membutuhkan minimal 2 kolom.\n  Gunakan metode lain untuk single column.")
      }

      jumlah_na_kolom <- sapply(data[kolom_untuk_diproses], function(x) sum(is.na(x)))
      kolom_dengan_nilai_hilang <- names(jumlah_na_kolom[jumlah_na_kolom > 0])

      # Konversi character ke factor untuk data kategorikal
      subset_data <- data[, kolom_untuk_diproses, drop = FALSE]
      for (kol in names(subset_data)) {
        if (is.character(subset_data[[kol]])) {
          unique_vals <- length(unique(subset_data[[kol]][!is.na(subset_data[[kol]])]))
          if (unique_vals < 50) {
            subset_data[[kol]] <- factor(subset_data[[kol]])
          } else {
            cat(sprintf("Kolom '%s' tidak diubah menjadi faktor karena memiliki %d nilai unik (> %d).\nApabila imputasi gagal, ubah kolom ini menjadi faktor terlebih dahulu.\n",
                            kol, unique_vals, max_unique))
          }
        }
      }

      tryCatch({
        hasil_rf <- missForest::missForest(subset_data, ntree = ntree, verbose = verbose)
        hasil[, kolom_untuk_diproses] <- hasil_rf$ximp

        for (kol in kolom_dengan_nilai_hilang) {
          info_imputasi[[kol]] <- list(
            jumlah = jumlah_na_kolom[kol],
            persen = round(100 * jumlah_na_kolom[kol] / nrow(data), 1),
            metode = "Random Forest",
            ntree = ntree
          )
        }

        cat("\n")

      }, error = function(e) {
        warning("Random Forest gagal melakukan imputasi: ", e$message, ".\n  Menggunakan nilai median untuk data numerik dan modus untuk data kategorik.\n")

         for (kol in kolom_dengan_nilai_hilang) {
          na_idx <- is.na(hasil[[kol]])
          if (any(na_idx)) {
            if (is.numeric(hasil[[kol]])) {
              fallback_val <- median(hasil[[kol]], na.rm = TRUE)
              hasil[[kol]][na_idx] <- fallback_val
              metode_used <- "Median (Fallback)"
            } else {
              non_na_vals <- hasil[[kol]][!na_idx]
              if (length(non_na_vals) > 0) {
                mode_val <- names(sort(table(non_na_vals), decreasing = TRUE))[1]
                hasil[[kol]][na_idx] <- mode_val
                metode_used <- "Modus (Fallback)"
              }
            }

            info_imputasi[[kol]] <- list(
              jumlah = jumlah_na_kolom[kol],
              persen = round(100 * jumlah_na_kolom[kol] / nrow(data), 1),
              metode = metode_used
            )
          }
        }
      })

    } else {
      stop("Package 'missForest' diperlukan untuk Random Forest.")
    }
  } else if (metode == "hotdeck") {
    if (requireNamespace("VIM", quietly = TRUE)) {
      if (verbose) {
        cat("Melakukan imputasi Hot Deck...\n")
      }

      # Untuk menyimpan nama kolom dan jumlah nilai yang hilang
      jumlah_na_kolom <- sapply(data[kolom_untuk_diproses], function(x) sum(is.na(x)))
      kolom_dengan_nilai_hilang <- names(jumlah_na_kolom[jumlah_na_kolom > 0])

      if (length(kolom_dengan_nilai_hilang) > 0) {
        data_imputasi <- VIM::hotdeck(data, variable = kolom_untuk_diproses)

        # Hapus kolom flag imputasi yang dihasilkan oleh VIM
        data_imputasi <- data_imputasi[, !grepl("_imp$", names(data_imputasi))]

        hasil <- data_imputasi

        for (kol in kolom_dengan_nilai_hilang) {
          info_imputasi[[kol]] <- list(
            jumlah = jumlah_na_kolom[kol],
            persen = 100 * jumlah_na_kolom[kol] / nrow(data),
            metode = "Hot Deck"
          )
        }
      }
    } else {
      stop("Package 'VIM' diperlukan untuk imputasi Hot Deck. Silakan install dengan: install.packages('VIM')")
    }
  }

  # Jika pengguna ingin membulatkan hasil
  if (round) {
    for (kol in kolom_untuk_diproses) {
      if (is.numeric(hasil[[kol]])) {
        # Tandai nilai yang diimputasi
        nilai_yang_hilang <- is.na(data[[kol]])

        if (any(nilai_yang_hilang)) {
          # Bulatkan nilai yang diimputasi
          hasil[[kol]][nilai_yang_hilang] <- round(hasil[[kol]][nilai_yang_hilang], digit)

          if (kol %in% names(info_imputasi)) {
            info_imputasi[[kol]]$dibulatkan <- TRUE
            info_imputasi[[kol]]$digit <- digit
          }
        }
      }
    }
  }

  # Print summary jika verbose TRUE
  if (verbose && length(info_imputasi) > 0) {
    for (nama_kolom in names(info_imputasi)) {
      info <- info_imputasi[[nama_kolom]]

      string_metode <- info$metode
      if (!is.null(info$k)) string_metode <- paste0(string_metode, " (k=", info$k, ")")
      if (!is.null(info$iterasi)) string_metode <- paste0(string_metode, " (iterasi=", info$iterasi, ")")
      if (!is.null(info$ntree)) string_metode <- paste0(string_metode, " (ntree=", info$ntree, ")")

      info_pembulatan <- if (!is.null(info$dibulatkan) && info$dibulatkan)
        sprintf(" (dibulatkan ke %d angka desimal)", info$digit)
      else
        ""

      cat(sprintf("Kolom '%s': Mengimputasi %d nilai yang hilang (%.1f%%) dengan %s%s\n",
                  nama_kolom, info$jumlah, info$persen, string_metode, info_pembulatan))
    }
  } else if (verbose && length(info_imputasi) == 0) {
    cat("Tidak ada nilai yang diimputasi. Tidak ada nilai yang hilang atau tidak dapat diimputasi dengan metode yang dipilih.\n")
  }

  return(invisible(hasil))
}
