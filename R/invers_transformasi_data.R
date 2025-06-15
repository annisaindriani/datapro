#' Mengembalikan data yang telah ditransformasi ke skala aslinya
#'
#' Fungsi ini mengembalikan data yang telah ditransformasi menggunakan fungsi transformasi_data
#' kembali ke skala dan distribusi aslinya. Fungsi ini memerlukan parameter yang disimpan
#' saat melakukan transformasi dengan opsi invers_params = TRUE.
#'
#' @param data Sebuah Data frame, tibble, atau vektor numerik yang telah ditransformasi dan akan dikembalikan ke skala asli.
#' @param params Parameter yang dihasilkan dari fungsi transformasi_data dengan opsi invers_params = TRUE.
#'   Parameter ini berisi informasi transformasi yang diperlukan untuk melakukan invers.
#' @param verbose Logis, apakah akan mencetak informasi tentang proses invers (default TRUE).
#'
#' @return Data dalam format yang sama dengan input, tetapi telah dikembalikan ke skala aslinya.
#'   Jika input berupa vektor, mengembalikan vektor. Jika input berupa data frame, mengembalikan data frame.
#'
#' @examples
#' # Membuat sampel data
#' data_asli <- c(1, 2, 3, 4, 5, 10, 15, 20)
#' hasil_transform <- transformasi_data(data_asli, metode = "log", invers_params = TRUE)
#' data_transformed <- hasil_transform$data
#' params <- hasil_transform$params
#' data_kembali <- invers_transformasi_data(data_transformed, params)
#'
#' # Membuat sampel data
#' df <- data.frame(a = c(1, 2, 3, 4, 5), b = c(10, 20, 30, 40, 50))
#' df_transform <- transformasi_data(df, kolom = c("a", "b"), metode = "boxcox", invers_params = TRUE)
#' df_transformed <- df_transform$data
#' params_transform <- df_transform$params
#' df_kembali <- invers_transformasi_data(df_transformed, params_transform)
#'
#' @seealso
#' \code{\link{transformasi_data}} untuk fungsi transformasi.
#' \code{\link{plot_transformasi}} untuk fungsi visualisasi data sebelum dan sesudah transformasi
#' \code{\link{normalisasi_data}} untuk fungsi menerapkan normalisasi pada dataset.
#'
#' @importFrom moments skewness
#' @importFrom stats setNames
#' @importFrom utils head
#'
#' @export

invers_transformasi_data <- function(data, params, verbose = TRUE) {

  # Untuk validasi input
  if(is.null(params) || length(params) == 0) {
    stop("Parameter invers tidak tersedia. Pastikan transformasi dilakukan dengan opsi invers_params = TRUE.")
  }

  # Untuk cek tipe input data pengguna
  adalah_vektor <- is.vector(data) && !is.list(data)

  if (adalah_vektor) {
    nama_vektor <- deparse(substitute(data))

    if (!is.numeric(data)) {
      stop("Untuk input vektor, data harus berupa numerik.")
    }

    data_asli <- data
    data_df <- data.frame(x = data)
    kolom_list <- "x"

    # Untuk input vektor, parameter langsung digunakan tanpa nama kolom
    if ("metode" %in% names(params)) {
      params <- setNames(list(params), "x")
    }

    input_vektor <- TRUE

  } else {
    # Memastikan bahwa input data merupakan data frame atau tibble
    if (!is.data.frame(data)) {
      stop("Input harus berupa data frame, tibble, atau vektor numerik.")
    }

    data_asli <- data
    data_df <- data

    # Untuk data frame, parameter bisa berupa list dengan nama kolom atau single parameter
    if ("metode" %in% names(params)) {
      if ("kolom" %in% names(params)) {
        kolom_list <- params$kolom
      } else {
        kolom_numerik <- names(data_df)[sapply(data_df, is.numeric)]

        if (length(kolom_numerik) == 0) {
          stop("Tidak ditemukan kolom numerik dalam data frame.")
        }

        kolom_list <- kolom_numerik[1]

        if (verbose) {
          cat("Peringatan: Nama kolom tidak ditemukan dalam parameter. Menggunakan kolom numerik pertama:", kolom_list, "\n")
        }
      }

      params <- setNames(list(params), kolom_list)
    } else {
      kolom_list <- names(params)
    }

    input_vektor <- FALSE
  }

  # Apabila dataset kosong
  if (nrow(data_df) == 0) {
    if (verbose) {
      cat("Dataset kosong. Tidak ada kolom yang dilakukan invers transformasi.\n")
    }

    return(invisible(data))
  }

  # Proses invers untuk setiap kolom
  for(kol in kolom_list) {
    if(kol %in% names(params)) {
      param <- params[[kol]]
      x <- data_df[[kol]]

      if(param$metode == "log") {
        # Invers log transformation
        if(!is.null(param$konstanta) && param$konstanta > 0) {
          # log(x + konstanta) -> exp(x) - konstanta
          data_df[[kol]] <- exp(x) - param$konstanta

        } else {
          # log(x) -> exp(x)
          data_df[[kol]] <- exp(x)
        }

      } else if(param$metode == "sqrt") {
        # Invers sqrt: sqrt(x) -> x^2
        data_df[[kol]] <- x^2

      } else if(param$metode == "boxcox") {
        # Invers Box-Cox transformation
        lambda <- param$lambda

        if (abs(lambda) < 1e-10) {
          # Jika lambda ≈ 0: log(x) -> exp(x)
          data_df[[kol]] <- exp(x)
        } else {
          # Invers Box-Cox: ((y * lambda) + 1)^(1/lambda)
          data_df[[kol]] <- (x * lambda + 1)^(1/lambda)
        }

      } else if(param$metode == "yeojohnson") {
        # Invers Yeo-Johnson transformation
        if (!requireNamespace("car", quietly = TRUE)) {
          stop("Package 'car' diperlukan untuk invers transformasi Yeo-Johnson.")
        }

        lambda <- param$lambda

        # Implementasi manual invers Yeo-Johnson
        result <- numeric(length(x))

        for (i in seq_along(x)) {
          if (is.na(x[i])) {
            result[i] <- NA
          } else if (x[i] >= 0) {
            # Untuk nilai non-negatif
            if (abs(lambda) < 1e-10) {
              # Jika lambda ≈ 0: exp(x) - 1
              result[i] <- exp(x[i]) - 1
            } else {
              # Invers: (lambda * x + 1)^(1/lambda) - 1
              result[i] <- (lambda * x[i] + 1)^(1/lambda) - 1
            }
          } else {
            # Untuk nilai negatif
            if (abs(lambda - 2) < 1e-10) {
              # Jika lambda ≈ 2: 1 - exp(-x)
              result[i] <- 1 - exp(-x[i])
            } else {
              # Invers: 1 - (-(2-lambda) * x + 1)^(1/(2-lambda))
              result[i] <- 1 - (-(2 - lambda) * x[i] + 1)^(1/(2 - lambda))
            }
          }
        }

        data_df[[kol]] <- result

      } else if(param$metode == "exp") {
        # Invers exp: exp(x) -> log(x)
        if (any(x <= 0, na.rm = TRUE)) {
          if (verbose) {
            cat("Peringatan untuk kolom '", kol, "': Nilai non-positif ditemukan untuk invers exp. Hasil mungkin mengandung NA.\n")
          }
        }

        data_df[[kol]] <- log(x)

      } else if(param$metode == "power") {
        lambda <- param$lambda

        if (lambda == 0) {
          if (verbose) {
            cat("Peringatan untuk kolom '", kol, "': Lambda = 0 untuk transformasi pangkat. Tidak dapat melakukan invers.\n")
          }
          next
        }

        # Untuk pangkat genap dengan nilai negatif
        if (lambda %% 2 == 0 && !is.null(param$negative_indices) && length(param$negative_indices) > 0) {
          # Untuk pangkat genap, ambil akar dengan mempertahankan tanda asli
          result <- x^(1/lambda)

          # Kembalikan tanda negatif untuk indeks yang aslinya negatif
          if (length(param$negative_indices) > 0) {
            # Pastikan indeks masih valid
            valid_indices <- param$negative_indices[param$negative_indices <= length(result)]
            if (length(valid_indices) > 0) {
              result[valid_indices] <- -abs(result[valid_indices])
            }
          }

          data_df[[kol]] <- result

          if (verbose) {
            cat("Info untuk kolom '", kol, "': Mengembalikan tanda negatif untuk ",
                length(param$negative_indices), " nilai yang aslinya negatif.\n")
          }
        } else {
          # Untuk pangkat ganjil atau tidak ada nilai negatif, inverse normal
          data_df[[kol]] <- sign(x) * abs(x)^(1/lambda)
        }



      } else {
        if (verbose) {
          cat("Metode '", param$metode, "' tidak dikenal untuk invers. Kolom '", kol, "' tidak diubah.\n")
        }
      }
    } else {
      if (verbose) {
        cat("Parameter untuk kolom '", kol, "' tidak ditemukan. Kolom tidak diubah.\n")
      }
    }
  }

  if (verbose) {
    cat("Data berhasil dikembalikan ke skala asli menggunakan invers transformasi.\n")
  }

  # Mengembalikan hasil dalam format yang sesuai dengan input
  if (input_vektor) {
    hasil <- data_df[[1]]

    if (verbose) {
      cat(sprintf("\nKolom: %s\n", nama_vektor))

      # Hitung skewness jika package moments tersedia
      if (requireNamespace("moments", quietly = TRUE)) {
        skew_before <- round(moments::skewness(data_asli, na.rm = TRUE), 4)
        skew_after <- round(moments::skewness(hasil, na.rm = TRUE), 4)
      } else {
        skew_before <- NA
        skew_after <- NA
      }

      orig_stats <- data.frame(
        "Statistik" = c("Count", "Mean", "SD", "Min", "Max", "Median", "Skewness"),
        `Sebelum_invers` = c(
          length(data_asli),
          round(mean(data_asli, na.rm = TRUE), 4),
          round(sd(data_asli, na.rm = TRUE), 4),
          round(min(data_asli, na.rm = TRUE), 4),
          round(max(data_asli, na.rm = TRUE), 4),
          round(median(data_asli, na.rm = TRUE), 4),
          skew_before
        ),
        `Setelah_invers` = c(
          length(hasil),
          round(mean(hasil, na.rm = TRUE), 4),
          round(sd(hasil, na.rm = TRUE), 4),
          round(min(hasil, na.rm = TRUE), 4),
          round(max(hasil, na.rm = TRUE), 4),
          round(median(hasil, na.rm = TRUE), 4),
          skew_after
        )
      )

      print(orig_stats)

      if (length(hasil) > 0) {
        cat("\nData setelah invers transformasi")

        if (length(hasil) > 5) {
          cat(sprintf(" (menampilkan 5 elemen pertama dari %d):\n", length(hasil)))

          data_contoh <- data.frame(
            sebelum_invers = head(data_asli, 5),
            setelah_invers = head(hasil, 5)
          )
          print(data_contoh)
        } else {
          cat(":\n")

          data_contoh <- data.frame(
            sebelum_invers = data_asli,
            setelah_invers = hasil
          )
          print(data_contoh)
        }
      }
    }

    return(invisible(hasil))

  } else {
    if (verbose) {
      for (kol in kolom_list) {
        if(kol %in% names(params)) {
          sebelum <- data_asli[[kol]]
          sesudah <- data_df[[kol]]

          # Hitung skewness jika package moments tersedia
          if (requireNamespace("moments", quietly = TRUE)) {
            skew_before <- round(moments::skewness(sebelum, na.rm = TRUE), 4)
            skew_after <- round(moments::skewness(sesudah, na.rm = TRUE), 4)
          } else {
            skew_before <- NA
            skew_after <- NA
          }

          orig_stats <- data.frame(
            "Statistik" = c("Count", "Mean", "SD", "Min", "Max", "Median", "Skewness"),
            `Sebelum_invers` = c(
              length(sebelum),
              round(mean(sebelum, na.rm = TRUE), 4),
              round(sd(sebelum, na.rm = TRUE), 4),
              round(min(sebelum, na.rm = TRUE), 4),
              round(max(sebelum, na.rm = TRUE), 4),
              round(median(sebelum, na.rm = TRUE), 4),
              skew_before
            ),
            `Setelah_invers` = c(
              length(sesudah),
              round(mean(sesudah, na.rm = TRUE), 4),
              round(sd(sesudah, na.rm = TRUE), 4),
              round(min(sesudah, na.rm = TRUE), 4),
              round(max(sesudah, na.rm = TRUE), 4),
              round(median(sesudah, na.rm = TRUE), 4),
              skew_after
            ),
            row.names = NULL
          )

          cat(sprintf("\nKolom: %s\n", kol))
          print(orig_stats)

          # Print sampel data
          n_cetak <- min(5, length(sesudah))

          cat("\nData setelah invers transformasi")
          if (length(sesudah) > 5) {
            cat(sprintf(" (menampilkan 5 elemen pertama dari %d):\n", length(sesudah)))

            data_contoh <- data.frame(
              sebelum_invers = head(sebelum, 5),
              setelah_invers = head(sesudah, 5)
            )
            print(data_contoh)
          } else {
            cat(":\n")

            data_contoh <- data.frame(
              sebelum_invers = sebelum,
              setelah_invers = sesudah
            )
            print(data_contoh)
          }
        }
      }
    }

    return(invisible(data_df))
  }
}
