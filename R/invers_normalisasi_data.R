#' Mengembalikan data yang telah dinormalisasi ke skala aslinya
#'
#' Fungsi ini mengembalikan data yang telah dinormalisasi menggunakan fungsi normalisasi_data
#' kembali ke skala dan distribusi aslinya. Fungsi ini memerlukan parameter yang disimpan
#' saat melakukan normalisasi dengan opsi invers_params = TRUE.
#'
#' @param data Sebuah data frame, tibble, atau vektor numerik yang telah dinormalisasi dan akan dikembalikan ke skala asli.
#' @param params Parameter yang dihasilkan dari fungsi normalisasi_data dengan opsi invers_params = TRUE.
#'   Parameter ini berisi informasi transformasi yang diperlukan untuk melakukan invers.
#' @param verbose Logis, apakah akan mencetak informasi tentang proses invers (default TRUE).
#'
#' @return Data dalam format yang sama dengan input, tetapi telah dikembalikan ke skala aslinya.
#'   Jika input berupa vektor, mengembalikan vektor. Jika input berupa data frame, mengembalikan data frame.
#'
#' @examples
#' # Membuat sampel data
#' data_asli <- c(1, 2, 3, 4, 5, 10, 15, 20)
#'
#' # Melakukan normalisasi dengan menyimpan parameter
#' hasil_norm <- normalisasi_data(data_asli, metode = "minmax", invers_params = TRUE)
#' data_normalized <- hasil_norm$data
#' params <- hasil_norm$params
#'
#' # Melakukan invers
#' data_kembali <- invers_normalisasi_data(data_normalized, params)
#'
#' # Melakukan validasi hasilnya sama dengan data asli
#' all.equal(data_asli, data_kembali, tolerance = 1e-10)
#'
#' # Membuat contoh data
#' df <- data.frame(
#'   x = c(1, 2, 3, 4, 5),
#'   y = c(10, 20, 30, 40, 50),
#'   z = c("a", "b", "c", "d", "e")
#' )
#'
#' # Melakukan normalisasi Z-score dengan parameter
#' hasil_zscore <- normalisasi_data(df, metode = "z-score", kolom = "x", invers_params = TRUE)
#' df_normalized <- hasil_zscore$data
#' params_zscore <- hasil_zscore$params
#'
#' # Melakukan invers
#' df_kembali <- invers_normalisasi_data(df_normalized, params_zscore)
#'
#' @seealso
#' \code{\link{normalisasi_data}} untuk fungsi normalisasi atau penskalaan dataset.
#' \code{\link{plot_normalisasi}} untuk fungsi visualisasi data sebelum dan sesudah normalisasi.
#' \code{\link{transformasi_data}} untuk fungsi menerapkan transformasi pada dataset.
#'
#' @importFrom moments skewness
#' @importFrom stats setNames
#' @importFrom utils head
#'
#' @export

invers_normalisasi_data <- function(data, params, verbose = TRUE) {

  # Untuk validasi input
  if(is.null(params) || length(params) == 0) {
    stop("Parameter invers tidak tersedia. Pastikan normalisasi dilakukan dengan opsi invers_params = TRUE.")
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
      # Single parameter untuk satu kolom
      if ("kolom" %in% names(params)) {
        kolom_list <- params$kolom
      } else {
        # Fallback: gunakan kolom numerik pertama
        kolom_numerik <- names(data_df)[sapply(data_df, is.numeric)]

        if (length(kolom_numerik) == 0) {
          stop("Tidak ditemukan kolom numerik dalam data frame.")
        }

        kolom_list <- kolom_numerik[1]

        if (verbose) {
          cat("Peringatan: Nama kolom tidak ditemukan dalam parameter. Menggunakan kolom numerik pertama:", kolom_list, "\n")
        }
      }

      if (length(kolom_list) == 1) {
        params <- setNames(list(params), kolom_list)
      } else {
        temp_params <- params
        params <- list()

        for (kol in kolom_list) {
          params[[kol]] <- temp_params
        }
      }
    } else {
      kolom_list <- names(params)
    }

    input_vektor <- FALSE
  }

  # Apabila dataset kosong
  if (nrow(data_df) == 0) {
    if (verbose) {
      cat("Dataset kosong. Tidak ada kolom yang dilakukan invers normalisasi.\n")
    }
    return(invisible(data))
  }

  # Proses invers untuk setiap kolom
  for(kol in kolom_list) {
    if(kol %in% names(params)) {
      param <- params[[kol]]
      x <- data_df[[kol]]

      # Skip jika metode adalah "none" (data konstan)
      if(param$metode == "none") {
        if (verbose) {
          cat("Kolom '", kol, "' tidak dilakukan invers karena memiliki nilai konstan.\n")
        }
        next
      }

      if(param$metode == "minmax") {
        # invers Min-Max: x_original = x_normalized * (max - min) + min
        data_df[[kol]] <- x * (param$max - param$min) + param$min

      } else if(param$metode == "z-score") {
        # invers Z-score: x_original = x_standardized * sd + mean
        data_df[[kol]] <- x * param$sd + param$mean

      } else if(param$metode == "robust") {
        # invers Robust: x_original = x_scaled * scale + center
        data_df[[kol]] <- x * param$scale + param$center

      } else if(param$metode == "quantile") {
        # invers Quantile normalization
        if (verbose) {
          cat("Invers tidak tersedia untuk metode Quantile normalization.\nMengembalikan data asli yang belum dilakukan normalisasi.\n")
        }

        if(!is.null(param$data_asli)) {
          data_df[[kol]] <- param$data_asli
        } else {
          cat("Data asli tidak tersimpan.\nMengembalikan data yang telah dinormalisasi.\n")
          data_df[[kol]] <- x
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
    cat("Data berhasil dikembalikan ke skala asli menggunakan invers normalisasi.\n")
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
        `Sebelum invers` = c(
          length(data_asli),
          round(mean(data_asli, na.rm = TRUE), 4),
          round(sd(data_asli, na.rm = TRUE), 4),
          round(min(data_asli, na.rm = TRUE), 4),
          round(max(data_asli, na.rm = TRUE), 4),
          round(median(data_asli, na.rm = TRUE), 4),
          skew_before
        ),
        `Setelah invers` = c(
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

      # Print sampel data
      n_cetak <- min(5, length(hasil))

      data_contoh <- data.frame(
        sebelum_invers = head(data_asli, n_cetak),
        setelah_invers = head(hasil, n_cetak)
      )

      cat("\nData setelah invers normalisasi")

      if (length(hasil) > 5) {
        cat(sprintf(" (menampilkan 5 elemen pertama dari %d):\n", length(hasil)))
        print(head(data_contoh, 5))
      } else {
        cat(":\n")
        print(data_contoh)
      }
    }

    return(invisible(hasil))

  } else {
    if (verbose) {
      for (kol in kolom_list) {
        if(kol %in% names(params) && params[[kol]]$metode != "none") {
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
            `Sebelum invers` = c(
              length(sebelum),
              round(mean(sebelum, na.rm = TRUE), 4),
              round(sd(sebelum, na.rm = TRUE), 4),
              round(min(sebelum, na.rm = TRUE), 4),
              round(max(sebelum, na.rm = TRUE), 4),
              round(median(sebelum, na.rm = TRUE), 4),
              skew_before
            ),
            `Setelah invers` = c(
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

          data_contoh <- data.frame(
            sebelum_invers = head(sebelum, n_cetak),
            setelah_invers = head(sesudah, n_cetak)
          )

          cat("\nData setelah invers normalisasi")

          if (length(sesudah) > 5) {
            cat(sprintf(" (menampilkan 5 elemen pertama dari %d):\n", length(sesudah)))
            print(head(data_contoh, 5))
          } else {
            cat(":\n")
            print(data_contoh)
          }
        }
      }
    }

    return(invisible(data_df))
  }
}
