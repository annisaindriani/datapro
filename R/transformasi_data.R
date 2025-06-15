#' Transformasi nilai numerik dalam dataset menggunakan berbagai metode
#'
#' Fungsi ini menyediakan beberapa metode untuk transformasi pada data numerik, baik sebagai vektor
#' maupun sebagai kolom dalam data frame, yang umum digunakan dalam analisis statistik.
#'
#' @param data Vektor atau dataframe yang berisi data yang akan ditransformasi.
#' @param kolom Vektor karakter dari nama kolom yang akan ditransformasi. Jika NULL (default), semua kolom numerik akan ditransformasi.
#' @param metode Metode transformasi yang tersedia: "log", "sqrt", "boxcox", "yeojohnson", "exp", atau "power".
#' @param lambda Parameter untuk transformasi Box-Cox, Yeo-Johnson atau power (default = 2 untuk power)
#' @param invers_params Jika TRUE, mengembalikan parameter untuk transformasi invers (default = FALSE)
#' @param verbose Logis, apakah akan mencetak informasi detail (default TRUE)
#'
#' @return Jika invers_params = FALSE, mengembalikan data yang telah ditransformasi secara invisible.
#'         Jika invers_params = TRUE, mengembalikan list dengan komponen data dan params.
#'
#' @examples
#' # Membuat contoh data
#' x <- c(1, 2, 3, 4, 5)
#' x_transform <- transformasi_data(x, metode = "log")
#'
#' # Membuat contoh data
#' df <- data.frame(a = c(1, 2, 3, 4, 5), b = c(10, 20, 30, 40, 50))
#' df_transform <- transformasi_data(df, kolom = c("a", "b"), metode = "log")
#'
#' @seealso
#' \code{\link{plot_transformasi}} untuk fungsi visualisasi data sebelum dan sesudah transformasi.
#' \code{\link{invers_transformasi_data}} untuk fungsi mengembalikan data yang ditransformasi ke skala aslinya.
#' \code{\link{normalisasi_data}} untuk fungsi menerapkan normalisasi pada dataset.
#' \code{\link{reduksi_dimensi}} untuk fungsi melakukan reduksi dimensi pada dataset.
#'
#' @importFrom stats median IQR sd quantile
#' @importFrom methods is
#' @importFrom moments skewness
#'
#' @export

transformasi_data <- function(data, kolom = NULL, metode = c("log", "sqrt", "boxcox", "yeojohnson", "exp", "power"),
                              lambda = NULL, invers_params = FALSE, verbose = TRUE) {

  # Untuk validasi metode yang dipilih
  metode <- match.arg(metode)

  # Untuk menyimpan parameter untuk transformasi invers
  params_invers <- list(metode = metode)

  # Untuk cek tipe input data pengguna
  adalah_vektor <- is.vector(data) && !is.list(data)

  if (adalah_vektor) {
    # Input vektor
    nama_vektor <- deparse(substitute(data))

    if (!is.numeric(data)) {
      stop("Untuk input vektor, data harus berupa numerik.")
    }

    data_input <- data
    data_asli <- data
    nama_data <- nama_vektor
    params_invers$kolom <- nama_data

    # Transform berdasarkan metode
    if (metode == "log") {
      if (any(data_input <= 0, na.rm = TRUE)) {
        stop("Transformasi log tidak dapat diterapkan pada nilai yang kurang dari atau sama dengan 0.\n Gunakan metode transformasi lain untuk data dengan nilai negatif.")
      }

      data_transformasi <- log(data_input)
      params_invers$konstanta <- 0

    } else if (metode == "sqrt") {
      if (any(data_input < 0, na.rm = TRUE)) {
        stop("Transformasi akar kuadrat tidak dapat diterapkan pada nilai negatif.\n Gunakan metode transformasi lain untuk data dengan nilai negatif.")
      }

      data_transformasi <- sqrt(data_input)

    } else if (metode == "boxcox") {
      if (!requireNamespace("MASS", quietly = TRUE)) {
        stop("Package 'MASS' diperlukan untuk transformasi Box-Cox.")
      }

      if (any(data_input <= 0, na.rm = TRUE)) {
        stop("Transformasi Box-Cox tidak dapat diterapkan pada nilai negatif.\n Gunakan metode transformasi lain untuk data dengan nilai negatif.")
      }

      if (is.null(lambda)) {
        bc <- MASS::boxcox(data_input ~ 1, plotit = FALSE)
        lambda <- bc$x[which.max(bc$y)]

        if (verbose) {
          cat("Lambda optimal Box-Cox:", round(lambda, 4), "\n")
        }
      }

      params_invers$lambda <- lambda

      if (abs(lambda) < 1e-10) {
        data_transformasi <- log(data_input)
      } else {
        data_transformasi <- (data_input^lambda - 1) / lambda
      }

    } else if (metode == "yeojohnson") {
      if (!requireNamespace("car", quietly = TRUE)) {
        stop("Package 'car' diperlukan untuk transformasi Yeo-Johnson.")
      }

      if (is.null(lambda)) {
        pt <- car::powerTransform(data_input, family = "yjPower")
        lambda <- pt$lambda

        if (verbose) {
          cat("Lambda optimal Yeo-Johnson:", round(lambda, 4), "\n")
        }
      }

      params_invers$lambda <- lambda
      data_transformasi <- car::yjPower(data_input, lambda = lambda)

    } else if (metode == "exp") {
      data_transformasi <- exp(data_input)

    } else if (metode == "power") {
      if (is.null(lambda)) {
        lambda <- 2

        if (verbose) {
          cat("Menggunakan lambda default = 2 untuk transformasi pangkat.\n")
        }

      }

      params_invers$lambda <- lambda
      params_invers$negative_indices <- which(data_input < 0)

      data_transformasi <- data_input^lambda

    }

    if (verbose) {
      cat(sprintf("Data berhasil dilakukan transformasi menggunakan metode %s.\n", metode))

      # Print
      cat(sprintf("\nKolom: %s\n", nama_data))

      # Hitung skewness
      if (requireNamespace("moments", quietly = TRUE)) {
        skew_before <- round(moments::skewness(data_asli, na.rm = TRUE), 4)
        skew_after <- round(moments::skewness(data_transformasi, na.rm = TRUE), 4)
      } else {
        skew_before <- NA
        skew_after <- NA
      }

      orig_stats <- data.frame(
        "Statistik" = c("Count", "Mean", "SD", "Min", "Max", "Median", "Skewness"),
        `Sebelum` = c(
          length(data_asli),
          round(mean(data_asli, na.rm = TRUE), 4),
          round(sd(data_asli, na.rm = TRUE), 4),
          round(min(data_asli, na.rm = TRUE), 4),
          round(max(data_asli, na.rm = TRUE), 4),
          round(median(data_asli, na.rm = TRUE), 4),
          skew_before
        ),
        `Sesudah` = c(
          length(data_transformasi),
          round(mean(data_transformasi, na.rm = TRUE), 4),
          round(sd(data_transformasi, na.rm = TRUE), 4),
          round(min(data_transformasi, na.rm = TRUE), 4),
          round(max(data_transformasi, na.rm = TRUE), 4),
          round(median(data_transformasi, na.rm = TRUE), 4),
          skew_after
        )
      )

      print(orig_stats)

      # Print sampel data
      n_cetak <- min(5, length(data_transformasi))
      data_contoh <- data.frame(
        asli = head(data_asli, n_cetak),
        transformasi = head(data_transformasi, n_cetak)
      )

      cat("\nData hasil transformasi")
      if (length(data_transformasi) > 5) {
        cat(sprintf(" (menampilkan 5 elemen pertama dari %d):\n", length(data_transformasi)))
        print(head(data_contoh, 5))
      } else {
        cat(":\n")
        print(data_contoh)
      }
    }

    if (invers_params) {
      return(list(data = data_transformasi, params = params_invers))
    } else {
      return(invisible(data_transformasi))
    }

  } else {
    # Apabila input berupa data frame
    if (!is.data.frame(data)) {
      stop("Input harus berupa data frame, tibble, atau vektor numerik.")
    }

    data_asli <- data

    # Apabila dataset kosong
    if (nrow(data) == 0) {
      if (verbose) {
        cat("Dataset kosong. Tidak ada kolom yang ditransformasi.\n")
      }

      if (invers_params) {
        return(list(data = data_asli, params = list()))
      }

      return(invisible(data_asli))
    }

    # Untuk mengambil kolom numerik yang akan ditransformasi
    if (is.null(kolom)) {
      kolom_numerik <- sapply(data, is.numeric)
      if (!any(kolom_numerik)) {
        stop("Tidak ditemukan kolom numerik dalam dataset.")
      }

      kolom <- names(data)[kolom_numerik]
    } else {
      # Untuk validasi bahwa kolom tersebut ada dan numerik
      kolom_hilang <- setdiff(kolom, names(data))

      if (length(kolom_hilang) > 0) {
        stop("Kolom berikut tidak ada dalam data: ", paste(kolom_hilang, collapse = ", "))
      }

      non_numerik <- kolom[!sapply(data[kolom], is.numeric)]

      if (length(non_numerik) > 0) {
        stop("Kolom berikut bukan numerik: ", paste(non_numerik, collapse = ", "))
      }
    }

    # Untuk menyimpan kolom yang akan ditransformasi untuk inverse
    params_invers$kolom <- kolom
    params_list <- list()

    # Transform setiap kolom
    for (kol in kolom) {
      x <- data[[kol]]

      if (metode == "log") {
        if (any(x <= 0, na.rm = TRUE)) {
          stop("Transformasi log tidak dapat diterapkan pada nilai yang kurang dari atau sama dengan 0 di kolom '", kol, "'.\n Gunakan metode transformasi lain untuk data dengan nilai negatif.")
        }

        data[[kol]] <- log(x)
        params_list[[kol]] <- list(metode = metode, konstanta = 0)

      } else if (metode == "sqrt") {
        if (any(x < 0, na.rm = TRUE)) {
          stop("Transformasi akar kuadrat tidak dapat diterapkan pada nilai negatif di kolom '", kol, "'.\n Gunakan metode transformasi lain untuk data dengan nilai negatif.")
        }

        data[[kol]] <- sqrt(x)
        params_list[[kol]] <- list(metode = metode)

      } else if (metode == "boxcox") {
        if (!requireNamespace("MASS", quietly = TRUE)) {
          stop("Package 'MASS' diperlukan untuk transformasi Box-Cox.")
        }

        if (any(x <= 0, na.rm = TRUE)) {
          stop("Transformasi Box-Cox tidak dapat diterapkan pada nilai negatif di kolom '", kol, "'.\n Gunakan metode transformasi lain untuk data dengan nilai negatif.")
        }

        lambda_kol <- lambda

        if (is.null(lambda_kol)) {
          bc <- MASS::boxcox(x ~ 1, plotit = FALSE)
          lambda_kol <- bc$x[which.max(bc$y)]

          if (verbose) {
            cat("Lambda optimal Box-Cox untuk kolom '", kol, "':", round(lambda_kol, 4), "\n")
          }

        }

        if (abs(lambda_kol) < 1e-10) {
          data[[kol]] <- log(x)
        } else {
          data[[kol]] <- (x^lambda_kol - 1) / lambda_kol
        }
        params_list[[kol]] <- list(metode = metode, lambda = lambda_kol)

      } else if (metode == "yeojohnson") {
        if (!requireNamespace("car", quietly = TRUE)) {
          stop("Package 'car' diperlukan untuk transformasi Yeo-Johnson.")
        }

        lambda_kol <- lambda

        if (is.null(lambda_kol)) {
          pt <- car::powerTransform(x, family = "yjPower")
          lambda_kol <- pt$lambda

          if (verbose) {
            cat("Lambda optimal Yeo-Johnson untuk kolom '", kol, "':", round(lambda_kol, 4), "\n")
          }

        }

        data[[kol]] <- car::yjPower(x, lambda = lambda_kol)
        params_list[[kol]] <- list(metode = metode, lambda = lambda_kol)

      } else if (metode == "exp") {
        data[[kol]] <- exp(x)
        params_list[[kol]] <- list(metode = metode)

      } else if (metode == "power") {
        lambda_kol <- lambda

        if (is.null(lambda_kol)) {
          lambda_kol <- 2

          if (verbose) {
            cat("Menggunakan lambda default = 2 untuk transformasi pangkat kolom '", kol, "'.\n")
          }

        }

        data[[kol]] <- x^lambda_kol
        params_list[[kol]] <- list(metode = metode, lambda = lambda_kol,
                                   negative_indices = which(x < 0))

      }

      # Print untuk setiap kolom
      if (verbose) {
        cat(sprintf("Data berhasil dilakukan transformasi menggunakan metode %s.\n", metode))
        cat(sprintf("\nKolom: %s\n", kol))

        # Hitung skewness
        if (requireNamespace("moments", quietly = TRUE)) {
          skew_before <- round(moments::skewness(data_asli[[kol]], na.rm = TRUE), 4)
          skew_after <- round(moments::skewness(data[[kol]], na.rm = TRUE), 4)
        } else {
          skew_before <- NA
          skew_after <- NA
        }

        orig_stats <- data.frame(
          "Statistik" = c("Count", "Mean", "SD", "Min", "Max", "Median", "Skewness"),
          `Sebelum` = c(
            length(data_asli[[kol]]),
            round(mean(data_asli[[kol]], na.rm = TRUE), 4),
            round(sd(data_asli[[kol]], na.rm = TRUE), 4),
            round(min(data_asli[[kol]], na.rm = TRUE), 4),
            round(max(data_asli[[kol]], na.rm = TRUE), 4),
            round(median(data_asli[[kol]], na.rm = TRUE), 4),
            skew_before
          ),
          `Sesudah` = c(
            length(data[[kol]]),
            round(mean(data[[kol]], na.rm = TRUE), 4),
            round(sd(data[[kol]], na.rm = TRUE), 4),
            round(min(data[[kol]], na.rm = TRUE), 4),
            round(max(data[[kol]], na.rm = TRUE), 4),
            round(median(data[[kol]], na.rm = TRUE), 4),
            skew_after
          )
        )

        print(orig_stats)

        # Print sampel data
        n_cetak <- min(5, length(data[[kol]]))
        data_contoh <- data.frame(
          asli = head(data_asli[[kol]], n_cetak),
          transformasi = head(data[[kol]], n_cetak)
        )

        cat("\nData hasil transformasi")
        if (length(data[[kol]]) > 5) {
          cat(sprintf(" (menampilkan 5 elemen pertama dari %d):\n", length(data[[kol]])))
          print(head(data_contoh, 5))
        } else {
          cat(":\n")
          print(data_contoh)
        }
      }
    }

    if (invers_params) {
      return(list(data = data, params = params_list))
    } else {
      return(invisible(data))
    }
  }
}
