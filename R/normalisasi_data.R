#' Normalisasi atau penskalaan nilai numerik dalam dataset menggunakan berbagai metode
#'
#' Fungsi ini menyediakan beberapa metode untuk normalisasi atau penskalaan data numerik,
#' termasuk Min-Max scaling, Z-score standardization, dan robust scaling.
#'
#' @param data Sebuah data frame, tibble, atau vektor numerik yang akan dinormalisasikan atau diskalakan.
#' @param metode String karakter yang menentukan metode normalisasi:
#'   \itemize{
#'     \item "minmax": Penskalaan Min-Max ke rentang 0 hingga 1 (default).
#'     \item "z-score": Standardisasi Z-score.
#'     \item "robust": Penskalaan robust menggunakan median dan iqr.
#'   }
#' @param hapus_na Logis, apakah akan menghapus nilai yang hilang sebelum perhitungan (default TRUE).
#' @param center Untuk penskalaan robust, nilai center yang tersedia adalah "median" dan "mean" (default "median").
#' @param scale Untuk penskalaan robust, nilai scale yang tersedia adalah "iqr", "mad", dan "sd" (default "iqr").
#' @param kolom Vektor karakter dari nama kolom yang akan dinormalisasi. Jika NULL (default), normalisasi semua kolom numerik.
#' @param invers_params Logis, apakah akan mengembalikan parameter transformasi untuk kemungkinan mengembalikan nilai atau invers (default FALSE).
#'
#' @return Jika invers_params = FALSE (default), mengembalikan data yang telah dinormalisasi dalam format yang sama dengan input.
#'         Jika invers_params = TRUE, mengembalikan list dengan komponen:
#'         \item{data}{Data yang telah dinormalisasi}
#'         \item{params}{Parameter yang digunakan untuk normalisasi, berguna untuk normalisasi invers}
#'
#' @examples
#' # Membuat sampel data
#' data <- data.frame(
#'   x = c(1, 2, 3, 4, 5),
#'   y = c(10, 20, 30, 40, 50),
#'   z = c("a", "b", "c", "d", "e")
#' )
#'
#' # Normalisasi metode Min-Max ke semua kolom numerik
#' normalisasi_data(data)
#'
#' # Normalisasi metode Z-score hanya ke kolom 'x'
#' normalisasi_data(data, metode = "z-score", kolom = "x")
#'
#' # Normalisasi Robust dengan opsi center dan scale yang berbeda
#' normalisasi_data(data, metode = "robust", center = "mean", scale = "sd")
#'
#' # Normalisasi Robust dengan mengembalikan parameter
#' hasil <- normalisasi_data(data, metode = "robust", invers_params = TRUE)
#' hasil$data
#' hasil$params
#'
#' @seealso
#' \code{\link{plot_normalisasi}} untuk fungsi visualisasi data sebelum dan sesudah normalisasi.
#' \code{\link{invers_normalisasi_data}} untuk fungsi mengembalikan data yang dinormalisasi ke skala aslinya.
#' \code{\link{transformasi_data}} untuk fungsi menerapkan transformasi pada dataset.
#' \code{\link{reduksi_dimensi}} untuk fungsi melakukan reduksi dimensi pada dataset.
#'
#' @importFrom stats median IQR sd quantile
#' @importFrom methods is
#' @importFrom moments skewness
#' @importFrom utils head
#'
#' @export

normalisasi_data <- function(data,
                             metode = c("minmax", "z-score", "robust"),
                             hapus_na = TRUE,
                             center = c("median", "mean"),
                             scale = c("iqr", "sd", "mad"),
                             kolom = NULL,
                             invers_params = FALSE) {

  # Untuk validasi argumen yang diberikan pengguna sesuai dengan pilihan yang tersedia
  metode <- match.arg(metode)
  center <- match.arg(center)
  scale <- match.arg(scale)

  # Untuk menyimpan parameter untuk invers
  params_invers <- list(metode = metode)

  # Untuk cek tipe input data pengguna
  adalah_vektor <- is.vector(data) && !is.list(data)

  # Jika input berupa vektor
  if (adalah_vektor) {
    nama_vektor <- deparse(substitute(data))

    if (!is.numeric(data)) {
      stop("Untuk input vektor, data harus berupa numerik.")
    }

    data_input <- data
    data_asli <- data
    nama_data <- nama_vektor
    params_invers$kolom <- nama_data

    # Jika nilai pada suatu kolom bersifat konstan
    if (all(data_input == data_input[1], na.rm = TRUE)) {
      cat("Data memiliki nilai konstan. Tidak dilakukan normalisasi.\n")

      if (invers_params) {
        params_invers$metode <- "none"
        return(invisible(list(data = data_input, params = params_invers)))
      }

      return(invisible(data_input))
    }

    # Jika terdapat nilai hilang
    if (any(is.na(data_input)) && !hapus_na) {
      stop("Nilai yang hilang ditemukan dalam data. Set hapus_na = TRUE untuk melanjutkan atau terapkan metode penanganan nilai hilang terlebih dahulu.")
    }

    # Jika terdapat nilai hilang
    if (any(is.na(data_input)) && hapus_na) {
      cat("Terdapat nilai yang hilang dalam data. Normalisasi akan dilakukan dengan menghapus nilai yang hilang.\n")
    }

    if (metode == "minmax") {
      # Mengambil nilai minimum dan maksimum
      nilai_min <- min(data_input, na.rm = hapus_na)
      nilai_max <- max(data_input, na.rm = hapus_na)

      # Jika nilai minimum dan maksimum sama maka semua nilai menjadi sama
      if (nilai_min == nilai_max) {
        cat("Data memiliki rentang nol. Mengembalikan nilai konstan.\n")
        params_invers$min <- nilai_min
        params_invers$max <- nilai_max

        if (invers_params) {
          return(invisible(list(data = data_input, params = params_invers)))
        }
        return(invisible(data_input))
      }

      # Rumus Min-Max: (x - min) / (max - min)
      data_normalisasi <- (data_input - nilai_min) / (nilai_max - nilai_min)
      params_invers$min <- nilai_min
      params_invers$max <- nilai_max

    } else if (metode == "z-score") {
      # Menghitung nilai mean dan standard deviation
      nilai_mean <- mean(data_input, na.rm = hapus_na)
      nilai_sd <- stats::sd(data_input, na.rm = hapus_na)

      # Jika nilai standard deviation = 0 maka semua nilai menjadi 0 (karena pembagi menjadi 0)
      if (nilai_sd == 0) {
        cat("Data memiliki standard deviation nol. Mengembalikan nilai nol.\n")
        data_normalisasi <- rep(0, length(data_input))
        params_invers$mean <- nilai_mean
        params_invers$sd <- nilai_sd
      } else {
        # Rumus Z-score: (x - mean) / sd
        data_normalisasi <- (data_input - nilai_mean) / nilai_sd
        params_invers$mean <- nilai_mean
        params_invers$sd <- nilai_sd
      }

    } else if (metode == "robust") {
      # Memilih metode centering
      if (center == "median") {
        nilai_center <- stats::median(data_input, na.rm = hapus_na)
      } else {
        nilai_center <- mean(data_input, na.rm = hapus_na)
      }

      # Memilih metode scaling
      if (scale == "iqr") {
        nilai_scale <- stats::IQR(data_input, na.rm = hapus_na)
      } else if (scale == "sd") {
        nilai_scale <- stats::sd(data_input, na.rm = hapus_na)
      } else {
        nilai_scale <- stats::mad(data_input, na.rm = hapus_na)
      }

      # Jika nilai scale 0 maka semua nilai menjadi 0 (karena pembagi menjadi 0)
      if (nilai_scale == 0) {
        cat("Data memiliki ", scale, " nol. Mengembalikan nilai nol.\n")
        data_normalisasi <- rep(0, length(data_input))
        params_invers$center <- nilai_center
        params_invers$scale <- nilai_scale
        params_invers$tipe_center <- center
        params_invers$tipe_scale <- scale
      } else {
        # Rumus Robust: (x - center) / scale
        data_normalisasi <- (data_input - nilai_center) / nilai_scale
        params_invers$center <- nilai_center
        params_invers$scale <- nilai_scale
        params_invers$tipe_center <- center
        params_invers$tipe_scale <- scale
      }

    }

    cat(sprintf("Data berhasil dilakukan normalisasi menggunakan metode %s.\n", metode))

    # Print statistik dan sampel data
    cat(sprintf("\nKolom: %s\n", nama_data))

    orig_stats <- data.frame(
      "Statistik" = c("Count", "Mean", "SD", "Min", "Max", "Median", "Skewness"),
      `Sebelum` = c(
        length(data_asli),
        round(mean(data_asli, na.rm = TRUE), 4),
        round(sd(data_asli, na.rm = TRUE), 4),
        round(min(data_asli, na.rm = TRUE), 4),
        round(max(data_asli, na.rm = TRUE), 4),
        round(median(data_asli, na.rm = TRUE), 4),
        round(skewness(data_asli, na.rm = TRUE), 4)
      ),
      `Sesudah` = c(
        length(data_normalisasi),
        round(mean(data_normalisasi, na.rm = TRUE), 4),
        round(sd(data_normalisasi, na.rm = TRUE), 4),
        round(min(data_normalisasi, na.rm = TRUE), 4),
        round(max(data_normalisasi, na.rm = TRUE), 4),
        round(median(data_normalisasi, na.rm = TRUE), 4),
        round(skewness(data_normalisasi, na.rm = TRUE), 4)
      )
    )

    print(orig_stats)

    # Print sampel data
    n_cetak <- min(5, length(data_normalisasi))

    data_contoh <- data.frame(
      asli = head(data_asli, n_cetak),
      normalisasi = head(data_normalisasi, n_cetak)
    )

    cat("\nData hasil normalisasi")

    if (length(data_normalisasi) > 5) {
      cat(sprintf(" (menampilkan 5 elemen pertama dari %d):\n", length(data_normalisasi)))
      print(head(data_contoh, 5))
    } else {
      cat(":\n")
      print(data_contoh)
    }

    if (invers_params) {
      return(invisible(list(data = data_normalisasi, params = params_invers)))
    } else {
      return(invisible(data_normalisasi))
    }

  } else {
    # Untuk input data frame
    if (!is.data.frame(data)) {
      stop("Input harus berupa data frame, tibble, atau vektor numerik.")
    }

    data_asli <- data

    # Apabila dataset kosong
    if (nrow(data) == 0) {
      cat("Dataset kosong. Tidak ada kolom yang dilakukan normalisasi.\n")

      if (invers_params) {
        return(invisible(list(data = data_asli, params = list())))
      }

      return(invisible(data_asli))
    }

    # Mengambil kolom numerik yang akan dinormalisasi jika user tidak memasukkan ke parameter
    if (is.null(kolom)) {
      kolom_numerik <- sapply(data, is.numeric)

      if (!any(kolom_numerik)) {
        stop("Tidak ditemukan kolom numerik dalam dataset.")
      }

      kolom <- names(data)[kolom_numerik]
    } else {
      # Validasi bahwa kolom tersebut ada dan hanya mengambil kolom numerik jika pengguna menentukan
      kolom_hilang <- setdiff(kolom, names(data))

      if (length(kolom_hilang) > 0) {
        stop("Kolom berikut tidak ada dalam data: ",
             paste(kolom_hilang, collapse = ", "))
      }

      non_numerik <- kolom[!sapply(data[kolom], is.numeric)]

      if (length(non_numerik) > 0) {
        stop("Kolom berikut bukan merupakan numerik: ",
             paste(non_numerik, collapse = ", "))
      }
    }

    # Untuk menyimpan kolom yang akan dinormalisasi untuk inverse
    params_invers$kolom <- kolom
    params_list <- list()

    for (kol in kolom) {
      x <- data[[kol]]

      # Jika nilai pada suatu kolom bersifat konstan
      if (all(x == x[1], na.rm = TRUE)) {
        cat("Kolom '", kol, "' memiliki nilai konstan. Tidak dilakukan normalisasi.\n")
        params_list[[kol]] <- list(metode = "none")
        next
      }

      # Jika terdapat nilai hilang
      if (any(is.na(x)) && !hapus_na) {
        stop("Nilai yang hilang ditemukan dalam kolom '", kol,
             "'. Set hapus_na = TRUE untuk melanjutkan atau terapkan metode penanganan nilai hilang terlebih dahulu.")
      }

      # Jika terdapat nilai hilang
      if (any(is.na(x)) && hapus_na) {
        cat("Terdapat nilai yang hilang dalam data. Normalisasi akan dilakukan dengan menghapus nilai yang hilang.\n")
      }

      # Untuk normalisasi per kolom
      if (metode == "minmax") {
        # Mengambil nilai minimum dan maksimum
        nilai_min <- min(x, na.rm = hapus_na)
        nilai_max <- max(x, na.rm = hapus_na)

        # Jika nilai minimum dan maksimum sama maka semua nilai menjadi sama
        if (nilai_min == nilai_max) {
          cat("Kolom '", kol, "' memiliki rentang nol. Mengembalikan nilai konstan.\n")
          params_list[[kol]] <- list(metode = metode, min = nilai_min, max = nilai_max)
          next
        }

        # Rumus Min-Max: (x - min) / (max - min)
        data[[kol]] <- (x - nilai_min) / (nilai_max - nilai_min)
        params_list[[kol]] <- list(metode = metode, min = nilai_min, max = nilai_max)

      } else if (metode == "z-score") {
        # Menghitung nilai mean dan standard deviation
        nilai_mean <- mean(x, na.rm = hapus_na)
        nilai_sd <- stats::sd(x, na.rm = hapus_na)

        # Jika nilai standard deviation = 0 maka semua nilai menjadi 0 (karena pembagi menjadi 0)
        if (nilai_sd == 0) {
          cat("Kolom '", kol, "' memiliki standard deviation nol. Mengembalikan nilai nol.\n")
          data[[kol]] <- rep(0, length(x))
          params_list[[kol]] <- list(metode = metode, mean = nilai_mean, sd = nilai_sd)
          next
        }

        # Rumus Z-score: (x - mean) / sd
        data[[kol]] <- (x - nilai_mean) / nilai_sd
        params_list[[kol]] <- list(metode = metode, mean = nilai_mean, sd = nilai_sd)

      } else if (metode == "robust") {
        # Memilih metode centering
        if (center == "median") {
          nilai_center <- stats::median(x, na.rm = hapus_na)
        } else {
          nilai_center <- mean(x, na.rm = hapus_na)
        }

        # Memilih metode scaling
        if (scale == "iqr") {
          nilai_scale <- stats::IQR(x, na.rm = hapus_na)
        } else if (scale == "sd") {
          nilai_scale <- stats::sd(x, na.rm = hapus_na)
        } else {
          nilai_scale <- stats::mad(x, na.rm = hapus_na)
        }

        # Jika nilai scale 0 maka semua nilai menjadi 0 (karena pembagi menjadi 0)
        if (nilai_scale == 0) {
          cat("Kolom '", kol, "' memiliki ", scale, " nol. Mengembalikan nilai nol.\n")
          data[[kol]] <- rep(0, length(x))
          params_list[[kol]] <- list(metode = metode, center = nilai_center, scale = nilai_scale,
                                     tipe_center = center, tipe_scale = scale)
          next
        }

        # Rumus Robust: (x - center) / scale
        data[[kol]] <- (x - nilai_center) / nilai_scale
        params_list[[kol]] <- list(metode = metode, center = nilai_center, scale = nilai_scale,
                                   tipe_center = center, tipe_scale = scale)

      }
    }

    cat(sprintf("Data berhasil dilakukan normalisasi menggunakan metode %s.\n", metode))

    # Print hasil untuk setiap kolom yang dinormalisasi
    for (kol in kolom) {
      if (params_list[[kol]]$metode != "none") {
        sebelum <- data_asli[[kol]]
        sesudah <- data[[kol]]

        cat(sprintf("\nKolom: %s\n", kol))

        orig_stats <- data.frame(
          "Statistik" = c("Count", "Mean", "SD", "Min", "Max", "Median", "Skewness"),
          `Sebelum` = c(
            length(sebelum),
            round(mean(sebelum, na.rm = TRUE), 4),
            round(sd(sebelum, na.rm = TRUE), 4),
            round(min(sebelum, na.rm = TRUE), 4),
            round(max(sebelum, na.rm = TRUE), 4),
            round(median(sebelum, na.rm = TRUE), 4),
            round(skewness(sebelum, na.rm = TRUE), 4)
          ),
          `Sesudah` = c(
            length(sesudah),
            round(mean(sesudah, na.rm = TRUE), 4),
            round(sd(sesudah, na.rm = TRUE), 4),
            round(min(sesudah, na.rm = TRUE), 4),
            round(max(sesudah, na.rm = TRUE), 4),
            round(median(sesudah, na.rm = TRUE), 4),
            round(skewness(sesudah, na.rm = TRUE), 4)
          )
        )

        print(orig_stats)

        # Print sampel data
        n_cetak <- min(5, length(sesudah))

        data_contoh <- data.frame(
          asli = head(sebelum, n_cetak),
          normalisasi = head(sesudah, n_cetak)
        )

        cat("\nData hasil normalisasi")

        if (length(sesudah) > 5) {
          cat(sprintf(" (menampilkan 5 elemen pertama dari %d):\n", length(sesudah)))
          print(head(data_contoh, 5))
        } else {
          cat(":\n")
          print(data_contoh)
        }
      }
    }

    if (invers_params) {
      return(invisible(list(data = data, params = params_list)))
    } else {
      return(invisible(data))
    }

  }
}
