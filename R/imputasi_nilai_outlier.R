#' Imputasi nilai outlier dalam dataset
#'
#' Fungsi ini mengganti nilai outlier dalam vektor numerik atau kolom data frame
#' dengan nilai imputasi menggunakan metode seperti mean, median, atau modus.
#' Nilai outlier dideteksi menggunakan metode Z-Score, IQR, atau persentil sebelum imputasi.
#'
#' @param data Vektor numerik atau dataframe.
#' @param kolom Jika data adalah dataframe, nama kolom yang akan diproses.
#' @param metode Metode yang digunakan untuk deteksi nilai outlier. Pilihan yang tersedia:
#'    \itemize{
#'          \item "z-score": Menggunakan standar deviasi dari mean.
#'          \item "iqr": Menggunakan rentang interkuartil.
#'          \item "percentile": Menggunakan ambang batas persentil khusus.
#'    }
#' @param ambang_batas Ambang batas untuk Z-Score (default 2) atau IQR (default 1.5).
#' @param persentil_bawah Ambang batas persentil bawah (default 0.05).
#' @param persentil_atas Ambang batas persentil atas (default 0.95).
#' @param metode_imputasi Metode yang digunakan untuk imputasi. Pilihan yang tersedia:
#'    \itemize{
#'      \item "mean": Mengganti nilai outlier dengan rata-rata kolom.
#'      \item "median": Mengganti nilai outlier dengan median kolom.
#'      \item "modus": Mengganti nilai outlier dengan modus kolom.
#'    }
#' @param round Logis, apakah akan membulatkan nilai numerik yang diimputasi (default FALSE)
#' @param digit Jumlah tempat desimal untuk pembulatan nilai imputasi (default 0).
#'
#' @return Jika input adalah vektor, mengembalikan vektor dengan nilai terimputasi.
#'         Jika input adalah dataframe, mengembalikan dataframe dengan nilai terimputasi.
#'
#' @examples
#' # Membuat sampel data
#' x <- c(1, 2, 3, 100, 4, 5, 200)
#' x_imputasi <- imputasi_nilai_outlier(x, metode = "z-score", metode_imputasi = "mean")
#' x_imputasi
#'
#' # Membuat sampel data
#' data <- data.frame(
#'   id = 1:7,
#'   nilai = c(1, 2, 3, 100, 4, 5, 200)
#' )
#'
#' df_imputasi <- imputasi_nilai_outlier(data, kolom = "nilai",
#'                                       metode = "iqr",
#'                                       metode_imputasi = "median")
#' df_imputasi
#'
#' @seealso
#' \code{\link{imputasi_nilai_outlier_lanjutan}} untuk fungsi imputasi nilai outlier dalam dataset menggunakan metode lanjutan.
#' \code{\link{plot_nilai_outlier}} untuk fungsi visualisasi data setelah menangani nilai outlier.
#' \code{\link{deteksi_nilai_outlier}} untuk fungsi identifikasi nilai outlier dalam dataset.
#' \code{\link{hapus_nilai_outlier}} untuk fungsi menghapus nilai outlier dari dataset.
#'
#' @importFrom stats median
#' @importFrom moments skewness
#'
#' @export

imputasi_nilai_outlier <- function(data, kolom = NULL,
                                   metode = "z-score",
                                   ambang_batas = NULL,
                                   persentil_bawah = 0.05,
                                   persentil_atas = 0.95,
                                   metode_imputasi = "mean",
                                   round = FALSE,
                                   digit = 0) {

  # Untuk melihat apakah input sudah sesuai
  if (!metode_imputasi %in% c("mean", "median", "modus")) {
    stop("metode_imputasi harus salah satu dari 'mean', 'median', atau 'modus'.")
  }

  # Untuk menghitung modus
  hitung_modus <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  # Apabila input berupa dataframe
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

    # Untuk menyimpan data non-outlier untuk perhitungan imputasi
    nilai_non_outlier <- data[[kolom]][!info_outlier$data$adalah_nilai_outlier]

    if (metode_imputasi == "mean") {
      nilai_imputasi <- mean(nilai_non_outlier, na.rm = TRUE)
    } else if (metode_imputasi == "median") {
      nilai_imputasi <- stats::median(nilai_non_outlier, na.rm = TRUE)
    } else {
      nilai_imputasi <- hitung_modus(nilai_non_outlier)
    }

    # Untuk membulatkan hasil jika parameter round is TRUE
    if (round && !is.na(nilai_imputasi)) {
      nilai_imputasi <- round(nilai_imputasi, digit)
    }

    # Untuk melakukan imputasi
    data_terimputasi <- data
    data_terimputasi[info_outlier$data$adalah_nilai_outlier, kolom] <- nilai_imputasi

    # Untuk menghitung persentase nilai outlier yang di-imputasi
    baris_terimputasi <- sum(info_outlier$data$adalah_nilai_outlier)
    persen_terimputasi <- round(baris_terimputasi / nrow(data) * 100, 1)

    if (length(baris_terimputasi) == 0) {
      cat("\nTidak ditemukan nilai outlier yang terdeteksi dalam kolom yang ditentukan.\n")
      return(invisible(data))
    } else {
      cat(sprintf("\nMelakukan imputasi %d baris dengan nilai outlier dari total %d baris (%.1f%%)\n",
                  baris_terimputasi, nrow(data), persen_terimputasi))
      cat(sprintf("Dimensi akhir: %d baris x %d kolom\n",
                  nrow(data_terimputasi), ncol(data_terimputasi)))

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
          length(data_terimputasi[[kolom]]),
          round(mean(data_terimputasi[[kolom]], na.rm = TRUE), 4),
          round(sd(data_terimputasi[[kolom]], na.rm = TRUE), 4),
          round(min(data_terimputasi[[kolom]], na.rm = TRUE), 4),
          round(max(data_terimputasi[[kolom]], na.rm = TRUE), 4),
          round(median(data_terimputasi[[kolom]], na.rm = TRUE), 4),
          round(skewness(data_terimputasi[[kolom]], na.rm = TRUE), 4)
        )
      )

      print(orig_stats)

      return(invisible(data_terimputasi))
    }

  } else {
    # Apabila input berupa vektor
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

    # Untuk menyimpan data non-outlier untuk perhitungan imputasi
    nilai_non_outlier <- data[!info_outlier$data$adalah_nilai_outlier]

    if (metode_imputasi == "mean") {
      nilai_imputasi <- mean(nilai_non_outlier, na.rm = TRUE)
    } else if (metode_imputasi == "median") {
      nilai_imputasi <- stats::median(nilai_non_outlier, na.rm = TRUE)
    } else {
      nilai_imputasi <- hitung_modus(nilai_non_outlier)
    }

    # Untuk membulatkan hasil jika parameter round is TRUE
    if (round && !is.na(nilai_imputasi)) {
      nilai_imputasi <- round(nilai_imputasi, digit)
    }

    # Untuk melakukan imputasi
    data_terimputasi <- data
    data_terimputasi[info_outlier$data$adalah_nilai_outlier] <- nilai_imputasi

    # Untuk menghitung persentase nilai outlier yang di-imputasi
    nilai_terimputasi <- sum(info_outlier$data$adalah_nilai_outlier)
    persen_terimputasi <- round(nilai_terimputasi / length(data) * 100, 1)

    if (nilai_terimputasi == 0) {
      cat("\nVektor tidak mengandung outlier sehingga tidak ada baris yang dilakukan imputasi.\n")
      return(invisible(data))
    } else {
      cat(sprintf("\nMelakukan imputasi %d baris dengan nilai outlier dari total %d baris (%.1f%%)\n",
                  nilai_terimputasi, length(data), persen_terimputasi))
      cat(sprintf("Panjang vektor akhir: %d\n", length(data_terimputasi)))

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
          length(data_terimputasi),
          round(mean(data_terimputasi, na.rm = TRUE), 4),
          round(sd(data_terimputasi, na.rm = TRUE), 4),
          round(min(data_terimputasi, na.rm = TRUE), 4),
          round(max(data_terimputasi, na.rm = TRUE), 4),
          round(median(data_terimputasi, na.rm = TRUE), 4),
          round(skewness(data_terimputasi), 4)
        )
      )

      print(orig_stats)

      return(invisible(data_terimputasi))
    }
  }
}
