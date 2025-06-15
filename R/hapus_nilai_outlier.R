#' Hapus nilai outlier dari dataset
#'
#' Fungsi ini menghapus nilai outlier dari vektor numerik atau kolom data frame
#' menggunakan metode deteksi seperti Z-Score, IQR (Interquartile Range), atau Percentile
#'
#' @param data Vektor numerik dari nilai-nilai atau dataframe.
#' @param kolom Jika data berupa dataframe, nama kolom yang akan diproses.
#' @param metode Metode yang digunakan untuk deteksi nilai outlier. Pilihan yang tersedia:
#'    \itemize{
#'          \item "z-score": Menggunakan standar deviasi dari mean (default).
#'          \item "iqr": Menggunakan rentang interkuartil.
#'          \item "percentile": Menggunakan ambang batas persentil khusus.
#'    }
#' @param ambang_batas Untuk metode Z-Score, nilai ambang batas (default adalah 3).
#'                 Untuk metode IQR, pengali untuk IQR (default adalah 1.5).
#' @param persentil_bawah Ambang batas bawah untuk metode percentile (default adalah 0.05).
#' @param persentil_atas Ambang batas atas untuk metode percentile (default adalah 0.95).
#'
#' @return Jika input berupa vektor, mengembalikan vektor yang telah dibersihkan.
#'         Jika input berupa dataframe, mengembalikan dataframe dengan nilai outlier yang telah dihapus.
#'
#' @examples
#' # Membuat sampel data
#' x <- c(1, 2, 3, 100, 4, 5, 200)
#' x_bersih <- hapus_nilai_outlier(x, metode = "z-score")
#' x_bersih
#'
#' # Membuat sampel data
#' data <- data.frame(
#'   id = 1:7,
#'   nilai = c(1, 2, 3, 100, 4, 5, 200)
#' )
#'
#' df_bersih <- hapus_nilai_outlier(data, kolom = "nilai", metode = "iqr")
#' df_bersih
#'
#' @seealso
#' \code{\link{deteksi_nilai_outlier}} untuk fungsi identifikasi nilai outlier dari dataset.
#' \code{\link{imputasi_nilai_outlier}} untuk fungsi imputasi nilai outlier dalam dataset menggunakan metode dasar.
#' \code{\link{imputasi_nilai_outlier_lanjutan}} untuk fungsi imputasi nilai outlier dalam dataset menggunakan metode lanjutan.
#' \code{\link{hapus_nilai_hilang}} untuk fungsi menghapus nilai yang hilang dalam dataset.
#' \code{\link{hapus_duplikat}} untuk fungsi menghapus baris data duplikat dari dataset.
#'
#' @import moments
#' @export

hapus_nilai_outlier <- function(data, kolom = NULL,
                                metode = "z-score",
                                ambang_batas = NULL,
                                persentil_bawah = 0.05,
                                persentil_atas = 0.95) {

  # Untuk melihat apakah input sudah sesuai
  if (is.data.frame(data)) {
    if (is.null(kolom)) {
      stop("Nama kolom harus ditentukan ketika input berupa dataframe.")
    }
    if (!kolom %in% names(data)) {
      stop("Kolom yang ditentukan tidak ditemukan dalam dataframe.")
    }
    if (!is.numeric(data[[kolom]])) {
      stop("Kolom yang ditentukan harus berupa numerik.")
    }

    # Untuk mengecek nilai outlier dengan fungsi deteksi_outlier
    info_nilai_outlier <- deteksi_nilai_outlier(data[[kolom]],
                                          metode = metode,
                                          ambang_batas = ambang_batas,
                                          persentil_bawah = persentil_bawah,
                                          persentil_atas = persentil_atas)

    # Untuk menghapus nilai outlier (hanya baris yang tidak NA dan bukan outlier)
    indeks_untuk_dihapus <- which(!is.na(info_nilai_outlier$data$adalah_nilai_outlier) &
                                    info_nilai_outlier$data$adalah_nilai_outlier == TRUE)

    if (length(indeks_untuk_dihapus) == 0) {
      cat("\nTidak ada kolom terpilih yang mengandung outlier sehingga tidak ada baris yang dihapus.\n")
      return(invisible(data))
    } else {

      data_bersih <- data[-indeks_untuk_dihapus, ]

      # Untuk menghitung persentase nilai outlier yang dihapus
      baris_dihapus <- nrow(data) - nrow(data_bersih)
      persen_dihapus <- round(baris_dihapus / nrow(data) * 100, 1)

      cat(sprintf("\nMenghapus %d baris dengan nilai outlier dari total %d baris (%.1f%%)\n",
                  baris_dihapus, nrow(data), persen_dihapus))
      cat(sprintf("Dimensi akhir: %d baris x %d kolom\n",
                  nrow(data_bersih), ncol(data_bersih)))

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
          length(data_bersih[[kolom]]),
          round(mean(data_bersih[[kolom]], na.rm = TRUE), 4),
          round(sd(data_bersih[[kolom]], na.rm = TRUE), 4),
          round(min(data_bersih[[kolom]], na.rm = TRUE), 4),
          round(max(data_bersih[[kolom]], na.rm = TRUE), 4),
          round(median(data_bersih[[kolom]], na.rm = TRUE), 4),
          round(skewness(data_bersih[[kolom]], na.rm = TRUE), 4)
        )
      )

      print(orig_stats)

      return(invisible(data_bersih))
    }
  } else {
    # Apabila input berupa vektor
    if (!is.numeric(data)) {
      stop("Vektor input harus berupa numerik.")
    }

    # Untuk mengecek nilai outlier dengan fungsi deteksi_outlier
    info_nilai_outlier <- deteksi_nilai_outlier(data,
                                          metode = metode,
                                          ambang_batas = ambang_batas,
                                          persentil_bawah = persentil_bawah,
                                          persentil_atas = persentil_atas)

    # Untuk menghapus nilai outlier (hanya nilai yang tidak NA dan bukan outlier)
    indeks_untuk_dihapus <- which(info_nilai_outlier$data$adalah_nilai_outlier == TRUE)
    data_bersih <- data[-indeks_untuk_dihapus]

    # Untuk menghitung persentase nilai outlier
    nilai_dihapus <- length(indeks_untuk_dihapus)
    persen_dihapus <- round(nilai_dihapus / length(data) * 100, 1)

    if (nilai_dihapus == 0) {
      cat("\nVektor tidak mengandung outlier sehingga tidak ada baris yang dihapus.\n")
      return(invisible(data))
    } else {
      cat(sprintf("\nMenghapus %d baris dengan nilai outlier dari total %d baris (%.1f%%)\n",
                  nilai_dihapus, length(data), persen_dihapus))
      cat(sprintf("Panjang vektor akhir: %d\n", length(data_bersih)))

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
          length(data_bersih),
          round(mean(data_bersih, na.rm = TRUE), 4),
          round(sd(data_bersih, na.rm = TRUE), 4),
          round(min(data_bersih, na.rm = TRUE), 4),
          round(max(data_bersih, na.rm = TRUE), 4),
          round(median(data_bersih, na.rm = TRUE), 4),
          round(skewness(data_bersih), 4)
        )
      )

      print(orig_stats)

      return(invisible(data_bersih))
    }
  }
}
