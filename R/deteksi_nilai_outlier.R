#' Deteksi nilai outlier dalam sebuah dataset
#'
#' Fungsi ini mengidentifikasi nilai outlier dalam vektor numerik menggunakan metode Z-Score,
#' IQR (Interquartile Range), atau Percentile. Fungsi ini akan menampilkan ringkasan dari
#' nilai outlier yang terdeteksi dan mengembalikan informasi secara detail.
#'
#' @param data Vektor numerik dari nilai-nilai.
#' @param metode Metode yang digunakan untuk deteksi nilai outlier. Pilihan yang tersedia:
#'    \itemize{
#'          \item: "z-score": Menggunakan standar deviasi dari mean.
#'          \item "iqr": Menggunakan rentang IQR.
#'          \item "percentile": Menggunakan ambang batas persentil khusus.
#'    }
#' @param ambang_batas Untuk metode Z-Score, nilai ambang batas (default adalah 3).
#'                 Untuk metode IQR, pengali untuk IQR (default adalah 1.5).
#' @param persentil_bawah Ambang batas bawah untuk metode percentile (default adalah 0.05).
#' @param persentil_atas Ambang batas atas untuk metode percentile (default adalah 0.95).
#'
#' @return Sebuah list yang berisi:
#'   \item{data}{Data frame dengan nilai asli dan status nilai outlier mereka.}
#'   \item{nilai_outlier}{Data frame dengan indeks dan nilai dari nilai outlier.}
#'   \item{jumlah_nilai_outlier}{Jumlah nilai outlier yang terdeteksi.}
#'
#' @examples
#' # Membuat sampel data
#' data <- data.frame(
#'   ID = c(10, 11, 12, 12, 13, 14, 15),
#'   Nama = c("Alice", "Bob", "Charlie", "Charlie", "Diana", "Evan", "Frank"),
#'   Umur = c(22, 28, 33, 33, 38, 42, 85)  # 85 bisa jadi nilai outlier
#' )
#'
#' # Deteksi nilai outlier menggunakan metode z-score
#' data_outlier <- deteksi_nilai_outlier(data$Umur, metode = "z-score")
#'
#' # Deteksi nilai outlier menggunakan metode 1.5 IQR
#' data_outlier <- deteksi_nilai_outlier(data$Umur, metode = "iqr")
#'
#' # Deteksi nilai outlier menggunakan metode percentile
#' data_outlier <- deteksi_nilai_outlier(data$Umur, metode = "percentile")
#'
#' @seealso
#' \code{\link{hapus_nilai_outlier}} untuk fungsi menghapus nilai outlier dari dataset.
#' \code{\link{imputasi_nilai_outlier}} untuk fungsi imputasi nilai outlier dalam dataset menggunakan metode dasar.
#' \code{\link{imputasi_nilai_outlier_lanjutan}} untuk fungsi imputasi nilai outlier dalam dataset menggunakan metode lanjutan.
#'
#' @importFrom stats quantile sd
#' @importFrom utils head
#'
#' @export

deteksi_nilai_outlier <- function(data, metode = c("z-score", "iqr", "percentile"), ambang_batas = NULL,
                            persentil_bawah = 0.05, persentil_atas = 0.95) {

  #  Untuk melihat apakah inputnya berupa angka
  if (!is.numeric(data)) {
    stop("Input harus berupa vektor numerik.")
  }

  # Untuk validasi argumen yang diberikan user sesuai dengan pilihan yang ada
  metode <- match.arg(metode)

  # Untuk menghapus nilai yang hilang pada data sebelum melakukan perhitungan
  indeks_na <- which(is.na(data))
  x_bersih <- data[!is.na(data)]

  # Apabila tidak ada data yang tidak memiliki nilai yang hilang
  if (length(x_bersih) == 0) {
    cat("Tidak ditemukan baris dengan nilai non-NA. Tidak ada nilai outlier yang ditemukan.")
    return(invisible(data))
  }

  # Apabila ada data yang memiliki nilai yang hilang maka tampilkan pesan warning ke user.
  if (any(is.na(data))) {
    cat("Ditemukan baris dengan nilai yang hilang dalam dataset. Baris ini akan diabaikan dalam proses deteksi outlier.\n")
  }

  # Untuk menentukan default threshold berdasarkan metode apabila user tidak memasukkan threshold
  if (is.null(ambang_batas)) {
    ambang_batas <- ifelse(metode == "z-score", 3, 1.5)
  }

  # Untuk membuat dataframe berisi result
  hasil <- data.frame(
    nilai = data,
    adalah_nilai_outlier = rep(FALSE, length(data))
  )

  # Untuk set value sebagai "NA" apabila row pada index tsb merupakan nilai yang hilang
  if (length(indeks_na) > 0) {
    hasil$adalah_nilai_outlier[indeks_na] <- NA
  }

  # Untuk menyimpan index row tanpa nilai yang hilang
  indeks_non_na <- which(!is.na(data))

  # Untuk menyimpan lower dan upper bound
  batas_bawah <- NA
  batas_atas <- NA

  if (metode == "z-score") {
    nilai_mean <- mean(x_bersih)
    nilai_sd <- stats::sd(x_bersih)

    if (nilai_sd > 0) {
      skor_z <- rep(NA, length(data))
      skor_z_bersih <- (x_bersih - nilai_mean) / nilai_sd
      skor_z[indeks_non_na] <- skor_z_bersih

      hasil$skor_z <- skor_z
      hasil$adalah_nilai_outlier[indeks_non_na] <- abs(skor_z_bersih) > ambang_batas
    } else {
      hasil$skor_z <- rep(0, length(data))
      hasil$skor_z[indeks_na] <- NA
      warning("Nilai Z-Score tidak dapat dihitung karena standar deviasi bernilai 0.\n")

    }

  } else if (metode == "iqr") {
    q1 <- stats::quantile(x_bersih, 0.25)
    q3 <- stats::quantile(x_bersih, 0.75)
    iqr <- stats::IQR(x_bersih)
    batas_bawah <- q1 - ambang_batas * iqr
    batas_atas <- q3 + ambang_batas * iqr

    hasil$batas_bawah <- batas_bawah
    hasil$batas_atas <- batas_atas
    hasil$adalah_nilai_outlier[indeks_non_na] <- data[indeks_non_na] < batas_bawah | data[indeks_non_na] > batas_atas

  } else if (metode == "percentile") {
    if (persentil_bawah < 0 || persentil_bawah > 1 || persentil_atas < 0 || persentil_atas > 1) {
      cat("Percentile harus antara 0 dan 1. \nSilakan coba kembali dengan memasukkan parameter yang tepat.\n")
      return(invisible(data))
    }
    if (persentil_bawah >= persentil_atas) {
      cat("Percentile bawah harus lebih kecil dari percentile atas. \nSilakan coba kembali dengan memasukkan parameter yang tepat.\n")
      return(invisible(data))
    }

    batas_bawah <- stats::quantile(x_bersih, persentil_bawah)
    batas_atas <- stats::quantile(x_bersih, persentil_atas)

    hasil$batas_bawah <- batas_bawah
    hasil$batas_atas <- batas_atas
    hasil$persentil_bawah <- persentil_bawah
    hasil$persentil_atas <- persentil_atas

    hasil$adalah_nilai_outlier[indeks_non_na] <- data[indeks_non_na] < batas_bawah | data[indeks_non_na] > batas_atas
  }

  # Untuk menyimpan index dan value yang merupakan nilai outlier untuk kebutuhan print summary
  indeks_nilai_outlier <- which(hasil$adalah_nilai_outlier == TRUE)
  nilai_nilai_outlier <- data[indeks_nilai_outlier]

  df_nilai_outlier <- data.frame(
    indeks = indeks_nilai_outlier,
    nilai = nilai_nilai_outlier
  )

  # Untuk sort nilai outlier berdasarkan index
  if (nrow(df_nilai_outlier) > 0) {
    df_nilai_outlier <- df_nilai_outlier[order(df_nilai_outlier$indeks), ]
  }

  # Print summary
  cat("Metode deteksi outlier:", metode, "\n")

  if (metode == "z-score") {
    cat("Ambang batas z-score:", ambang_batas, "\n")
  } else if (metode == "iqr") {
    cat("Pengali IQR:", ambang_batas, "\n")
    cat("Batas: [", round(batas_bawah, 4), ", ", round(batas_atas, 4), "]\n", sep="")
  } else if (metode == "percentile") {
    cat("Rentang percentile:", persentil_bawah, "sampai", persentil_atas, "\n")
    cat("Batas: [", round(batas_bawah, 4), ", ", round(batas_atas, 4), "]\n", sep="")
  }

  cat("Jumlah nilai outlier yang terdeteksi:", length(indeks_nilai_outlier), "\n")

  if (length(indeks_nilai_outlier) > 0) {
    cat("Indeks nilai outlier:", paste(indeks_nilai_outlier, collapse = ", "), "\n")
  } else {
    cat("Tidak ada nilai outlier yang terdeteksi.\n")
  }

  cat("\nSampel data hasil deteksi outlier")
  if (nrow(hasil) > 5) {
    cat(sprintf(" (menampilkan %d baris pertama dari %d):\n", 5, nrow(hasil)))
    print(head(hasil, 5))
  } else {
    cat(":\n")
    print(hasil)
  }

  return(invisible(list(
    data = hasil,
    nilai_outlier = df_nilai_outlier,
    jumlah_nilai_outlier = nrow(df_nilai_outlier)
  )))
}
