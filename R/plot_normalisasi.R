#' Plot distribusi sebelum dan sesudah normalisasi data
#'
#' Fungsi ini membuat visualisasi distribusi data sebelum dan sesudah menerapkan
#' berbagai metode normalisasi. Memungkinkan perbandingan efek normalisasi
#' pada bentuk dan properti data.
#'
#' @param data_asli Data frame atau vektor numerik yang berisi data asli.
#' @param data_normalisasi Data frame atau vektor numerik yang berisi data hasil normalisasi.
#' @param kolom Vektor karakter dari nama kolom yang akan diplot. Jika NULL (default), plot semua
#'   kolom numerik yang mengalami perubahan normalisasi.
#' @param maks_kolom Jumlah maksimal kolom yang ditampilkan per baris dalam plot grid (default 2).
#' @param metode String karakter yang menunjukkan metode normalisasi yang digunakan (untuk judul plot).
#' @param jenis_plot String karakter yang menentukan jenis plot. Pilihan adalah:
#' \itemize{
#'   \item "histogram" (default): Plot histogram untuk memvisualisasikan distribusi.
#'   \item "density": Plot density untuk menunjukkan distribusi yang dihaluskan.
#'   \item "boxplot": Boxplot untuk menunjukkan sebaran dan outlier.
#'   \item "qq": Plot quantile-quantile untuk membandingkan distribusi dengan distribusi normal.
#' }
#' @param bin_histogram Integer yang menentukan jumlah bin untuk histogram (default 30).
#' @param cetak_statistik Logis, apakah akan mencetak statistik ringkasan (default TRUE).
#'
#' @return Objek ggplot2 yang berisi visualisasi untuk penggunaan di Shiny.
#'
#' @examples
#' # Membuat sampel data
#' data <- data.frame(
#'   x = c(1, 2, 3, 4, 5),
#'   y = c(10, 20, 30, 40, 50)
#' )
#'
#' # Normalisasi data
#' data_norm <- normalisasi_data(data, metode = "minmax")
#'
#' # Visualisasi hasil normalisasi
#' plot_normalisasi(data, data_norm)
#'
#' @seealso
#' \code{\link{normalisasi_data}} untuk fungsi normalisasi atau penskalaan dataset.
#' \code{\link{plot_nilai_outlier}} untuk fungsi visualisasi data setelah menangani outlier.
#' \code{\link{plot_transformasi}} untuk fungsi visualisasi efek transformasi.
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density geom_boxplot
#'             geom_qq geom_qq_line theme_minimal labs
#'             scale_fill_manual theme element_text
#' @importFrom gridExtra grid.arrange
#'
#' @export

plot_normalisasi <- function(data_asli,
                             data_normalisasi,
                             kolom = NULL,
                             maks_kolom = 2,
                             metode = NULL,
                             jenis_plot = c("histogram", "density", "boxplot", "qq"),
                             bin_histogram = 30,
                             cetak_statistik = TRUE) {

  # Untuk cek package eksternal yang diperlukan
  required_packages <- c("ggplot2", "gridExtra")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop("Package berikut diperlukan: ", paste(missing_packages, collapse = ", "),
         "\nSilakan install dengan: install.packages(c('",
         paste(missing_packages, collapse = "', '"), "'))")
  }

  # Untuk validasi argumen
  jenis_plot <- match.arg(jenis_plot)

  # Konversi vektor ke data frame jika diperlukan
  is_vector_asli <- is.vector(data_asli) && !is.list(data_asli)
  is_vector_norm <- is.vector(data_normalisasi) && !is.list(data_normalisasi)

  if (is_vector_asli && is_vector_norm) {
    data_asli <- data.frame(nilai = data_asli)
    data_normalisasi <- data.frame(nilai = data_normalisasi)
    kolom <- "nilai"
  } else if (is_vector_asli != is_vector_norm) {
    stop("Kedua data_asli dan data_normalisasi harus memiliki tipe yang sama.")
  }

  # Validasi kolom
  if (is.null(kolom)) {
    kolom_numerik_asli <- sapply(data_asli, is.numeric)
    kolom_numerik_norm <- sapply(data_normalisasi, is.numeric)

    kolom_bersama <- intersect(names(data_asli)[kolom_numerik_asli],
                               names(data_normalisasi)[kolom_numerik_norm])

    if (length(kolom_bersama) == 0) {
      stop("Tidak ditemukan kolom numerik yang sama antara kedua dataset.")
    }

    # Auto-detect kolom yang benar-benar berubah
    kolom_berubah <- c()
    for (kol in kolom_bersama) {
      # Cek apakah ada perbedaan signifikan antara data asli dan normalisasi
      if (!identical(data_asli[[kol]], data_normalisasi[[kol]])) {
        # Double check dengan tolerance untuk floating point
        max_diff <- max(abs(data_asli[[kol]] - data_normalisasi[[kol]]), na.rm = TRUE)
        if (is.finite(max_diff) && max_diff > 1e-10) {
          kolom_berubah <- c(kolom_berubah, kol)
        }
      }
    }

    if (length(kolom_berubah) == 0) {
      stop("Tidak ada kolom yang menunjukkan perubahan normalisasi.")
    }

    kolom <- kolom_berubah

    if (cetak_statistik) {
      kolom_tidak_berubah <- setdiff(kolom_bersama, kolom_berubah)
      if (length(kolom_tidak_berubah) > 0) {
        cat("Info: Kolom berikut tidak diplot karena tidak ada perubahan normalisasi:",
            paste(kolom_tidak_berubah, collapse = ", "), "\n")
      }
    }
  } else {
    # Untuk validasi kolom yang diminta user
    missing_asli <- setdiff(kolom, names(data_asli))
    missing_norm <- setdiff(kolom, names(data_normalisasi))

    if (length(missing_asli) > 0) {
      stop("Kolom berikut tidak ada dalam data asli: ", paste(missing_asli, collapse = ", "))
    }

    if (length(missing_norm) > 0) {
      stop("Kolom berikut tidak ada dalam data normalisasi: ", paste(missing_norm, collapse = ", "))
    }

    # Untuk validasi tipe numerik
    non_numerik_asli <- kolom[!sapply(data_asli[kolom], is.numeric)]
    non_numerik_norm <- kolom[!sapply(data_normalisasi[kolom], is.numeric)]

    if (length(non_numerik_asli) > 0) {
      stop("Kolom berikut dalam data asli bukan merupakan numerik: ", paste(non_numerik_asli, collapse = ", "))
    }

    if (length(non_numerik_norm) > 0) {
      stop("Kolom berikut dalam data normalisasi bukan merupakan numerik: ", paste(non_numerik_norm, collapse = ", "))
    }

    # Warning untuk kolom yang tidak berubah (jika user specify)
    if (cetak_statistik) {
      kolom_tidak_berubah <- c()
      for (kol in kolom) {
        if (identical(data_asli[[kol]], data_normalisasi[[kol]])) {
          max_diff <- max(abs(data_asli[[kol]] - data_normalisasi[[kol]]), na.rm = TRUE)
          if (!is.finite(max_diff) || max_diff <= 1e-10) {
            kolom_tidak_berubah <- c(kolom_tidak_berubah, kol)
          }
        }
      }

      if (length(kolom_tidak_berubah) > 0) {
        cat("Peringatan: Kolom berikut tampaknya tidak mengalami normalisasi:",
            paste(kolom_tidak_berubah, collapse = ", "), "\n")
      }
    }
  }

  daftar_plot <- list()

  for (kol in kolom) {
    nilai_asli <- data_asli[[kol]]
    nilai_norm <- data_normalisasi[[kol]]

    # Gabungkan data untuk plotting
    data_gabungan <- data.frame(
      nilai = c(nilai_asli, nilai_norm),
      tipe = factor(c(rep("Asli", length(nilai_asli)),
                      rep("Normalisasi", length(nilai_norm))),
                    levels = c("Asli", "Normalisasi"))
    )

    # Buat plot berdasarkan jenis
    if (jenis_plot == "histogram") {
      p <- ggplot2::ggplot(data_gabungan, ggplot2::aes(x = nilai, fill = tipe)) +
        ggplot2::geom_histogram(alpha = 0.7, position = "identity", bins = bin_histogram) +
        ggplot2::scale_fill_manual(values = c("Asli" = "#2ecc71", "Normalisasi" = "#e67e22")) +
        ggplot2::labs(title = paste("Histogram:", kol),
                      x = "Nilai", y = "Jumlah", fill = "Data") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
                       legend.position = "bottom")

    } else if (jenis_plot == "density") {
      p <- ggplot2::ggplot(data_gabungan, ggplot2::aes(x = nilai, fill = tipe)) +
        ggplot2::geom_density(alpha = 0.7) +
        ggplot2::scale_fill_manual(values = c("Asli" = "#2ecc71", "Normalisasi" = "#e67e22")) +
        ggplot2::labs(title = paste("Density:", kol),
                      x = "Nilai", y = "Kepadatan", fill = "Tipe") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
                       legend.position = "bottom")

    } else if (jenis_plot == "boxplot") {
      p <- ggplot2::ggplot(data_gabungan, ggplot2::aes(x = tipe, y = nilai, fill = tipe)) +
        ggplot2::geom_boxplot() +
        ggplot2::scale_fill_manual(values = c("Asli" = "#2ecc71", "Normalisasi" = "#e67e22")) +
        ggplot2::labs(title = paste("Box Plot:", kol),
                      x = "", y = "Nilai") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
                       legend.position = "none")

    } else if (jenis_plot == "qq") {
      # QQ plot untuk data asli
      p_asli <- ggplot2::ggplot(data.frame(nilai = nilai_asli), ggplot2::aes(sample = nilai)) +
        ggplot2::geom_qq() +
        ggplot2::geom_qq_line() +
        ggplot2::labs(title = paste("QQ Plot (Asli):", kol),
                      x = "Theoretical Quantiles", y = "Sample Quantiles") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"))

      # QQ plot untuk data normalisasi
      p_norm <- ggplot2::ggplot(data.frame(nilai = nilai_norm), ggplot2::aes(sample = nilai)) +
        ggplot2::geom_qq() +
        ggplot2::geom_qq_line() +
        ggplot2::labs(title = paste("QQ Plot (Normalisasi):", kol),
                      x = "Theoretical Quantiles", y = "Sample Quantiles") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"))

      daftar_plot[[paste(kol, "qq_asli", sep = "_")]] <- p_asli
      daftar_plot[[paste(kol, "qq_norm", sep = "_")]] <- p_norm
      next
    }

    daftar_plot[[paste(kol, jenis_plot, sep = "_")]] <- p
  }

  # Return plot list untuk Shiny atau single plot jika hanya satu
  if (length(daftar_plot) == 0) {
    warning("Tidak ada plot yang dibuat.")
    return(NULL)
  }

  if (length(daftar_plot) == 1) {
    return(daftar_plot[[1]])
  }

  # Jika ada multiple plots, buat grid arrangement
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    judul <- paste("Efek normalisasi", metode, "pada distribusi data")

    grid_plot <- gridExtra::grid.arrange(
      grobs = daftar_plot,
      ncol = maks_kolom,
      top = judul
    )

    return(grid_plot)
  } else {
    return(daftar_plot)
  }
}
