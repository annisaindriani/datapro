#' Plot distribusi sebelum dan sesudah penanganan nilai outlier
#'
#' Fungsi ini membuat visualisasi distribusi data sebelum dan sesudah menerapkan
#' penanganan nilai outlier seperti imputasi atau penghapusan. Ini membantu untuk menilai efek
#' dari penanganan tersebut pada distribusi data secara keseluruhan.
#'
#' @param data_asli Data frame atau vektor numerik yang berisi data asli.
#' @param data_penanganan Data frame atau vektor numerik yang berisi data setelah penanganan nilai outlier.
#' @param kolom Vektor karakter dari nama kolom yang akan diplot. Jika NULL (default), akan plot semua
#'   kolom numerik yang sama antara kedua dataset.
#' @param maks_kolom Jumlah maksimal kolom yang ditampilkan per baris dalam plot grid (default 2).
#' @param jenis_penanganan String karakter yang menunjukkan penanganan mana yang diterapkan (untuk judul plot).
#' @param metode String karakter yang menunjukkan metode deteksi nilai outlier mana yang digunakan (untuk judul plot).
#' @param jenis_plot String karakter yang menentukan jenis plot. Pilihan adalah:
#' \itemize{
#'   \item "histogram" (default): Plot histogram untuk memvisualisasikan distribusi.
#'   \item "density": Plot density untuk menunjukkan distribusi yang dihaluskan.
#'   \item "boxplot": Boxplot untuk menunjukkan sebaran dan nilai outlier.
#'   \item "qq": Plot quantile-quantile untuk membandingkan distribusi dengan distribusi normal.
#' }
#' @param flag_outlier Logikal, apakah akan menyoroti nilai outlier dalam data asli (default TRUE).
#' @param ambang_batas Nilai ambang batas yang digunakan untuk deteksi nilai outlier (untuk referensi).
#' @param bin_histogram Integer yang menentukan jumlah bin untuk histogram (default 30).
#'
#' @return Objek ggplot2 yang berisi visualisasi (secara tidak langsung).
#'
#' @examples
#' # Membuat sampel data
#' set.seed(123)
#' data <- c(rnorm(95), rnorm(5, mean=10))
#' data_terimputasi <- imputasi_nilai_outlier(data, metode = "z-score")
#'
#' # Menampilkan plot visualisasi
#' plot_nilai_outlier(data, data_terimputasi, jenis_penanganan = "imputasi", metode = "z-score")
#'
#' @seealso
#' \code{\link{deteksi_nilai_outlier}} untuk fungsi mengidentifikasi nilai outlier dalam dataset.
#' \code{\link{imputasi_nilai_outlier}} untuk fungsi mengimputasi nilai outlier dalam dataset menggunakan metode biasa.
#' \code{\link{imputasi_nilai_outlier_lanjutan}} untuk fungsi mengimputasi nilai outlier dalam dataset menggunakan metode lanjutan.
#' \code{\link{hapus_nilai_outlier}} untuk fungsi menghapus nilai outlier dari dataset.
#' \code{\link{plot_normalisasi}} untuk fungsi memvisualisasikan efek normalisasi.
#' \code{\link{plot_transformasi}} untuk fungsi memvisualisasikan efek transformasi.
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density geom_boxplot
#'             geom_point geom_qq geom_qq_line theme_minimal labs
#'             scale_fill_manual scale_color_manual ggtitle theme element_text
#' @importFrom gridExtra grid.arrange
#' @importFrom reshape2 melt
#'
#' @export

plot_nilai_outlier <- function(data_asli,
                               data_penanganan,
                               kolom = NULL,
                               maks_kolom = 2,
                               jenis_penanganan = c("imputasi", "penghapusan"),
                               metode = c("z-score", "iqr", "percentile"),
                               jenis_plot = c("histogram", "density", "boxplot", "qq"),
                               flag_outlier = TRUE,
                               ambang_batas = NULL,
                               bin_histogram = 30) {

  # Untuk validasi argumen jenis_penanganan dan jenis_plot yang diberikan user sesuai dengan pilihan yang ada
  jenis_penanganan <- match.arg(jenis_penanganan)
  jenis_plot <- match.arg(jenis_plot)

  # Untuk mengecek package eksternal yang dimiliki
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Paket 'ggplot2' dan 'gridExtra' diperlukan untuk fungsi ini.\n",
         "Silakan install dengan: install.packages(c('ggplot2', 'gridExtra'))")
  }

  # Apabila input berupa vektor
  adalah_vektor_asli <- is.vector(data_asli) && !is.list(data_asli)
  adalah_vektor_ditangani <- is.vector(data_penanganan) && !is.list(data_penanganan)

  if (adalah_vektor_asli && adalah_vektor_ditangani) {
    # Apabila kedua input berupa vektor, ubah menjadi dataframe
    data_asli <- data.frame(nilai = data_asli)
    data_penanganan <- data.frame(nilai = data_penanganan)
    kolom <- "nilai"
  } else if (adalah_vektor_asli != adalah_vektor_ditangani) {
    # Apabila kedua input memiliki tipe data yang berbeda
    stop("Kedua data_asli dan data_penanganan harus memiliki tipe yang sama (baik keduanya vektor atau keduanya data frame).")
  }

  # Untuk mengambil kolom numerik
  if (is.null(kolom)) {
    kolom_numerik_asli <- sapply(data_asli, is.numeric)
    kolom_numerik_ditangani <- sapply(data_penanganan, is.numeric)

    if (!any(kolom_numerik_asli) || !any(kolom_numerik_ditangani)) {
      stop("Tidak ditemukan kolom numerik dalam dataset.")
    }

    kolom_bersama <- intersect(names(data_asli)[kolom_numerik_asli],
                               names(data_penanganan)[kolom_numerik_ditangani])

    if (length(kolom_bersama) == 0) {
      stop("Tidak ditemukan kolom numerik yang sama antara dataset asli dan yang ditangani.")
    }

    kolom <- kolom_bersama
  } else {
    # Untuk memvalidasi bahwa kolom yang diinput user ada
    kolom_hilang_asli <- setdiff(kolom, names(data_asli))
    kolom_hilang_ditangani <- setdiff(kolom, names(data_penanganan))

    if (length(kolom_hilang_asli) > 0) {
      stop("Kolom berikut tidak ada dalam data asli: ",
           paste(kolom_hilang_asli, collapse = ", "))
    }

    if (length(kolom_hilang_ditangani) > 0) {
      stop("Kolom berikut tidak ada dalam data yang ditangani: ",
           paste(kolom_hilang_ditangani, collapse = ", "))
    }

    # Untuk memvalidasi bahwa kolom yang diinput user merupakan numerik
    non_numerik_asli <- kolom[!sapply(data_asli[kolom], is.numeric)]
    non_numerik_ditangani <- kolom[!sapply(data_penanganan[kolom], is.numeric)]

    if (length(non_numerik_asli) > 0) {
      stop("Kolom berikut dalam data asli bukan numerik: ",
           paste(non_numerik_asli, collapse = ", "))
    }

    if (length(non_numerik_ditangani) > 0) {
      stop("Kolom berikut dalam data yang ditangani bukan numerik: ",
           paste(non_numerik_ditangani, collapse = ", "))
    }
  }

  daftar_plot <- list()

  for (kol in kolom) {
    nilai_asli <- data_asli[[kol]]
    nilai_ditangani <- data_penanganan[[kol]]

    # Untuk mengidentifikasi dan menyimpan nilai outlier pada data
    if (flag_outlier) {
      info_nilai_outlier <- deteksi_nilai_outlier(nilai_asli, metode = metode, ambang_batas = ambang_batas)
      adalah_nilai_outlier <- info_nilai_outlier$data$adalah_nilai_outlier
    } else {
      adalah_nilai_outlier <- rep(FALSE, length(nilai_asli))
    }

    if (jenis_plot == "histogram") {
      p_asli <- ggplot2::ggplot() +
        ggplot2::geom_histogram(data = data.frame(x = nilai_asli, nilai_outlier = adalah_nilai_outlier),
                                ggplot2::aes(x = x, fill = nilai_outlier), bins = bin_histogram, alpha = 0.7) +
        ggplot2::scale_fill_manual(values = c("FALSE" = "#1f77b4", "TRUE" = "#d62728"),
                                   labels = c("Normal", "Nilai Outlier")) +
        ggplot2::labs(title = paste("Sebelum", jenis_penanganan, ":", kol),
                      x = "Nilai", y = "Jumlah") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = if(sum(adalah_nilai_outlier) > 0) "top" else "none",
                       legend.direction = "horizontal")

      p_ditangani <- ggplot2::ggplot(data.frame(x = nilai_ditangani),
                                     ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(fill = "#2ca02c", alpha = 0.7, bins = bin_histogram) +
        ggplot2::labs(title = paste("Setelah", jenis_penanganan, ":", kol),
                      x = "Nilai", y = "Jumlah") +
        ggplot2::theme_minimal()

      daftar_plot[[paste(kol, "asli", sep = "_")]] <- p_asli
      daftar_plot[[paste(kol, "ditangani", sep = "_")]] <- p_ditangani
    }

    if (jenis_plot == "density") {
      # Untuk plot density
      df_asli <- data.frame(x = nilai_asli, nilai_outlier = adalah_nilai_outlier)
      df_ditangani <- data.frame(x = nilai_ditangani, nilai_outlier = rep(FALSE, length(nilai_ditangani)))

      p_density <- ggplot2::ggplot() +
        ggplot2::geom_density(data = df_asli[!df_asli$nilai_outlier,],
                              ggplot2::aes(x = x, fill = "Asli"), alpha = 0.5) +
        ggplot2::geom_density(data = df_ditangani,
                              ggplot2::aes(x = x, fill = "Ditangani"), alpha = 0.5)

      if (sum(adalah_nilai_outlier) > 0) {
        p_density <- p_density +
          ggplot2::geom_rug(data = df_asli[df_asli$nilai_outlier,],
                            ggplot2::aes(x = x, color = "Nilai Outlier"),
                            size = 1.5, alpha = 0.8)
      }

      p_density <- p_density +
        ggplot2::scale_fill_manual(name = "Data",
                                   values = c("Asli" = "#1f77b4", "Ditangani" = "#2ca02c")) +
        ggplot2::scale_color_manual(name = "", values = c("Nilai Outlier" = "#d62728")) +
        ggplot2::labs(title = paste("Plot density:", kol),
                      x = "Nilai", y = "density") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "top",
                       legend.direction = "horizontal")

      daftar_plot[[paste(kol, "density", sep = "_")]] <- p_density
    }

    if (jenis_plot == "boxplot") {
      df_asli <- data.frame(
        nilai = nilai_asli,
        data = "Asli",
        nilai_outlier = adalah_nilai_outlier
      )

      df_ditangani <- data.frame(
        nilai = nilai_ditangani,
        data = "Ditangani",
        nilai_outlier = rep(FALSE, length(nilai_ditangani))
      )

      df_gabungan <- rbind(df_asli, df_ditangani)
      df_gabungan$data <- factor(df_gabungan$data, levels = c("Asli", "Ditangani"))

      p_boxplot <- ggplot2::ggplot(df_gabungan, ggplot2::aes(x = data, y = nilai, fill = data)) +
        ggplot2::geom_boxplot() +
        ggplot2::scale_fill_manual(values = c("Asli" = "#1f77b4", "Ditangani" = "#2ca02c")) +
        ggplot2::labs(title = paste("Boxplot:", kol),
                      y = "Nilai") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")

      if (flag_outlier && sum(adalah_nilai_outlier) > 0) {
        p_boxplot <- p_boxplot +
          ggplot2::geom_point(data = df_asli[df_asli$nilai_outlier,],
                              ggplot2::aes(x = data, y = nilai),
                              color = "#d62728", size = 3, shape = 8)
      }

      daftar_plot[[paste(kol, "boxplot", sep = "_")]] <- p_boxplot
    }

    if (jenis_plot == "qq") {
      # QQ plot untuk data asli dan yang ditangani
      p_qq_asli <- ggplot2::ggplot(data.frame(nilai = nilai_asli),
                                   ggplot2::aes(sample = nilai)) +
        ggplot2::geom_qq() +
        ggplot2::geom_qq_line() +
        ggplot2::labs(title = paste("Plot QQ (Asli):", kol),
                      x = "Theoritical Quantiles", y = "Sample Quantiles") +
        ggplot2::theme_minimal()

      p_qq_ditangani <- ggplot2::ggplot(data.frame(nilai = nilai_ditangani),
                                        ggplot2::aes(sample = nilai)) +
        ggplot2::geom_qq() +
        ggplot2::geom_qq_line() +
        ggplot2::labs(title = paste("Plot QQ (Setelah Penanganan):", kol),
                      x = "Theoritical Quantiles", y = "Sample Quantiles") +
        ggplot2::theme_minimal()

      daftar_plot[[paste(kol, "qq_asli", sep = "_")]] <- p_qq_asli
      daftar_plot[[paste(kol, "qq_ditangani", sep = "_")]] <- p_qq_ditangani
    }
  }

  # Untuk menentukan baris x kolom pada grid
  if (jenis_plot %in% c("qq", "histogram")) {
    jumlah_plot_per_kolom <- 2  # dua plot per kolom
    jumlah_baris <- ceiling(length(kolom) * jumlah_plot_per_kolom / maks_kolom)
  } else {
    jumlah_plot_per_kolom <- 1
    jumlah_baris <- ceiling(length(daftar_plot) / maks_kolom)
  }

  if (length(daftar_plot) > 0) {
    awalan_judul <- "Efek penanganan nilai outlier"

    judul <- paste(awalan_judul, "yang dideteksi menggunakan metode", metode)

    # Untuk mengecek paket eksternal yang dimiliki oleh user
    if (!requireNamespace("grid", quietly = TRUE)) {
      stop("Paket 'grid' diperlukan untuk fungsi ini.\n",
           "Silakan install dengan: install.packages('grid')")
    }

    # Untuk convert plot ke dalam bentuk grid
    plot_grid <- suppressMessages(
      gridExtra::grid.arrange(
        grobs = daftar_plot,
        ncol = maks_kolom,
        top = grid::textGrob(
          judul,
          gp = grid::gpar(fontsize = 16, fontface = "bold")
        )
      )
    )

    return(invisible(plot_grid))
  } else {
    warning("Tidak ada plot yang dibuat.")
    return(invisible(NULL))
  }
}
