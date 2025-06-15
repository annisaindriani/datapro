#' Reduksi dimensi dataset menggunakan berbagai metode
#'
#' Fungsi ini melakukan reduksi dimensi pada dataset menggunakan metode seperti PCA,
#' ICA, t-SNE, UMAP, atau LDA dan menghasilkan berbagai plot untuk memudahkan analisis statistik.
#'
#' @param data Data frame atau matriks yang berisi data untuk direduksi.
#' @param metode Metode reduksi dimensi: "PCA", "ICA", "t-SNE", "UMAP", atau "LDA".
#' @param n_komponen Jumlah dimensi yang akan direduksi (default 2).
#' @param target Untuk LDA saja, variabel faktor yang berisi label kelas.
#' @param scale Logis, apakah data akan discale sebelum reduksi (default TRUE).
#' @param center Logis, apakah data akan dicenterkan sebelum reduksi (default TRUE).
#' @param buat_plot Logis, apakah akan membuat plot (default TRUE).
#' @param warna_berdasarkan Vektor atau nama kolom untuk mewarnai titik dalam plot.
#' @param judul Judul opsional untuk plot.
#' @param seed Random seed untuk reproduktibilitas (default 42).
#' @param return_plotly Logis, apakah mengembalikan plot interaktif plotly (default FALSE).
#' @param ... Parameter tambahan untuk metode reduksi dimensi spesifik.
#'
#' @return List berisi data yang telah direduksi, model, plots, ringkasan, info_metode, validasi.
#'
#' @examples
#' # Memuat data iris
#' library(datasets)
#' hasil_pca <- reduksi_dimensi(iris[, 1:4], metode = "PCA",
#'                             warna_berdasarkan = iris$Species)
#'
#' @seealso
#' \code{\link{normalisasi_data}} untuk fungsi normalisasi atau penskalaan dataset.
#' \code{\link{transformasi_data}} untuk fungsi transformasi data.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_segment geom_text
#'                     geom_tile labs theme theme_minimal element_text scale_x_continuous
#'                     scale_y_continuous scale_fill_gradient scale_fill_gradient2
#'                     scale_color_viridis_d unit arrow geom_hline geom_vline
#' @importFrom dplyr as_tibble
#' @importFrom stats prcomp predict as.formula
#' @importFrom fastICA fastICA
#' @importFrom Rtsne Rtsne
#' @importFrom MASS lda
#' @importFrom umap umap
#' @importFrom plotly ggplotly
#' @importFrom utils head tail
#'
#' @export

reduksi_dimensi <- function(data,
                            metode = c("PCA", "ICA", "t-SNE", "UMAP", "LDA"),
                            n_komponen = 2,
                            target = NULL,
                            scale = TRUE,
                            center = TRUE,
                            buat_plot = TRUE,
                            warna_berdasarkan = NULL,
                            judul = NULL,
                            seed = 42,
                            return_plotly = FALSE,
                            ...) {

  # Untuk validasi dan pencocokan argumen metode
  metode <- match.arg(metode)

  # Untuk menyimpan hasil
  hasil <- list(
    data_reduksi = NULL,
    model = NULL,
    plots = NULL,
    ringkasan = list(),
    info_metode = list(metode = metode, n_komponen = n_komponen),
    validasi = list(pesan = character(), peringatan = character())
  )

  # Daftar package yang diperlukan
  package_diperlukan <- list(
    "PCA" = c("ggplot2", "dplyr"),
    "ICA" = c("ggplot2", "dplyr", "fastICA"),
    "t-SNE" = c("ggplot2", "dplyr", "Rtsne"),
    "UMAP" = c("ggplot2", "dplyr", "umap"),
    "LDA" = c("ggplot2", "dplyr", "MASS")
  )

  # Tambahkan plotly jika diperlukan
  if (return_plotly) {
    package_diperlukan[[metode]] <- c(package_diperlukan[[metode]], "plotly")
  }

  # Untuk mengecek package eksternal
  package_kosong <- package_diperlukan[[metode]][
    !sapply(package_diperlukan[[metode]], requireNamespace, quietly = TRUE)
  ]

  if (length(package_kosong) > 0) {
    pesan_error <- paste0(
      "Package berikut diperlukan untuk metode '", metode, "': ",
      paste(package_kosong, collapse = ", "), ".\n",
      "Silakan instal dengan: install.packages(c(",
      paste(sprintf("'%s'", package_kosong), collapse = ", "), "))"
    )

    hasil$validasi$pesan <- c(hasil$validasi$pesan, pesan_error)
    stop(pesan_error)
  }

  # Untuk validasi input data
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Input harus berupa data frame atau matriks.")
  }

  # Konversi ke matriks untuk komputasi
  matriks_data <- as.matrix(data)

  # Untuk validasi khusus untuk LDA
  if (metode == "LDA") {
    if (is.null(target)) {
      stop("Untuk metode LDA, variabel 'target' faktor harus ditentukan.")
    }

    if (!is.factor(target)) {
      target <- as.factor(target)
    }

    if (length(target) != nrow(matriks_data)) {
      stop("Panjang 'target' harus sesuai dengan jumlah baris dalam 'data'.")
    }

  }

  set.seed(seed)

  # Proses reduksi dimensi berdasarkan metode
  if (metode == "PCA") {
    hasil <- proses_pca(matriks_data, n_komponen, center, scale, hasil)

  } else if (metode == "ICA") {
    hasil <- proses_ica(matriks_data, n_komponen, center, scale, hasil, ...)

  } else if (metode == "t-SNE") {
    hasil <- proses_tsne(matriks_data, n_komponen, center, scale, hasil, ...)

  } else if (metode == "UMAP") {
    hasil <- proses_umap(matriks_data, n_komponen, center, scale, hasil, ...)

  } else if (metode == "LDA") {
    hasil <- proses_lda(matriks_data, target, n_komponen, hasil, ...)
  }

  # Tambahkan nama baris jika ada
  if (!is.null(rownames(data))) {
    rownames(hasil$data_reduksi) <- rownames(data)
  }

  # Konversi ke tibble dengan ID
  hasil$data_reduksi <- dplyr::as_tibble(hasil$data_reduksi, rownames = "id")

  # Buat plot jika diminta
  if (buat_plot) {
    hasil$plots <- buat_semua_plots(hasil$data_reduksi, hasil$model, hasil$ringkasan,
                                    metode, warna_berdasarkan, judul, data,
                                    target, return_plotly)
  }

  ringkasan_reduksi(hasil)
  ambil_plot(hasil)

  return(invisible(hasil))
}

# Fungsi untuk PCA
proses_pca <- function(matriks_data, n_komponen, center, scale, hasil) {
  model <- stats::prcomp(matriks_data, center = center, scale. = scale)
  data_reduksi <- as.data.frame(model$x[, 1:min(n_komponen, ncol(model$x))])
  colnames(data_reduksi) <- paste0("PC", 1:ncol(data_reduksi))

  varians_dijelaskan <- (model$sdev^2) / sum(model$sdev^2) * 100
  varians_kumulatif <- cumsum(varians_dijelaskan)

  hasil$model <- model
  hasil$data_reduksi <- data_reduksi
  hasil$ringkasan$varians_dijelaskan <- varians_dijelaskan[1:n_komponen]
  hasil$ringkasan$varians_kumulatif <- varians_kumulatif[1:n_komponen]
  hasil$ringkasan$total_varians <- sum(varians_dijelaskan[1:n_komponen])
  hasil$ringkasan$loading <- model$rotation[, 1:n_komponen]

  return(hasil)
}

# Fungsi untuk ICA
proses_ica <- function(matriks_data, n_komponen, center, scale, hasil, ...) {
  if (scale || center) {
    matriks_data <- scale(matriks_data, center = center, scale = scale)
  }

  dots <- list(...)
  model <- do.call(fastICA::fastICA,
                   c(list(X = matriks_data, n.comp = n_komponen), dots))

  data_reduksi <- as.data.frame(model$S)
  colnames(data_reduksi) <- paste0("IC", 1:ncol(data_reduksi))

  hasil$model <- model
  hasil$data_reduksi <- data_reduksi
  hasil$ringkasan$matriks_mixing <- model$A
  hasil$ringkasan$matriks_unmixing <- model$W

  return(hasil)
}

# Fungsi untuk t-SNE
proses_tsne <- function(matriks_data, n_komponen, center, scale, hasil, ...) {
  dots <- list(...)

  # Set perplexity defaultnya
  if (is.null(dots$perplexity)) {
    perplexity <- min(30, floor(nrow(matriks_data) / 5))
  } else {
    perplexity <- dots$perplexity
    dots$perplexity <- NULL
  }

  if (scale || center) {
    matriks_data <- scale(matriks_data, center = center, scale = scale)
  }

  if (any(is.na(matriks_data))) {
    stop("t-SNE tidak dapat menangani data dengan nilai yang hilang. Silakan lakukan penanganan terlebih dahulu.")
  }

  model <- do.call(Rtsne::Rtsne,
                   c(list(X = matriks_data, dims = n_komponen,
                          perplexity = perplexity, check_duplicates = FALSE,
                          pca = TRUE), dots))

  data_reduksi <- as.data.frame(model$Y)
  colnames(data_reduksi) <- paste0("tSNE", 1:ncol(data_reduksi))

  hasil$model <- model
  hasil$data_reduksi <- data_reduksi
  hasil$ringkasan$kl_divergence <- tail(model$itercosts, 1)

  return(hasil)
}

# Fungsi untuk UMAP
proses_umap <- function(matriks_data, n_komponen, center, scale, hasil, ...) {
  if (scale || center) {
    matriks_data <- scale(matriks_data, center = center, scale = scale)
  }

  if (any(is.na(matriks_data))) {
    stop("UMAP tidak dapat menangani missing values. Silakan imputasi atau hapus terlebih dahulu.")
  }

  dots <- list(...)
  model <- do.call(umap::umap,
                   c(list(d = matriks_data, n_components = n_komponen), dots))

  data_reduksi <- as.data.frame(model$layout)
  colnames(data_reduksi) <- paste0("UMAP", 1:ncol(data_reduksi))

  hasil$model <- model
  hasil$data_reduksi <- data_reduksi
  hasil$ringkasan$konfigurasi <- model$config

  return(hasil)
}

# Fungsi untuk LDA
proses_lda <- function(matriks_data, target, n_komponen, hasil, ...) {
  if (length(unique(target)) < 2) {
    stop("LDA memerlukan setidaknya 2 kelas dalam 'target'.")
  }

  max_komponen <- min(ncol(matriks_data), length(unique(target)) - 1)
  if (n_komponen > max_komponen) {
    peringatan <- paste0("LDA maksimal dapat memiliki ", max_komponen,
                         " komponen. Mengatur n_komponen ke ", max_komponen, ".")
    hasil$validasi$peringatan <- c(hasil$validasi$peringatan, peringatan)
    warning(peringatan)
    n_komponen <- max_komponen

  }

  data_lda <- data.frame(matriks_data, Kelas = target)
  formula <- stats::as.formula(paste("Kelas ~", paste(colnames(matriks_data), collapse = " + ")))
  model <- MASS::lda(formula, data = data_lda, ...)

  pred <- stats::predict(model, newdata = data_lda)
  data_reduksi <- as.data.frame(pred$x[, 1:n_komponen, drop = FALSE])
  colnames(data_reduksi) <- paste0("LD", 1:ncol(data_reduksi))

  hasil$model <- model
  hasil$data_reduksi <- data_reduksi
  hasil$ringkasan$rata_rata_kelas <- model$means
  hasil$ringkasan$scale <- model$scaling
  hasil$ringkasan$proporsi_trace <- model$svd^2 / sum(model$svd^2)
  hasil$ringkasan$prediksi <- pred$class

  return(hasil)
}

# Fungsi utama untuk membuat semua plot
buat_semua_plots <- function(data_reduksi, model, ringkasan, metode,
                             warna_berdasarkan, judul, data_asli, target,
                             return_plotly) {

  plots <- list()

  # Proses variabel warna
  variabel_warna <- NULL
  if (!is.null(warna_berdasarkan)) {
    if (is.character(warna_berdasarkan) && length(warna_berdasarkan) == 1 &&
        warna_berdasarkan %in% colnames(data_asli)) {
      variabel_warna <- data_asli[[warna_berdasarkan]]
    } else {
      variabel_warna <- warna_berdasarkan
    }

    # Untuk validasi panjang
    if (length(variabel_warna) != nrow(data_asli)) {
      warning("Panjang 'warna_berdasarkan' tidak sesuai dengan jumlah baris data.")
      variabel_warna <- NULL
    }
  }

  # Buat plot berdasarkan metode
  if (metode == "PCA") {
    plots <- buat_plots_pca(data_reduksi, model, ringkasan, variabel_warna,
                            judul, return_plotly)

  } else if (metode == "LDA") {
    plots <- buat_plots_lda(data_reduksi, model, ringkasan, variabel_warna,
                            target, judul, return_plotly)

  } else if (metode == "t-SNE") {
    plots <- buat_plots_tsne(data_reduksi, model, variabel_warna, judul, return_plotly)

  } else if (metode == "UMAP") {
    plots <- buat_plots_umap(data_reduksi, variabel_warna, judul, return_plotly)

  } else if (metode == "ICA") {
    plots <- buat_plots_ica(data_reduksi, model, ringkasan, variabel_warna,
                            judul, return_plotly)
  }

  return(plots)
}

# Plot untuk PCA
buat_plots_pca <- function(data_reduksi, model, ringkasan, variabel_warna,
                           judul, return_plotly) {
  plots <- list()

  # Untuk membuat scree plot
  if (length(ringkasan$varians_dijelaskan) > 0) {
    semua_varians <- (model$sdev^2) / sum(model$sdev^2) * 100

    data_scree <- data.frame(
      PC = paste0("PC", 1:length(semua_varians)),
      PC_num = 1:length(semua_varians),
      ExplainedVar = semua_varians
    )

    p_scree <- ggplot2::ggplot(data_scree, ggplot2::aes(x = PC_num, y = ExplainedVar)) +
      ggplot2::geom_line(color = "blue", size = 1.2) +
      ggplot2::geom_point(color = "red", size = 3) +
      ggplot2::labs(title = "Scree Plot - Explained Variance",
                    x = "Principal Component",
                    y = "Explained Variance (%)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12)) +
      ggplot2::scale_x_continuous(breaks = data_scree$PC_num, labels = data_scree$PC)

    plots$scree_plot <- p_scree
  }

  # Untuk membuat biplot
  if (ncol(data_reduksi) >= 3) { # minimal PC1, PC2 + id
    scores <- as.data.frame(model$x[, 1:2])
    colnames(scores) <- c("PC1", "PC2")

    loadings <- as.data.frame(model$rotation[, 1:2])
    colnames(loadings) <- c("PC1", "PC2")
    loadings$Variable <- rownames(loadings)

    # Scale loadings untuk visualisasi
    scale_factor <- 3
    max_score_range <- max(abs(c(scores$PC1, scores$PC2)))
    loadings$PC1_scaled <- loadings$PC1 * scale_factor * max_score_range
    loadings$PC2_scaled <- loadings$PC2 * scale_factor * max_score_range

    p_biplot <- ggplot2::ggplot() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "PCA Biplot",
                    x = paste0("PC1 (", round(ringkasan$varians_dijelaskan[1], 1), "%)"),
                    y = paste0("PC2 (", round(ringkasan$varians_dijelaskan[2], 1), "%)")) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12))

    # Tambahkan points
    if (!is.null(variabel_warna)) {
      scores$Warna <- variabel_warna
      p_biplot <- p_biplot +
        ggplot2::geom_point(data = scores, ggplot2::aes(x = PC1, y = PC2, color = Warna),
                            alpha = 0.7, size = 2) +
        ggplot2::scale_color_viridis_d(name = "Group")
    } else {
      p_biplot <- p_biplot +
        ggplot2::geom_point(data = scores, ggplot2::aes(x = PC1, y = PC2),
                            alpha = 0.7, size = 2, color = "steelblue")
    }

    # Tambahkan loading vectors
    p_biplot <- p_biplot +
      ggplot2::geom_segment(data = loadings,
                            ggplot2::aes(x = 0, y = 0, xend = PC1_scaled, yend = PC2_scaled),
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm")),
                            color = "red", size = 1, alpha = 0.8) +
      ggplot2::geom_text(data = loadings,
                         ggplot2::aes(x = PC1_scaled * 1.1, y = PC2_scaled * 1.1, label = Variable),
                         color = "red", size = 3, fontface = "bold") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5)

    plots$biplot <- p_biplot
  }

  # Untuk membuat scatter plot
  if (ncol(data_reduksi) >= 3) {
    col_names <- colnames(data_reduksi)
    pc1_col <- col_names[2]  # skip 'id'
    pc2_col <- col_names[3]

    p_scatter <- buat_scatter_plot(data_reduksi, pc1_col, pc2_col,
                                   "PCA Scatter Plot", ringkasan$varians_dijelaskan,
                                   variabel_warna)
    plots$scatter_plot <- p_scatter
  }

  # Konversi ke plotly jika diminta
  if (return_plotly && requireNamespace("plotly", quietly = TRUE)) {
    if (!is.null(plots$scree_plot)) plots$scree_plot <- plotly::ggplotly(plots$scree_plot)
    if (!is.null(plots$biplot)) plots$biplot <- plotly::ggplotly(plots$biplot)
    if (!is.null(plots$scatter_plot)) plots$scatter_plot <- plotly::ggplotly(plots$scatter_plot)
  }

  return(plots)
}

# Plot untuk LDA
buat_plots_lda <- function(data_reduksi, model, ringkasan, variabel_warna,
                           target, judul, return_plotly) {
  plots <- list()

  # Untuk membuat LDA score plot
  if (ncol(data_reduksi) >= 3) {
    col_names <- colnames(data_reduksi)
    ld1_col <- col_names[2]  # skip 'id'
    ld2_col <- if(length(col_names) > 2) col_names[3] else ld1_col

    p_score <- buat_scatter_plot(data_reduksi, ld1_col, ld2_col,
                                 "LDA Score Plot", NULL, variabel_warna)
    plots$score_plot <- p_score
  }

  # Untuk membuat confusion matrix
  if (!is.null(target) && !is.null(ringkasan$prediksi)) {
    conf_matrix <- table(Actual = target, Predicted = ringkasan$prediksi)
    conf_df <- as.data.frame(conf_matrix)

    p_confusion <- ggplot2::ggplot(conf_df, ggplot2::aes(x = Predicted, y = Actual, fill = Freq)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = Freq), color = "black", size = 4) +
      ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
      ggplot2::labs(title = "Confusion Matrix",
                    x = "Predicted Class",
                    y = "Actual Class") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12))

    plots$confusion_matrix <- p_confusion
  }

  # Konversi ke plotly jika diminta
  if (return_plotly && requireNamespace("plotly", quietly = TRUE)) {
    if (!is.null(plots$score_plot)) plots$score_plot <- plotly::ggplotly(plots$score_plot)
    if (!is.null(plots$confusion_matrix)) plots$confusion_matrix <- plotly::ggplotly(plots$confusion_matrix)
  }

  return(plots)
}


# Plot untuk t-SNE
buat_plots_tsne <- function(data_reduksi, model, variabel_warna, judul, return_plotly) {
  plots <- list()

  if (ncol(data_reduksi) >= 3) {
    col_names <- colnames(data_reduksi)
    tsne1_col <- col_names[2]  # skip 'id'
    tsne2_col <- col_names[3]

    p_embedding <- buat_scatter_plot(data_reduksi, tsne1_col, tsne2_col,
                                     "t-SNE Embedding", NULL, variabel_warna)
    plots$embedding_plot <- p_embedding

    # Plot untuk melihat konvergensi t-SNE berdasarkan itercosts jika ada
    if (!is.null(model$itercosts)) {
      iter_df <- data.frame(
        Iterasi = seq_along(model$itercosts),
        KL_Divergence = model$itercosts
      )

      p_itercost <- ggplot2::ggplot(iter_df, ggplot2::aes(x = Iterasi, y = KL_Divergence)) +
        ggplot2::geom_line(color = "blue") +
        ggplot2::geom_point(color = "darkblue", size = 1) +
        ggplot2::ggtitle("Konvergensi t-SNE (KL Divergence per Iterasi)") +
        ggplot2::xlab("Iterasi") +
        ggplot2::ylab("KL Divergence") +
        ggplot2::theme_minimal()

      plots$itercost_plot <- p_itercost
    }

    # Konversi ke plotly jika diminta dan plotly tersedia
    if (return_plotly && requireNamespace("plotly", quietly = TRUE)) {
      plots$embedding_plot <- plotly::ggplotly(plots$embedding_plot)
      if (!is.null(plots$itercost_plot)) {
        plots$itercost_plot <- plotly::ggplotly(plots$itercost_plot)
      }
    }
  }

  return(plots)
}

# Plot untuk UMAP
buat_plots_umap <- function(data_reduksi, variabel_warna, judul, return_plotly) {
  plots <- list()

  if (ncol(data_reduksi) >= 3) {
    col_names <- colnames(data_reduksi)
    umap1_col <- col_names[2]  # skip 'id'
    umap2_col <- col_names[3]

    p_embedding <- buat_scatter_plot(data_reduksi, umap1_col, umap2_col,
                                     "UMAP Embedding", NULL, variabel_warna)
    plots$embedding_plot <- p_embedding

    # Konversi ke plotly jika diminta
    if (return_plotly && requireNamespace("plotly", quietly = TRUE)) {
      plots$embedding_plot <- plotly::ggplotly(plots$embedding_plot)
    }
  }

  return(plots)
}

# Plot untuk ICA
buat_plots_ica <- function(data_reduksi, model, ringkasan, variabel_warna,
                           judul, return_plotly) {
  plots <- list()

  # Untuk membuat IC plot
  if (ncol(data_reduksi) >= 3) {
    col_names <- colnames(data_reduksi)
    ic1_col <- col_names[2]  # skip 'id'
    ic2_col <- col_names[3]

    p_ic <- buat_scatter_plot(data_reduksi, ic1_col, ic2_col,
                              "Independent Components", NULL, variabel_warna)
    plots$ic_plot <- p_ic
  }

  # Untuk membuat mixing matrix heatmap
  if (!is.null(ringkasan$matriks_mixing)) {
    mixing_matrix <- ringkasan$matriks_mixing
    mixing_df <- expand.grid(Component = 1:ncol(mixing_matrix),
                             Variable = 1:nrow(mixing_matrix))
    mixing_df$Value <- as.vector(mixing_matrix)

    p_mixing <- ggplot2::ggplot(mixing_df, ggplot2::aes(x = Component, y = Variable, fill = Value)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                                    midpoint = 0, name = "Weight") +
      ggplot2::labs(title = "Mixing Matrix Heatmap",
                    x = "Independent Component",
                    y = "Original Variable") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12)) +
      ggplot2::scale_x_continuous(breaks = 1:ncol(mixing_matrix)) +
      ggplot2::scale_y_continuous(breaks = 1:nrow(mixing_matrix))

    plots$mixing_matrix <- p_mixing
  }

  # Konversi ke plotly jika diminta
  if (return_plotly && requireNamespace("plotly", quietly = TRUE)) {
    if (!is.null(plots$ic_plot)) plots$ic_plot <- plotly::ggplotly(plots$ic_plot)
    if (!is.null(plots$mixing_matrix)) plots$mixing_matrix <- plotly::ggplotly(plots$mixing_matrix)
  }

  return(plots)
}

# Fungsi untuk membuat scatter plot umum
buat_scatter_plot <- function(data_plot, x_col, y_col, judul, explained_var = NULL, variabel_warna) {

  # Buat label axis
  if (!is.null(explained_var) && length(explained_var) >= 2) {
    x_label <- paste0(x_col, " (", round(explained_var[1], 1), "%)")
    y_label <- paste0(y_col, " (", round(explained_var[2], 1), "%)")
  } else {
    x_label <- x_col
    y_label <- y_col
  }

  # Buat plot dasar
  p <- ggplot2::ggplot(data_plot) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = judul, x = x_label, y = y_label) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12))

  # Tambahkan points dengan atau tanpa warna
  if (!is.null(variabel_warna)) {
    # Tambahkan kolom warna ke data_plot
    data_plot_with_color <- data_plot
    data_plot_with_color$Warna <- variabel_warna

    p <- p + ggplot2::geom_point(data = data_plot_with_color,
                                 ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], color = Warna),
                                 alpha = 0.7, size = 2) +
      ggplot2::scale_color_viridis_d(name = "Group")
  } else {
    p <- p + ggplot2::geom_point(ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]]),
                                 alpha = 0.7, size = 2, color = "steelblue")
  }

  return(p)
}

ringkasan_reduksi <- function(hasil_reduksi) {
  if (is.null(hasil_reduksi) || !is.list(hasil_reduksi)) {
    stop("Input harus berupa hasil dari fungsi reduksi_dimensi()")
  }

  metode <- hasil_reduksi$info_metode$metode
  cat("Metode:", metode, "\n")
  cat("Jumlah komponen:", hasil_reduksi$info_metode$n_komponen, "\n")
  cat("Dimensi data hasil:", paste(dim(hasil_reduksi$data_reduksi[,-1]), collapse = " x "), "\n\n")

  if (metode == "PCA") {
    cat("Varians yang dijelaskan per komponen:\n")
    for (i in 1:length(hasil_reduksi$ringkasan$varians_dijelaskan)) {
      cat(sprintf("  PC%d: %.2f%%\n", i, hasil_reduksi$ringkasan$varians_dijelaskan[i]))
    }

    cat(sprintf("\nTotal varians yang dijelaskan: %.2f%%\n", hasil_reduksi$ringkasan$total_varians))

    cat("\nLoading matrix:\n")
    print(hasil_reduksi$ringkasan$loading)

    cat(sprintf("\nData hasil reduksi (menampilkan 5 dari %d baris):\n", nrow(hasil_reduksi$data_reduksi)))
    print(head(as.data.frame(hasil_reduksi$data_reduksi[,-1]), 5))

  } else if (metode == "LDA") {
    print(hasil_reduksi$model)

    cat(sprintf("\nData hasil reduksi (menampilkan 5 dari %d baris):\n", nrow(hasil_reduksi$data_reduksi)))
    print(head(as.data.frame(hasil_reduksi$data_reduksi[,-1]), 5))

  } else if (metode == "t-SNE") {
    cat("Perplexity yang digunakan:", hasil_reduksi$model$perplexity, "\n")
    cat("\nNilai KL Divergence akhir:", round(hasil_reduksi$ringkasan$kl_divergence, 4), "\n")

    cat(sprintf("\nData hasil reduksi (menampilkan 5 dari %d baris):\n", nrow(hasil_reduksi$data_reduksi)))
    print(head(as.data.frame(hasil_reduksi$data_reduksi[,-1]), 5))

  } else if (metode == "ICA") {
    cat("Mixing matrix:\n")
    print(hasil_reduksi$model$A)

    cat("\nUnmixing matrix:\n")
    print(hasil_reduksi$model$W)

    cat("\nWhitening matrix:\n")
    print(hasil_reduksi$model$K)

    cat(sprintf("\nData hasil reduksi (menampilkan 5 dari %d baris):\n", nrow(hasil_reduksi$data_reduksi)))
    print(head(as.data.frame(hasil_reduksi$data_reduksi[,-1]), 5))

  } else if (metode == "UMAP") {
    cat(sprintf("\nData hasil reduksi (menampilkan 5 dari %d baris):\n", nrow(hasil_reduksi$data_reduksi)))
    print(head(as.data.frame(hasil_reduksi$data_reduksi[,-1]), 5))
  }

  if (length(hasil_reduksi$validasi$peringatan) > 0) {
    cat("\nPeringatan:\n")
    for (peringatan in hasil_reduksi$validasi$peringatan) {
      cat(" -", peringatan, "\n")
    }
  }

  cat("\n")
}

ambil_plot <- function(hasil_reduksi, nama_plot = NULL) {
  if (is.null(hasil_reduksi$plots)) {
    stop("Tidak ada plot yang tersedia. Pastikan buat_plot = TRUE saat menjalankan reduksi_dimensi()")
  }

  if (is.null(nama_plot)) {
    cat("Plot yang tersedia:\n")
    for (i in 1:length(hasil_reduksi$plots)) {
      cat(" -", names(hasil_reduksi$plots)[i], "\n")
    }
    return(invisible(NULL))
  }

  if (!nama_plot %in% names(hasil_reduksi$plots)) {
    stop("Plot '", nama_plot, "' tidak ditemukan. Gunakan ambil_plot(hasil) untuk melihat plot yang tersedia.")
  }

  return(hasil_reduksi$plots[[nama_plot]])
}
