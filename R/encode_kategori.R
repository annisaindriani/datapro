#' Encode kolom kategorikal menjadi format numerik
#'
#' Fungsi ini mengubah kolom kategorikal menjadi format numerik menggunakan berbagai
#' metode encoding seperti one-hot, label encoding, ordinal encoding, atau dummy variables.
#'
#' @param data Sebyag data frame atau tibble yang akan di-encode kolom kategorinya.
#' @param kolom Nama kolom kategorikal yang akan di-encode (harus berupa character string tunggal).
#' @param metode Metode encoding yang akan digunakan. Pilihan yang tersedia: "onehot" (one-hot encoding),
#'   "label" (label encoding), "ordinal" (ordinal encoding), "dummy" (dummy variables).
#'   Default "onehot".
#' @param level_ordinal Untuk ordinal encoding, vektor yang menentukan urutan kategori
#'   dari rendah ke tinggi. Jika NULL, akan menggunakan urutan alfabetis (default NULL).
#' @param kategori_referensi Untuk dummy variables, kategori yang akan dijadikan referensi
#'   (tidak akan dibuatkan kolom). Jika NULL, akan menggunakan kategori pertama secara alfabetis (default NULL).
#' @param drop_original Logis, apakah akan menghapus kolom asli setelah encoding (default TRUE).
#' @param verbose Logis, apakah akan mencetak informasi proses encoding (default TRUE).
#'
#' @return Data frame dengan kolom yang telah di-encode. Format hasil tergantung metode:
#'   \itemize{
#'     \item onehot: Beberapa kolom binary (0/1) untuk setiap kategori
#'     \item label: Satu kolom numerik dengan nilai 1, 2, 3, dst.
#'     \item ordinal: Satu kolom numerik sesuai urutan yang ditentukan
#'     \item dummy: Beberapa kolom binary, lebih sedikit dari onehot (n-1 kategori)
#'   }
#'
#' @examples
#' # Membuat sampel data
#' data <- data.frame(
#'   id = 1:10,
#'   warna = c("Merah", "Biru", "Hijau", "Merah", "Kuning",
#'             "Biru", "Hijau", "Merah", "Kuning", "Biru"),
#'   ukuran = c("Kecil", "Sedang", "Besar", "Kecil", "Besar",
#'              "Sedang", "Kecil", "Besar", "Sedang", "Kecil"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # One-hot encoding
#' data_onehot <- encode_kategori(data, "warna")
#'
#' # Label encoding
#' data_label <- encode_kategori(data, "warna", metode = "label")
#'
#' # Ordinal encoding dengan urutan khusus
#' data_ordinal <- encode_kategori(data, "ukuran", metode = "ordinal",
#'                                level_ordinal = c("Kecil", "Sedang", "Besar"))
#'
#' # Dummy variables
#' data_dummy <- encode_kategori(data, "warna", metode = "dummy")
#'
#' @seealso
#' \code{\link{cek_frekuensi_kategori}} untuk fungsi analisis frekuensi kategori.
#' \code{\link{konversi_data}} untuk fungsi mengubah tipe data suatu kolom.
#' \code{\link{ubah_kategori}} untuk fungsi mengubah nilai kategorikal dalam suatu kolom.
#' \code{\link{standarisasi_kategori}} untuk fungsi standarisasi format kategori.
#'
#' @export

encode_kategori <- function(data,
                            kolom,
                            metode = c("onehot", "label", "ordinal", "dummy"),
                            level_ordinal = NULL,
                            kategori_referensi = NULL,
                            drop_original = TRUE,
                            verbose = TRUE) {

  # Untuk melihat apakah inputnya sudah berupa data frame atau tibble
  if (!is.data.frame(data)) {
    stop("Input must be a data frame or tibble.")
  }

  # Untuk validasi parameter kolom
  if (length(kolom) != 1 || !is.character(kolom)) {
    stop("Parameter 'kolom' harus berupa character string tunggal.")
  }

  # Untuk validasi bahwa kolom ada dalam data
  if (!kolom %in% names(data)) {
    stop("Kolom '", kolom, "' tidak ditemukan dalam dataset.")
  }

  # Untuk validasi argumen metode yang diberikan user
  metode <- match.arg(metode)

  # Apabila tidak ada data
  if (nrow(data) == 0) {
    cat("Dataset kosong. Tidak ada encoding yang dilakukan.")
    return(invisible(data))
  }

  # Untuk mengambil data kolom
  col_data <- data[[kolom]]

  # Untuk memastikan kolom berupa character atau factor
  if (!is.character(col_data) && !is.factor(col_data)) {
    col_data <- as.character(col_data)
  }

  # Untuk menangani nilai yang hilang
  has_na <- any(is.na(col_data))
  if (has_na) {
    if (verbose) {
      cat("Peringatan: Ditemukan nilai NA dalam kolom '", kolom, "'. Nilai NA akan diabaikan dalam encoding.\n", sep = "")
    }
  }

  # Untuk mendapatkan unique values (tanpa NA)
  unique_values <- unique(col_data[!is.na(col_data)])
  n_categories <- length(unique_values)

  # Untuk proses encoding berdasarkan metode
  if (metode == "label") {
    sorted_values <- sort(unique_values)
    encoded_values <- match(col_data, sorted_values) - 1

    # Untuk membuat kolom baru
    new_col_name <- paste0(kolom, "_label")
    data[[new_col_name]] <- encoded_values

    if (verbose) {
      cat("Melakukan label encoding pada kolom '", kolom, "'.\n", sep = "")
      cat("Kriteria mapping kategori: ")
      for (i in 1:length(sorted_values)) {
        cat("'", sorted_values[i], "' = ", i-1, sep = "")
        if (i < length(sorted_values)) cat(", ")
      }
      cat("\n")
    }

  } else if (metode == "ordinal") {
    if (is.null(level_ordinal)) {
      level_ordinal <- sort(unique_values)
      if (verbose) {
        cat("Tidak ada urutan yang ditentukan, menggunakan urutan alfabetis.\n")
      }
    } else {
      # Untuk memvalidasi bahwa semua kategori ada dalam level_ordinal
      missing_categories <- setdiff(unique_values, level_ordinal)
      extra_categories <- setdiff(level_ordinal, unique_values)

      if (length(missing_categories) > 0) {
        stop("Kategori berikut tidak ada dalam level_ordinal: ", paste(missing_categories, collapse = ", "))
      }
      if (length(extra_categories) > 0 && verbose) {
        cat("Peringatan: Kategori berikut ada dalam level_ordinal tapi tidak ada dalam data: ",
            paste(extra_categories, collapse = ", "), "\n")
      }
    }

    encoded_values <- match(col_data, level_ordinal) - 1

    # Untuk membuat kolom baru
    new_col_name <- paste0(kolom, "_ordinal")
    data[[new_col_name]] <- encoded_values

    if (verbose) {
      cat("Melakukan ordinal encoding pada kolom '", kolom, "'.\n", sep = "")
      cat("Kriteria urutan kategori: ")
      for (i in 1:length(level_ordinal)) {
        if (level_ordinal[i] %in% unique_values) {
          cat("'", level_ordinal[i], "' = ", i-1, sep = "")
          if (i < length(level_ordinal)) cat(", ")
        }
      }
      cat("\n")
    }

  } else if (metode == "onehot") {
    sorted_values <- sort(unique_values)

    for (val in sorted_values) {
      new_col_name <- paste0(kolom, "_", val)
      # Untuk mengganti karakter yang tidak valid dalam nama kolom
      new_col_name <- gsub("[^A-Za-z0-9_]", "_", new_col_name)
      data[[new_col_name]] <- as.numeric(col_data == val)
    }

    if (verbose) {
      cat("Melakukan one-hot encoding pada kolom '", kolom, "'.\n", sep = "")
      cat("Menghasilkan ", n_categories, " kolom binary.\n", sep = "")
    }

  } else if (metode == "dummy") {
    sorted_values <- sort(unique_values)

    # Untuk menentukan kategori referensi
    if (is.null(kategori_referensi)) {
      reference_cat <- sorted_values[1]
    } else {
      # Untuk memvalidasi bahwa kategori_referensi ada dalam data
      if (!kategori_referensi %in% unique_values) {
        stop("Kategori referensi '", kategori_referensi, "' tidak ditemukan dalam kolom '", kolom, "'.")
      }
      reference_cat <- kategori_referensi
    }

    # Untuk membuat kolom dummy untuk semua kategori kecuali referensi
    categories_to_encode <- setdiff(sorted_values, reference_cat)

    for (val in categories_to_encode) {
      new_col_name <- paste0(kolom, "_", val)
      # Untuk mengganti karakter yang tidak valid dalam nama kolom
      new_col_name <- gsub("[^A-Za-z0-9_]", "_", new_col_name)
      data[[new_col_name]] <- as.numeric(col_data == val)
    }

    if (verbose) {
      cat("Melakukan dummy variables encoding pada kolom '", kolom, "'.\n", sep = "")
      cat("Menghasilkan ", length(categories_to_encode), " kolom binary.\n", sep = "")
      cat("Kriteria kategori referensi (tidak dibuat kolom): '", reference_cat, "'\n", sep = "")
    }
  }

  # Untuk menghapus kolom asli jika diminta
  if (drop_original) {
    data[[kolom]] <- NULL
    if (verbose) {
      cat("\nMenghapus kolom asli '", kolom, "'.\n", sep = "")
    }
  }

  return(invisible(data))
}
