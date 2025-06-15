#' Konversi tipe data kolom dalam dataset
#'
#' Fungsi ini akan melakukan konversi tipe data kolom yang dipilih pengguna ke tipe data target.
#' Fungsi ini dapat digunakan untuk memperbaiki tipe data yang salah dibaca saat import file CSV atau
#' format lainnya, serta mempersiapkan data untuk analisis.
#'
#' @param data Sebuah data frame atau tibble yang akan dikonversi.
#' @param kolom Vektor karakter berisi nama kolom yang akan dikonversi.
#' @param jenis_target Tipe data target untuk konversi. Pilihan yang tersedia:
#'   \itemize{
#'     \item "character": Mengkonversi ke karakter.
#'     \item "factor": Mengkonversi ke faktor.
#'     \item "numeric": Mengkonversi ke numerik.
#'     \item "logical": Mengkonversi ke logikal.
#'     \item "date": Mengkonversi ke tanggal.
#'   }
#' @param verbose Logis, apakah akan mencetak informasi detail (default TRUE).
#'
#' @return Data frame dengan kolom yang telah dikonversi ke tipe data yang diinginkan.
#'
#' @examples
#' # Membuat sampel data
#' data <- data.frame(
#'   kategori = c("A", "B", "C", "A", "B"),
#'   status = c("1", "0", "1", "0", "1"),
#'   nilai_char = c("10", "20", "30", "40", "50"),
#'   tanggal = c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Melihat struktur data
#' str(data)
#'
#' # Melakukan konversi kolom kategori ke factor
#' data_faktor <- konversi_data(data, kolom = "kategori", jenis_target = "factor")
#' str(data_faktor)
#'
#' # Melakukan konversi beberapa kolom sekaligus
#' data_multi <- konversi_data(data, kolom = c("status", "nilai_char"), jenis_target = "numeric")
#' str(data_multi)
#'
#' # Melakukan konversi kolom tanggal
#' data_tanggal <- konversi_data(data, kolom = "tanggal", jenis_target = "date")
#' str(data_tanggal)
#'
#' @seealso
#' \code{\link{cek_frekuensi_kategori}} untuk fungsi analisis frekuensi kategori.
#' \code{\link{standarisasi_kategori}} untuk fungsi standarisasi format kategori.
#' \code{\link{encode_kategori}} untuk fungsi melakukan encoding pada suatu kolom kategorikal.
#' \code{\link{ubah_kategori}} untuk fungsi mengubah nilai kategorikal dalam suatu kolom.
#'
#' @export

konversi_data <- function(data,
                              kolom,
                              jenis_target,
                              verbose = TRUE) {

  # Untuk melihat apakah inputnya sudah berupa data frame atau tibble
  if (!is.data.frame(data)) {
    stop("Input harus berupa data frame atau tibble.")
  }

  # Untuk validasi parameter jenis konversi yang dimasukkan
  valid_types <- c("character", "factor", "numeric", "logical", "date")
  if (!jenis_target %in% valid_types) {
    stop("jenis_target harus salah satu dari: ", paste(valid_types, collapse = ", "))
  }

  # Untuk validasi parameter kolom
  if (is.null(kolom) || length(kolom) == 0) {
    stop("Harus menentukan minimal satu kolom untuk dikonversi.")
  }

  # Apabila tidak ada kolom
  kolom_tidak_ada <- setdiff(kolom, names(data))
  if (length(kolom_tidak_ada) > 0) {
    stop("Kolom berikut tidak ada dalam dataset: ", paste(kolom_tidak_ada, collapse = ", "))
  }

  # Apabila tidak ada data
  if (nrow(data) == 0) {
    if (verbose) {
      cat("Dataset kosong. Tidak ada konversi yang dilakukan.\n")
    }
    return(invisible(data))
  }

  data_asli <- data
  hasil <- data

  # Untuk menyimpan beberapa format tanggal
  date_formats <- c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%Y-%m-%d %H:%M:%S", "%d-%m-%Y")

  # Untuk melakukan konversi tiap kolom
  konversi_berhasil <- character()
  konversi_gagal <- character()

  for (col in kolom) {
    data_kolom <- hasil[[col]]
    tipe_asli <- class(data_kolom)[1]

    tryCatch({
      if (jenis_target == "character") {
        hasil[[col]] <- as.character(data_kolom)

      } else if (jenis_target == "factor") {
        hasil[[col]] <- as.factor(data_kolom)

      } else if (jenis_target == "numeric") {
        # Untuk memberikan warning apabila ada yang menjadi NA ketika konversi ke numerik
        numeric_result <- suppressWarnings(as.numeric(data_kolom))

        # Untuk cek data yang gagal dikonversi
        na_asli <- sum(is.na(data_kolom))
        na_baru <- sum(is.na(numeric_result))

        if (na_baru > na_asli && verbose) {
          cat(sprintf("Peringatan: Kolom '%s' - %d nilai tidak dapat dikonversi ke numeric dan menjadi NA\n",
                      col, na_baru - na_asli))
        }

        hasil[[col]] <- numeric_result

      } else if (jenis_target == "logical") {
        if (is.character(data_kolom)) {
          data_clean <- tolower(trimws(data_kolom))
          logical_result <- ifelse(data_clean %in% c("true", "t", "yes", "y", "1"), TRUE,
                                   ifelse(data_clean %in% c("false", "f", "no", "n", "0"), FALSE, NA))
        } else {
          logical_result <- as.logical(data_kolom)
        }

        hasil[[col]] <- logical_result

      } else if (jenis_target == "date") {
        date_result <- rep(as.Date(NA), length(data_kolom))

        for (fmt in date_formats) {
          na_indices <- is.na(date_result)
          if (sum(na_indices) == 0) break

          temp_dates <- suppressWarnings(as.Date(data_kolom[na_indices], format = fmt))
          valid_dates <- !is.na(temp_dates)
          date_result[na_indices][valid_dates] <- temp_dates[valid_dates]
        }

        # Untuk cek data yang gagal dikonversi
        na_asli <- sum(is.na(data_kolom))
        na_baru <- sum(is.na(date_result))

        if (na_baru > na_asli && verbose) {
          cat(sprintf("Peringatan: Kolom '%s' - %d nilai tidak dapat dikonversi ke dalam format tanggal dan menjadi NA\n",
                      col, na_baru - na_asli))
        }

        hasil[[col]] <- date_result
      }

      konversi_berhasil <- c(konversi_berhasil, col)

    }, error = function(e) {
      if (verbose) {
        cat(sprintf("Gagal melakukan konversi tipe data pada kolom '%s':\n%s", col, e$message))
      }
      konversi_gagal <- c(konversi_gagal, col)
    })
  }

  # Untuk menampilkan hasil konversi
  if (verbose) {
    if (length(konversi_berhasil) > 0) {
      cat(sprintf("Berhasil melakukan konversi tipe data pada %d kolom:\n", length(konversi_berhasil)))

      for (col in konversi_berhasil) {
        tipe_asli <- class(data_asli[[col]])[1]
        tipe_baru <- class(hasil[[col]])[1]

        if (tipe_baru == "factor") {
          n_levels <- nlevels(hasil[[col]])
          cat(sprintf("  - %s: %s -> %s (%d levels)\n", col, tipe_asli, tipe_baru, n_levels))
        } else {
          cat(sprintf("  - %s: %s -> %s\n", col, tipe_asli, tipe_baru))
        }
      }
    }

    if (length(konversi_gagal) > 0) {
      cat(sprintf("\nGagal melakukan konversi tipe data pada %d kolom %s:\n",
                  length(konversi_gagal), paste(konversi_gagal, collapse = ", ")))
    }
  }

  return(invisible(hasil))
}
