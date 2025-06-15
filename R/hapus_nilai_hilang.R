#' Hapus nilai yang hilang dari dataset
#'
#' Fungsi ini akan menghapus nilai yang hilang dari dataset berdasarkan opsi yang dipilih oleh pengguna.
#' Fungsi ini dapat menghapus baris dengan nilai yang hilang, kolom dengan nilai yang hilang, kombinasi keduanya, atau kolom tertentu yang dipilih.
#'
#' @param data Sebuah data frame atau tibble yang memiliki nilai yang hilang yang akan dihapus.
#' @param opsi Pilihan metode penghapusan:
#'    \itemize{
#'        \item "baris": Hapus baris yang memiliki nilai yang hilang.
#'        \item "kolom": Hapus kolom yang memiliki nilai yang hilang.
#'        \item "keduanya": Hapus baik baris maupun kolom yang memiliki nilai yang hilang.
#'        \item "kolom_tertentu": Hapus kolom tertentu yang dipilih pengguna.
#'    }
#' @param kolom_terpilih Vektor karakter berisi nama kolom yang akan dihapus (hanya digunakan Apabila opsi = "kolom_tertentu").
#' @param verbose Logis, apakah akan mencetak informasi tentang elemen yang dihapus (default TRUE).
#'
#' @return Data frame dengan nilai yang hilang yang telah dihapus sesuai dengan opsi yang dipilih.
#'   Apabila tidak ditemukan nilai yang hilang, akan mengembalikan pesan.
#'
#' @examples
#' # Membuat sampel data
#' data <- data.frame(
#'   id = 1:5,
#'   nama = c("Alice", "Bob", NA, "Dave", "Eve"),
#'   umur = c(25, 30, 22, NA, 28),
#'   skor = c(NA, 85, 90, 78, 92)
#' )
#'
#' # Hapus baris dengan nilai yang hilang
#' data_bersih_baris <- hapus_nilai_hilang(data, opsi = "baris")
#' data_bersih_baris
#'
#' # Hapus kolom dengan nilai yang hilang
#' data_bersih_kolom <- hapus_nilai_hilang(data, opsi = "kolom")
#' data_bersih_kolom
#'
#' # Hapus baris dan kolom dengan nilai yang hilang
#' data_bersih_keduanya <- hapus_nilai_hilang(data, opsi = "keduanya")
#' data_bersih_keduanya
#'
#' # Hapus kolom tertentu
#' data_bersih_tertentu <- hapus_nilai_hilang(data, opsi = "kolom_tertentu",
#'                                            kolom_terpilih = c("nama", "skor"))
#' data_bersih_tertentu
#'
#' # Hapus baris dengan nilai yang hilang tanpa output verbose
#' data_bersih <- hapus_nilai_hilang(data, opsi = "baris", verbose = FALSE)
#' data_bersih
#'
#' @seealso
#' \code{\link{deteksi_nilai_hilang}} untuk fungsi identifikasi baris dengan nilai yang hilang dari dataset.
#' \code{\link{imputasi_nilai_hilang}} untuk fungsi imputasi nilai yang hilang dalam dataset menggunakan metode biasa.
#' \code{\link{imputasi_nilai_hilang_lanjutan}} untuk fungsi imputasi nilai yang hilang dalam dataset menggunakan metode lanjutan.
#' \code{\link{hapus_duplikat}} untuk fungsi menghapus baris data duplikat dari dataset.
#' \code{\link{hapus_nilai_outlier}} untuk fungsi menghapus nilai outlier dari dataset.
#'
#' @export

hapus_nilai_hilang <- function(data, opsi = c("baris", "kolom", "keduanya", "kolom_tertentu"),
                               kolom_terpilih = NULL, verbose = TRUE) {

  # Untuk melihat input merupakan data frame atau tibble
  if (!is.data.frame(data)) {
    stop("Input harus berupa data frame atau tibble")
  }

  # Untuk validasi argumen opsi yang diberikan pengguna sesuai dengan pilihan yang tersedia
  opsi <- match.arg(opsi)

  # Apabila tidak ada data
  if (nrow(data) == 0) {
    cat("Dataset kosong. Tidak ada baris atau kolom yang dihapus.\n")
    return(invisible(data))
  }

  # Apabila tidak ada nilai yang hilang
  if (!any(is.na(data))) {
    cat("Tidak ada baris atau kolom dengan nilai yang hilang yang ditemukan dalam dataset. Tidak ada baris atau kolom yang dihapus.\n")
    return(invisible(data))
  }

  # Untuk menyimpan dimensi asli data
  baris_asli <- nrow(data)
  kolom_asli <- ncol(data)

  # Untuk menyimpan data asli dalam variabel baru
  hasil <- data

  if (opsi == "baris") {
    # Untuk identifikasi baris dengan nilai yang hilang
    baris_dengan_na <- which(apply(hasil, 1, function(x) any(is.na(x))))

    # Hapus semua baris dengan nilai yang hilang
    if (length(baris_dengan_na) > 0) {
      hasil <- hasil[-baris_dengan_na, , drop = FALSE]

      # Print informasi
      if (verbose) {
        cat(sprintf("Menghapus %d baris dengan nilai yang hilang dari total %d baris (%.1f%%)\n",
                    length(baris_dengan_na), baris_asli,
                    100 * length(baris_dengan_na) / baris_asli))
      }
    } else {
      if (verbose) {
        cat("Tidak ditemukan baris dengan nilai yang hilang.\n")
      }
    }
  } else if (opsi == "kolom") {
    # Untuk identifikasi kolom dengan nilai yang hilang
    kolom_dengan_na <- which(apply(hasil, 2, function(x) any(is.na(x))))

    # Hapus semua kolom dengan nilai yang hilang
    if (length(kolom_dengan_na) > 0) {
      kolom_dihapus <- names(hasil)[kolom_dengan_na]
      hasil <- hasil[, -kolom_dengan_na, drop = FALSE]

      # Print informasi
      if (verbose) {
        cat(sprintf("Menghapus %d kolom dengan nilai yang hilang dari total %d kolom (%.1f%%): %s\n",
                    length(kolom_dengan_na), kolom_asli,
                    100 * length(kolom_dengan_na) / kolom_asli,
                    paste(kolom_dihapus, collapse = ", ")))
      }
    } else {
      if (verbose) {
        cat("Tidak ditemukan kolom dengan nilai yang hilang.\n")
      }
    }
  } else if (opsi == "keduanya") {
    # Untuk identifikasi kolom dengan nilai yang hilang
    kolom_dengan_na <- which(apply(data, 2, function(x) any(is.na(x))))

    # Untuk identifikasi baris dengan nilai yang hilang
    baris_dengan_na <- which(apply(data, 1, function(x) any(is.na(x))))

    # Hapus semua kolom dengan nilai yang hilang
    if (length(kolom_dengan_na) > 0) {
      kolom_dihapus <- names(hasil)[kolom_dengan_na]
      hasil <- hasil[, -kolom_dengan_na, drop = FALSE]

      if (verbose) {
        cat(sprintf("Menghapus %d kolom dengan nilai yang hilang dari total %d kolom (%.1f%%): %s\n",
                    length(kolom_dengan_na), kolom_asli,
                    100 * length(kolom_dengan_na) / kolom_asli,
                    paste(kolom_dihapus, collapse = ", ")))
      }
    }

    # Hapus semua baris dengan nilai yang hilang
    if (length(baris_dengan_na) > 0) {
      hasil <- hasil[-baris_dengan_na, , drop = FALSE]

      if (verbose) {
        cat(sprintf("Menghapus %d baris dengan nilai yang hilang dari total %d baris (%.1f%%)\n",
                    length(baris_dengan_na), baris_asli,
                    100 * length(baris_dengan_na) / baris_asli))
      }
    }
  } else if (opsi == "kolom_tertentu") {
    # Validasi input kolom_terpilih
    if (is.null(kolom_terpilih)) {
      stop("Parameter 'kolom_terpilih' harus disediakan ketika opsi = 'kolom_tertentu'")
    }

    # Untuk melihat apakah kolom yang dipilih ada dalam dataset
    kolom_tidak_ada <- setdiff(kolom_terpilih, names(data))
    if (length(kolom_tidak_ada) > 0) {
      stop("Kolom berikut tidak ditemukan dalam dataset: ",
           paste(kolom_tidak_ada, collapse = ", "))
    }

    # Untuk melihat apakah kolom yang dipilih mengandung nilai yang hilang
    kolom_dengan_na <- sapply(data[kolom_terpilih], function(x) any(is.na(x)))
    kolom_tanpa_na <- names(kolom_dengan_na)[!kolom_dengan_na]
    kolom_ada_na <- names(kolom_dengan_na)[kolom_dengan_na]

    # Untuk memberi warning pada kolom yang tidak mengandung nilai yang hilang
    if (length(kolom_tanpa_na) > 0) {
      if (verbose) {
        cat(sprintf("Peringatan:\nKolom berikut tidak mengandung nilai yang hilang: %s\n",
                    paste(kolom_tanpa_na, collapse = ", ")))
      }
    }

    # Apabila tidak ada kolom yang mengandung nilai yang hilang
    if (length(kolom_ada_na) == 0) {
      if (verbose) {
        cat("Tidak ada kolom terpilih yang mengandung nilai yang hilang sehingga tidak ada kolom yang dihapus.\n")
      }
    } else {
      # Hapus hanya kolom yang mengandung nilai yang hilang
      kolom_indeks <- which(names(hasil) %in% kolom_ada_na)
      hasil <- hasil[, -kolom_indeks, drop = FALSE]

      # Print informasi
      if (verbose) {
        cat(sprintf("Menghapus %d kolom dengan nilai yang hilang dari %d kolom terpilih (%.1f%% dari total %d kolom): %s\n",
                    length(kolom_ada_na), length(kolom_terpilih),
                    100 * length(kolom_ada_na) / kolom_asli, kolom_asli,
                    paste(kolom_ada_na, collapse = ", ")))
      }
    }

    # Print informasi tambahan tentang kolom yang tidak dihapus
    if (length(kolom_tanpa_na) > 0 && verbose) {
      cat(sprintf("Kolom yang tetap dipertahankan karena tidak mengandung nilai yang hilang: %s\n",
                  paste(kolom_tanpa_na, collapse = ", ")))
    }
  }

  # Print dimensi akhir setelah proses penghapusan
  if (verbose) {
    cat(sprintf("Dimensi akhir: %d baris x %d kolom\n", nrow(hasil), ncol(hasil)))
  }

  return(invisible(hasil))
}
