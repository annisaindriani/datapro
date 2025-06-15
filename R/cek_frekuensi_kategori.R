#' Cek distribusi frekuensi kolom kategorikal dalam dataset
#'
#' Fungsi ini akan menghitung distribusi frekuensi dari kolom kategorikal (character atau factor)
#' dalam dataset dan menyediakan statistik komprehensif yang terdiri dari jumlah,
#' persentase, dan juga mengidentifikasi nilai unik untuk setiap kolom kategorikal.
#'
#' @param data Sebuah data frame atau tibble untuk dianalisis.
#' @param kolom Vektor karakter berisi nama kolom yang akan dianalisis. Jika NULL (default),
#'   akan menganalisis semua kolom kategorikal dalam dataset.
#' @param max_display Jumlah maksimum kategori yang ditampilkan per kolom (default 10).
#'   Jika kolom memiliki lebih banyak nilai unik, hanya kategori teratas yang akan ditampilkan.
#' @param sort_by Urutkan hasil berdasarkan frekuensi. Pilihan yang tersedia adalah "desc" (menurun),
#'   "asc" (menaik), atau "none" (default "desc").
#' @param include_missing Logical, apakah ingin menyertakan nilai yang hilang (NA) dalam
#'   perhitungan frekuensi (default TRUE).
#' @param verbose Logis, apakah akan mencetak informasi frekuensi detail (default TRUE).
#'
#' @return List berisi analisis frekuensi untuk setiap kolom kategorikal:
#'   \item{column_name}{Untuk setiap kolom kategorikal, mengembalikan data frame dengan:
#'     \itemize{
#'       \item Value: Nilai kategorikal
#'       \item Count: Jumlah frekuensi untuk setiap nilai
#'       \item Persentase: Persentase dari setiap nilai
#'       \item Persentase_Kumulatif: Persentase kumulatif
#'     }
#'   }
#'   \item{summary}{Data frame berisi summary yang menunjukkan total nilai unik dan
#'     nilai paling sering muncul untuk setiap kolom}
#'
#' @examples
#' # Membuat data contoh
#' data <- data.frame(
#'   id = 1:100,
#'   kategori1 = sample(c("A", "B", "C", "D"), 100, replace = TRUE),
#'   kategori2 = sample(c("Merah", "Biru", "Hijau"), 100, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
#'   status = sample(c("Aktif", "Tidak Aktif", NA), 100, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
#'   kolom_numerik = rnorm(100)
#' )
#'
#' # Melakukan analisis untuk semua kolom kategorikal
#' hasil_freq <- cek_frekuensi_kategori(data)
#'
#' # Melakukan analisis untuk kolom tertentu
#' hasil_freq <- cek_frekuensi_kategori(data, kolom = c("kategori1", "status"))
#'
#' # Membatasi jumlah kategori yang ditampilkan
#' hasil_freq <- cek_frekuensi_kategori(data, max_display = 5)
#'
#' # Mengurutkan berdasarkan frekuensi menaik
#' hasil_freq <- cek_frekuensi_kategori(data, sort_by = "asc")
#'
#' # Melakukan analisis tanpa membawa nilai yang hilang
#' hasil_freq <- cek_frekuensi_kategori(data, include_missing = FALSE)
#'
#' @seealso
#' \code{\link{standarisasi_kategori}} untuk fungsi standarisasi format kategori.
#' \code{\link{konversi_data}} untuk fungsi mengubah tipe data suatu kolom.
#' \code{\link{encode_kategori}} untuk fungsi melakukan encoding pada suatu kolom kategorikal.
#' \code{\link{ubah_kategori}} untuk fungsi mengubah nilai kategorikal dalam suatu kolom.
#'
#' @export

cek_frekuensi_kategori <- function(data,
                            kolom = NULL,
                            max_display = 10,
                            sort_by = c("desc", "asc", "none"),
                            include_missing = TRUE,
                            verbose = TRUE) {

  # Untuk melihat apakah inputnya sudah berupa data frame atau tibble
  if (!is.data.frame(data)) {
    stop("Input must be a data frame or tibble.")
  }

  # Untuk memvalidasi dan mencocokkan argumen sort_by yang diberikan user sesuai dengan pilihan yang ada
  sort_by <- match.arg(sort_by)

  # Apabila tidak ada data
  if (nrow(data) == 0) {
    cat("Dataset kosong. Tidak ada konversi yang dilakukan.\n")
    return(invisible(NULL))
  }

  # Untuk mengidentifikasi kolom kategorikal
  if (is.null(kolom)) {
    kolom_kategorikal <- sapply(data, function(x) is.character(x) || is.factor(x))

    if (!any(kolom_kategorikal)) {
      cat("Tidak ada kolom kategorikal yang ditemukan dalam dataset.\n")
      return(invisible(NULL))
    }

    kolom <- names(data)[kolom_kategorikal]
  } else {
    # Untuk memvalidasi bahwa kolom yang diinput user ada
    kolom_tidak_ada <- setdiff(kolom, names(data))
    if (length(kolom_tidak_ada) > 0) {
      stop("Kolom berikut tidak ada dalam dataset: ", paste(kolom_tidak_ada, collapse = ", "))
    }

    # Untuk memvalidasi bahwa kolom yang diinput user merupakan kategorikal
    non_kategorikal <- kolom[!sapply(data[kolom], function(x) is.character(x) || is.factor(x))]
    if (length(non_kategorikal) > 0) {
      stop("Kolom berikut bukan merupakan kolom kategorikal: ",
           paste(non_kategorikal, collapse = ", "))
    }
  }

  # Untuk menyimpan hasil analisis
  result <- list()
  summary_data <- data.frame(
    Kolom = character(),
    Total_Unik = integer(),
    Most_Frequent_Value = character(),
    Most_Frequent_Count = integer(),
    Most_Frequent_Persentase = numeric(),
    stringsAsFactors = FALSE
  )

  # Untuk menganalisis setiap kolom kategorikal
  for (col_name in kolom) {
    col_data <- data[[col_name]]

    # Untuk menghitung frekuensi
    if (include_missing) {
      freq_table <- table(col_data, useNA = "ifany")
    } else {
      freq_table <- table(col_data, useNA = "no")
    }

    # Apabila tidak ada data dalam kolom tersebut
    if (length(freq_table) == 0) {
      if (verbose) {
        cat("Kolom '", col_name, "': Tidak ada data untuk dianalisis\n", sep = "")
      }
      next
    }

    freq_df <- data.frame(
      Value = names(freq_table),
      Count = as.numeric(freq_table),
      stringsAsFactors = FALSE
    )

    # Untuk menghitung persentase
    freq_df$Persentase <- round(freq_df$Count / sum(freq_df$Count) * 100, 2)

    # Untuk mengurutkan berdasarkan frekuensi
    if (sort_by == "desc") {
      freq_df <- freq_df[order(freq_df$Count, decreasing = TRUE), ]
    } else if (sort_by == "asc") {
      freq_df <- freq_df[order(freq_df$Count, decreasing = FALSE), ]
    }

    # Untuk menghitung persentase kumulatif
    freq_df$Persentase_Kumulatif <- cumsum(freq_df$Persentase)

    # Untuk membatasi jumlah kategori yang ditampilkan
    if (nrow(freq_df) > max_display) {
      displayed_df <- freq_df[1:max_display, ]
      remaining_count <- nrow(freq_df) - max_display
      if (verbose) {
        cat("Kolom '", col_name, "': Menampilkan ", max_display, " dari ", nrow(freq_df),
            " kategori (", remaining_count, " kategori lainnya tidak ditampilkan)\n", sep = "")
      }
    } else {
      displayed_df <- freq_df
    }

    # Untuk menyimpan hasil dalam list
    result[[col_name]] <- displayed_df

    # Untuk menyimpan informasi summary
    most_frequent_idx <- which.max(freq_df$Count)
    summary_row <- data.frame(
      Kolom = col_name,
      Total_Unik = nrow(freq_df),
      Most_Frequent_Value = freq_df$Value[most_frequent_idx],
      Most_Frequent_Count = freq_df$Count[most_frequent_idx],
      Most_Frequent_Persentase = freq_df$Persentase[most_frequent_idx],
      stringsAsFactors = FALSE
    )
    summary_data <- rbind(summary_data, summary_row)

    # Untuk print informasi detail jika verbose = TRUE
    if (verbose) {
      cat("Menampilkan hasil analisis frekuensi untuk kolom:", col_name, "\n")
      # Untuk mencetak tabel frekuensi
      print(displayed_df, row.names = FALSE)
      cat("\n")
    }
  }

  # Untuk menambahkan summary ke hasil
  result$summary <- summary_data

  # Untuk print summary jika verbose = TRUE
  if (verbose && nrow(summary_data) > 0) {
    cat("Menampilkan hasil analisis frekuensi untuk semua kolom:\n")
    print(summary_data, row.names = FALSE)
  }

  return(invisible(result))
}
