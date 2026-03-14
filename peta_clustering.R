# =============================================================================
# VISUALISASI PETA CLUSTERING KABUPATEN INDONESIA
# Berdasarkan Hasil Analisis Fuzzy C-Means (FCM)
# Sumber Data: Badan Pusat Statistik (BPS) 2024
# =============================================================================

# =============================================================================
# 1. INSTALL DAN LOAD LIBRARY
# =============================================================================

# install.packages("sf")
# install.packages("geodata")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("stringr")

library(sf)
library(geodata)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)

# =============================================================================
# 2. LOAD DATA HASIL CLUSTERING
# =============================================================================

df_hasil <- read_csv("D:/fcm_ekonomi/hasil_clustering_fcm.csv")

cat("===== DATA HASIL CLUSTERING =====\n")
cat("Jumlah kabupaten:", nrow(df_hasil), "\n")
print(head(df_hasil[, c("Wilayah", "Provinsi", "Cluster")]))

# =============================================================================
# 3. LOAD SHAPEFILE INDONESIA
# =============================================================================

cat("\n===== LOAD SHAPEFILE =====\n")

# Download shapefile kabupaten/kota Indonesia (Level 2)
# Jika sudah pernah didownload, file akan diload dari cache lokal
indo_map <- gadm(
  country = "IDN", level = 2,
  path    = "D:/fcm_ekonomi/"
)

indo_sf <- st_as_sf(indo_map)
cat("Jumlah wilayah di shapefile:", nrow(indo_sf), "\n")

# =============================================================================
# 4. STANDARISASI DAN PERBAIKAN NAMA WILAYAH
# =============================================================================

# Fungsi untuk membersihkan nama wilayah
bersihkan_nama <- function(x) {
  x %>%
    str_to_upper()          %>%
    str_trim()              %>%
    str_replace_all("KABUPATEN ", "") %>%
    str_replace_all("KOTA ", "")      %>%
    str_replace_all("KAB\\. ", "")    %>%
    str_replace_all("[^A-Z ]", "")    %>%
    str_squish()
}

# Bersihkan nama di kedua dataset
indo_sf  <- indo_sf  %>% mutate(nama_bersih = bersihkan_nama(NAME_2))
df_hasil <- df_hasil %>% mutate(nama_bersih = bersihkan_nama(Wilayah))

# Perbaikan manual nama yang tidak cocok antara BPS dan shapefile
df_hasil <- df_hasil %>%
  mutate(nama_bersih = case_when(
    
    # Sumatera Utara
    nama_bersih == "TOBA SAMOSIR TOBA"    ~ "TOBA SAMOSIR",
    nama_bersih == "LABUHAN BATU"         ~ "LABUHANBATU",
    nama_bersih == "LABUHAN BATU SELATAN" ~ "LABUHANBATU SELATAN",
    nama_bersih == "LABUHAN BATU UTARA"   ~ "LABUHANBATU UTARA",
    nama_bersih == "TANJUNG BALAI"        ~ "TANJUNGBALAI",
    nama_bersih == "PEMATANG SIANTAR"     ~ "PEMATANGSIANTAR",
    nama_bersih == "TEBING TINGGI"        ~ "TEBINGTINGGI",
    nama_bersih == "PAKPAK BHARAT"        ~ "PAKPAK BARAT",
    
    # Sumatera Barat
    nama_bersih == "SAWAH LUNTO"          ~ "SAWAHLUNTO",
    
    # Jambi
    nama_bersih == "TANJUNG JABUNG TIMUR" ~ "TANJUNG JABUNG T",
    nama_bersih == "TANJUNG JABUNG BARAT" ~ "TANJUNG JABUNG B",
    
    # Kepulauan Bangka Belitung
    nama_bersih == "PANGKAL PINANG"       ~ "PANGKALPINANG",
    
    # Kepulauan Riau
    nama_bersih == "TANJUNG PINANG"       ~ "TANJUNGPINANG",
    
    # Bali
    nama_bersih == "KARANG ASEM"          ~ "KARANGASEM",
    # Sulawesi Barat
    nama_bersih == "PASANGKAYU"  ~ "MAMUJU UTARA",
    nama_bersih == "MAMUJU UTARA" ~ "MAMUJU UTARA",
    # Maluku — nama lama di shapefile
    nama_bersih == "KEPULAUAN TANIMBAR"   ~ "MALUKU TENGGARA BARAT",
    
    # Semua nama lain tetap sama
    TRUE ~ nama_bersih
  ))

# =============================================================================
# 5. GABUNGKAN DATA CLUSTERING DENGAN SHAPEFILE
# =============================================================================

peta_data <- indo_sf %>%
  left_join(
    df_hasil %>% select(nama_bersih, Cluster),
    by = "nama_bersih"
  )

# Laporan hasil penggabungan
cat("\n===== HASIL PENGGABUNGAN =====\n")
cat("Total wilayah di shapefile  :", nrow(peta_data), "\n")
cat("Berhasil dipetakan           :", sum(!is.na(peta_data$Cluster)), "\n")
cat("Tidak terpetakan (NA)        :", sum(is.na(peta_data$Cluster)), "\n")
cat("Keterangan: wilayah NA adalah danau, waduk, dan kabupaten\n")
cat("yang dihapus saat proses cleaning karena memiliki missing value.\n")

# =============================================================================
# 6. WARNA CLUSTER
# =============================================================================

warna_peta <- c(
  "1 (Ekonomi Rendah)" = "#E74C3C",
  "2 (Ekonomi Sedang)" = "#F39C12",
  "3 (Ekonomi Tinggi)" = "#2ECC71"
)

# =============================================================================
# 7. PETA INDONESIA (NASIONAL)
# =============================================================================

plot_peta <- ggplot(data = peta_data) +
  geom_sf(aes(fill = Cluster), color = "white", linewidth = 0.05) +
  scale_fill_manual(
    values   = warna_peta,
    na.value = "grey80",
    name     = "Kategori Ekonomi",
    labels   = c(
      "1 (Ekonomi Rendah)" = "Ekonomi Rendah",
      "2 (Ekonomi Sedang)" = "Ekonomi Sedang",
      "3 (Ekonomi Tinggi)" = "Ekonomi Tinggi",
      "NA"                 = "Data Tidak Tersedia"
    )
  ) +
  labs(
    title    = "Peta Pengelompokan Kabupaten/Kota di Indonesia",
    subtitle = "Berdasarkan Indikator Ekonomi Tahun 2024 — Metode Fuzzy C-Means (c = 3)",
    caption  = "Sumber: Badan Pusat Statistik (BPS), 2024"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle   = element_text(size = 10, hjust = 0.5, color = "gray40"),
    plot.caption    = element_text(size = 8, hjust = 1, color = "gray50"),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold", size = 10),
    legend.text     = element_text(size = 9),
    legend.key.size = unit(0.8, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(10, 10, 10, 10)
  ) +
  guides(fill = guide_legend(nrow = 1))

print(plot_peta)
ggsave("plot_peta_clustering.png", plot_peta,
       width = 16, height = 8, dpi = 200, bg = "white")
cat("Peta Indonesia disimpan: plot_peta_clustering.png\n")

# =============================================================================
# 8. PETA PER PULAU
# =============================================================================

# Fungsi pembantu untuk membuat peta per pulau
buat_peta_pulau <- function(data, provinsi, judul, file) {
  peta <- data %>% filter(NAME_1 %in% provinsi)
  plot <- ggplot(data = peta) +
    geom_sf(aes(fill = Cluster), color = "white", linewidth = 0.1) +
    scale_fill_manual(
      values   = warna_peta,
      na.value = "grey80",
      name     = "Kategori Ekonomi",
      labels   = c(
        "1 (Ekonomi Rendah)" = "Ekonomi Rendah",
        "2 (Ekonomi Sedang)" = "Ekonomi Sedang",
        "3 (Ekonomi Tinggi)" = "Ekonomi Tinggi"
      )
    ) +
    labs(
      title    = judul,
      subtitle = "Berdasarkan Indikator Ekonomi Tahun 2024",
      caption  = "Sumber: BPS, 2024"
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5, color = "gray40"),
      plot.caption    = element_text(size = 8, hjust = 1, color = "gray50"),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = NA)
    )
  print(plot)
  ggsave(file, plot, width = 12, height = 7, dpi = 200, bg = "white")
  cat("Peta disimpan:", file, "\n")
}

# --- Peta Jawa ---
buat_peta_pulau(
  data     = peta_data,
  provinsi = c("Jawa Barat", "Jawa Tengah", "Jawa Timur",
               "DI Yogyakarta", "Banten", "DKI Jakarta"),
  judul    = "Peta Clustering Kabupaten/Kota — Pulau Jawa",
  file     = "plot_peta_jawa.png"
)

# --- Peta Sumatera ---
buat_peta_pulau(
  data     = peta_data,
  provinsi = c("Aceh", "Sumatera Utara", "Sumatera Barat",
               "Riau", "Kepulauan Riau", "Jambi",
               "Sumatera Selatan", "Kepulauan Bangka Belitung",
               "Bengkulu", "Lampung"),
  judul    = "Peta Clustering Kabupaten/Kota — Pulau Sumatera",
  file     = "plot_peta_sumatera.png"
)

# --- Peta Kalimantan ---
buat_peta_pulau(
  data     = peta_data,
  provinsi = c("Kalimantan Barat", "Kalimantan Tengah",
               "Kalimantan Selatan", "Kalimantan Timur",
               "Kalimantan Utara"),
  judul    = "Peta Clustering Kabupaten/Kota — Pulau Kalimantan",
  file     = "plot_peta_kalimantan.png"
)

# --- Peta Sulawesi ---
buat_peta_pulau(
  data     = peta_data,
  provinsi = c("Sulawesi Utara", "Sulawesi Tengah", "Sulawesi Selatan",
               "Sulawesi Tenggara", "Gorontalo", "Sulawesi Barat"),
  judul    = "Peta Clustering Kabupaten/Kota — Pulau Sulawesi",
  file     = "plot_peta_sulawesi.png"
)

# --- Peta Papua ---
buat_peta_pulau(
  data     = peta_data,
  provinsi = c("Papua", "Papua Barat"),
  judul    = "Peta Clustering Kabupaten/Kota — Papua",
  file     = "plot_peta_papua.png"
)

# =============================================================================
# SELESAI
# =============================================================================
cat("\n===== VISUALISASI PETA SELESAI =====\n")
cat("Output yang dihasilkan:\n")
cat("  - plot_peta_clustering.png  (Peta Indonesia)\n")
cat("  - plot_peta_jawa.png        (Peta Jawa)\n")
cat("  - plot_peta_sumatera.png    (Peta Sumatera)\n")
cat("  - plot_peta_kalimantan.png  (Peta Kalimantan)\n")
cat("  - plot_peta_sulawesi.png    (Peta Sulawesi)\n")
cat("  - plot_peta_papua.png       (Peta Papua)\n")