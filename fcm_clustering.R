# =============================================================================
# ANALISIS CLUSTERING FUZZY C-MEANS (FCM)
# Pengelompokan Kabupaten di Indonesia Berdasarkan Indikator Ekonomi 2024
# Sumber Data: Badan Pusat Statistik (BPS)
# =============================================================================

# =============================================================================
# 1. INSTALL DAN LOAD LIBRARY
# =============================================================================

# Install library
# install.packages("ppclust")
# install.packages("factoextra")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("readr")
# install.packages("reshape2")
# install.packages("gridExtra")
# install.packages("RColorBrewer")

library(ppclust)
library(factoextra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(gridExtra)
library(RColorBrewer)

# =============================================================================
# 2. IMPORT DATA
# =============================================================================

# impor file
df <- read_csv("D:/fcm_ekonomi/Data_ekonomi_kabupaten_2024_BPS.csv")

# Tampilkan struktur data
cat("===== STRUKTUR DATA =====\n")
str(df)

# Tampilkan beberapa baris pertama
cat("\n===== 6 BARIS PERTAMA =====\n")
print(head(df))

# Dimensi data
cat("\n===== DIMENSI DATA =====\n")
cat("Jumlah baris:", nrow(df), "\n")
cat("Jumlah kolom:", ncol(df), "\n")

# =============================================================================
# 3. PREPROCESSING DATA
# =============================================================================

# Rename kolom agar lebih mudah digunakan
df <- df %>%
  rename(
    Wilayah            = `Nama Wilayah`,
    Provinsi           = Provinsi,
    IPM                = IPM,
    Kemiskinan         = `Penduduk Miskin`,
    Pengangguran       = `tingkat pengangguran`,
    PDRB               = pdrb,
    Kepadatan          = `Kepadatan Penduduk (orang/km2)`,
    RLS                = RLS,
    AHH                = AHH
  )

# Konversi kolom numerik (tangani nilai '-' sebagai NA)
variabel <- c("IPM", "Kemiskinan", "Pengangguran", "PDRB", "Kepadatan", "RLS", "AHH")

for (col in variabel) {
  df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
}

# Cek missing value
cat("\n===== CEK MISSING VALUE =====\n")
print(colSums(is.na(df)))

# Hapus baris dengan missing value
df_clean <- df %>% drop_na(all_of(variabel))
cat("\nJumlah data setelah hapus missing value:", nrow(df_clean), "baris\n")

# =============================================================================
# 4. STATISTIK DESKRIPTIF
# =============================================================================

cat("\n===== STATISTIK DESKRIPTIF =====\n")
print(summary(df_clean[, variabel]))

# =============================================================================
# 5. NORMALISASI MIN-MAX
# =============================================================================

# Fungsi normalisasi Min-Max
min_max_norm <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Terapkan normalisasi pada semua variabel
X_norm <- as.data.frame(lapply(df_clean[, variabel], min_max_norm))
rownames(X_norm) <- df_clean$Wilayah

cat("\n===== DATA SETELAH NORMALISASI (6 baris pertama) =====\n")
print(head(X_norm))

# =============================================================================
# 6. PENENTUAN JUMLAH CLUSTER OPTIMAL — ELBOW METHOD
# =============================================================================

cat("\n===== ELBOW METHOD (c = 2 hingga 8) =====\n")

set.seed(42)
J_values <- c()

for (c in 2:8) {
  hasil_fcm <- fcm(X_norm, centers = c, m = 2, nstart = 1, iter.max = 100)
  J_val <- hasil_fcm$func.val  # <-- GANTI dari obj.func ke func.val
  J_values <- c(J_values, J_val)
  cat(sprintf("c = %d | Fungsi Objektif (J) = %.4f\n", c, J_val))
}

# Tabel hasil elbow
df_elbow <- data.frame(
  c       = 2:8,
  J       = J_values
)

# Visualisasi Elbow Method
plot_elbow <- ggplot(df_elbow, aes(x = c, y = J)) +
  geom_line(color = "#2C7BB6", linewidth = 1.5) +
  geom_point(color = "#2C7BB6", size = 4, shape = 21,
             fill = "white", stroke = 2) +
  geom_point(data = df_elbow[df_elbow$c == 3, ],
             aes(x = c, y = J), color = "red", size = 5) +
  annotate("text", x = 3.3, y = df_elbow$J[df_elbow$c == 3] + 0.8,
           label = "Titik Siku\n(c = 3)", color = "red",
           fontface = "bold", size = 4) +
  scale_x_continuous(breaks = 2:8) +
  labs(
    title    = "Elbow Method — Fungsi Objektif FCM",
    subtitle = "Penentuan Jumlah Cluster Optimal",
    x        = "Jumlah Cluster (c)",
    y        = "Nilai Fungsi Objektif (J)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray50"),
    panel.grid.minor = element_blank()
  )

print(plot_elbow)
ggsave("plot_elbow.png", plot_elbow, width = 8, height = 5, dpi = 150)
cat("\nGrafik elbow disimpan: plot_elbow.png\n")

# =============================================================================
# 7. CLUSTERING FCM DENGAN c = 3
# =============================================================================

cat("\n===== CLUSTERING FCM (c = 3) =====\n")

set.seed(42)
hasil_fcm3 <- fcm(X_norm, centers = 3, m = 2, nstart = 10, iter.max = 300)

# Tampilkan pusat cluster (centroid)
cat("\n--- Pusat Cluster (Centroid) ---\n")
print(round(hasil_fcm3$v, 4))

# Tampilkan matriks keanggotaan (6 baris pertama)
cat("\n--- Matriks Keanggotaan U (6 baris pertama) ---\n")
print(round(head(hasil_fcm3$u), 4))

# =============================================================================
# 8. PENENTUAN KEANGGOTAAN AKHIR DAN PELABELAN EKONOMI
# =============================================================================

# Dapatkan keanggotaan awal dari FCM
cluster_awal <- hasil_fcm3$cluster

# Terapkan pengurutan berdasarkan IPM untuk menentukan tingkat ekonomi
# (Cluster dengan rata-rata IPM terendah = 1, tertinggi = 3)
rata_ipm <- sapply(1:3, function(i) mean(df_clean$IPM[cluster_awal == i], na.rm = TRUE))
urutan_cluster <- order(rata_ipm) 

# mapping_baru: index adalah cluster lama, valuenya adalah cluster baru (1, 2, 3)
mapping_baru <- integer(3)
mapping_baru[urutan_cluster[1]] <- 1 # Ekonomi Rendah
mapping_baru[urutan_cluster[2]] <- 2 # Ekonomi Sedang
mapping_baru[urutan_cluster[3]] <- 3 # Ekonomi Tinggi

# Sesuaikan matriks centroid agar urutannya pas
centroid_baru <- hasil_fcm3$v[urutan_cluster, ]

# Sesuaikan vektor cluster dan matriks membership ke urutan baru
cluster_baru <- mapping_baru[cluster_awal]
u_baru <- hasil_fcm3$u[, urutan_cluster]

# Gabungkan ke dataframe
label_ekonomi <- c("1 (Ekonomi Rendah)", "2 (Ekonomi Sedang)", "3 (Ekonomi Tinggi)")

df_hasil <- df_clean %>%
  mutate(
    Cluster       = factor(cluster_baru, levels = 1:3, labels = label_ekonomi),
    Member_C1     = round(u_baru[, 1], 4),
    Member_C2     = round(u_baru[, 2], 4),
    Member_C3     = round(u_baru[, 3], 4)
  )

# Distribusi kabupaten per cluster
cat("\n===== DISTRIBUSI KABUPATEN PER CLUSTER =====\n")
print(table(df_hasil$Cluster))

# =============================================================================
# 9. ANALISIS KARAKTERISTIK CLUSTER
# =============================================================================

cat("\n===== RATA-RATA VARIABEL PER CLUSTER =====\n")

df_karakteristik <- df_hasil %>%
  group_by(Cluster) %>%
  summarise(
    Jumlah       = n(),
    IPM          = round(mean(IPM, na.rm = TRUE), 2),
    Kemiskinan   = round(mean(Kemiskinan, na.rm = TRUE), 2),
    Pengangguran = round(mean(Pengangguran, na.rm = TRUE), 2),
    PDRB         = round(mean(PDRB, na.rm = TRUE), 0),
    Kepadatan    = round(mean(Kepadatan, na.rm = TRUE), 2),
    RLS          = round(mean(RLS, na.rm = TRUE), 2),
    AHH          = round(mean(AHH, na.rm = TRUE), 2)
  )

print(df_karakteristik)

# =============================================================================
# 10. VISUALISASI HASIL CLUSTERING
# =============================================================================

# Warna untuk setiap cluster
warna_cluster <- setNames(c("#E74C3C", "#2ECC71", "#3498DB"), label_ekonomi)

# --- (a) Boxplot per Variabel per Cluster ---
df_long <- df_hasil %>%
  select(Cluster, all_of(variabel)) %>%
  pivot_longer(cols = all_of(variabel), names_to = "Variabel", values_to = "Nilai")

plot_boxplot <- ggplot(df_long, aes(x = Cluster, y = Nilai, fill = Cluster)) +
  geom_boxplot(alpha = 0.8, outlier.size = 1) +
  facet_wrap(~Variabel, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = warna_cluster) +
  labs(
    title    = "Distribusi Variabel per Cluster",
    subtitle = "Hasil Clustering Fuzzy C-Means (c = 3)",
    x        = "Cluster",
    y        = "Nilai"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold"),
    legend.position = "none",
    strip.text    = element_text(face = "bold"),
    axis.text.x   = element_text(angle = 15, hjust = 1)
  )

print(plot_boxplot)
ggsave("plot_boxplot.png", plot_boxplot, width = 12, height = 7, dpi = 150)
cat("Grafik boxplot disimpan: plot_boxplot.png\n")

# --- (b) Heatmap Centroid ---
# Normalisasi centroid untuk visualisasi
centroid_df <- as.data.frame(centroid_baru)
colnames(centroid_df) <- variabel
centroid_df$Cluster <- label_ekonomi

centroid_long <- centroid_df %>%
  pivot_longer(cols = all_of(variabel), names_to = "Variabel", values_to = "Nilai")

plot_heatmap <- ggplot(centroid_long, aes(x = Variabel, y = Cluster, fill = Nilai)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = round(Nilai, 3)), size = 3.5, fontface = "bold") +
  scale_fill_gradient2(
    low      = "#3498DB",
    mid      = "#ECF0F1",
    high     = "#E74C3C",
    midpoint = 0.5,
    name     = "Nilai\n(Ternormalisasi)"
  ) +
  labs(
    title    = "Heatmap Pusat Cluster (Centroid)",
    subtitle = "Nilai Ternormalisasi — Fuzzy C-Means (c = 3)",
    x        = "Variabel",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold"),
    axis.text.x  = element_text(angle = 30, hjust = 1),
    panel.grid   = element_blank()
  )

print(plot_heatmap)
ggsave("plot_heatmap.png", plot_heatmap, width = 10, height = 4, dpi = 150)
cat("Grafik heatmap disimpan: plot_heatmap.png\n")

# --- (c) Scatter Plot IPM vs Kemiskinan ---
plot_scatter <- ggplot(df_hasil, aes(x = IPM, y = Kemiskinan, color = Cluster)) +
  geom_point(alpha = 0.7, size = 2.5) +
  scale_color_manual(values = warna_cluster) +
  labs(
    title    = "Scatter Plot: IPM vs Kemiskinan",
    subtitle = "Warna menunjukkan tingkat ekonomi kabupaten",
    x        = "IPM",
    y        = "Persentase Penduduk Miskin (%)",
    color    = "Kategori Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(plot_scatter)
ggsave("plot_scatter.png", plot_scatter, width = 8, height = 6, dpi = 150)
cat("Grafik scatter disimpan: plot_scatter.png\n")

# =============================================================================
# 11. EKSPOR HASIL DATA
# =============================================================================

# Ekspor Data Hasil Clustering Final
write_csv(df_hasil, "hasil_clustering_fcm.csv")
cat("\nHasil clustering disimpan: hasil_clustering_fcm.csv\n")

# Ekspor Rata-rata Karakteristik per Cluster
write_csv(df_karakteristik, "karakteristik_cluster.csv")
cat("Karakteristik cluster disimpan: karakteristik_cluster.csv\n")

# Ekspor Data Hasil Normalisasi Min-Max
# Untuk `X_norm`, kita perlu memasukkan kembali kolom 'Wilayah' dari row.names
df_norm_export <- X_norm
df_norm_export$Wilayah <- rownames(X_norm)
df_norm_export <- df_norm_export %>% select(Wilayah, everything())
write_csv(df_norm_export, "data_normalisasi.csv")
cat("Data setelah normalisasi disimpan: data_normalisasi.csv\n")

# =============================================================================
# SELESAI
# =============================================================================
cat("\n===== ANALISIS FCM SELESAI =====\n")
cat("Output yang dihasilkan:\n")
cat("  - plot_elbow.png\n")
cat("  - plot_boxplot.png\n")
cat("  - plot_heatmap.png\n")
cat("  - plot_scatter.png\n")
cat("  - hasil_clustering_fcm.csv\n")
cat("  - karakteristik_cluster.csv\n")
cat("  - data_normalisasi.csv\n")