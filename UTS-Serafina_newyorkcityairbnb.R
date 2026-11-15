# =====================================================================
# DASHBOARD EKSPLORASI & VISUALISASI DATA MULTIVARIAT
# Studi Kasus : New York City Airbnb Open Data
# UTS Gasal 2026/2027 - Eksplorasi dan Visualisasi Data Multivariat
# ---------------------------------------------------------------------
# CARA MENJALANKAN:
#   1. Simpan file ini sebagai app.R dalam satu folder dengan data_uts.xlsx
#   2. Buka app.R di RStudio lalu klik "Run App"
#      (atau di console: shiny::runApp("folder_anda"))
#   Jika data_uts.xlsx tidak ditemukan, akan muncul jendela pilih file.
# =====================================================================

# ---- 1. Paket -------------------------------------------------------
pkgs <- c("shiny", "shinydashboard", "readxl", "dplyr", "ggplot2",
          "plotly", "DT", "leaflet")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
suppressPackageStartupMessages({
  library(shiny); library(shinydashboard); library(readxl); library(dplyr)
  library(ggplot2); library(plotly); library(DT); library(leaflet)
})


# ---- Palet warna dashboard ------------------------------------------
PAL <- list(
  navy      = "#183D46",
  blue      = "#285F73",
  teal      = "purple",
  green     = "#6D9F91",
  mint      = "#A9CBB9",
  cream     = "#F3F0E6",
  sand      = "#D9C9A8",
  lightblue = "#DCE7EF",
  darkgreen = "#315B53",
  white     = "#FFFFFF",
  rose      = "#C0707F",   # aksen rose
  deeprose  = "#9E4A5E"
)
col_room <- c("Entire home/apt" = "#285F73",
              "Private room"    = "#6D9F91",
              "Shared room"     = "#D9A66C")

# skala warna fill: pakai warna room_type bila kategorinya tipe kamar,
# selain itu gradasi dari palet
fill_scale <- function(lv) {
  vals <- if (all(lv %in% names(col_room))) col_room[lv] else
    setNames(colorRampPalette(c(PAL$blue, PAL$green, PAL$sand, PAL$rose))(length(lv)), lv)
  scale_fill_manual(values = vals)
}


theme_dash <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(plot.title    = element_text(face = "bold", color = PAL$navy),
          plot.subtitle = element_text(color = PAL$teal),
          axis.title    = element_text(color = PAL$blue),
          axis.text     = element_text(color = PAL$navy),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#E6E2D6"))
}

# gaya plotly seragam: tooltip navy dengan tepi rose, tanpa logo plotly
sp <- function(p) {
  p %>%
    layout(font = list(family = "Poppins, Segoe UI, Arial, sans-serif", color = PAL$navy),
           hoverlabel = list(bgcolor = PAL$navy, bordercolor = PAL$rose,
                             font = list(color = "#FFFFFF", size = 13)),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

kpi_row <- function(prefix) {
  fluidRow(
    valueBoxOutput(paste0(prefix, "_k1"), width = 3),
    valueBoxOutput(paste0(prefix, "_k2"), width = 3),
    valueBoxOutput(paste0(prefix, "_k3"), width = 3),
    valueBoxOutput(paste0(prefix, "_k4"), width = 3)
  )
}

# ---- 2. Data --------------------------------------------------------
data_path <- "D:/kuliah/sem 7/eksplorasi & Visualisasi/septian/data uts.xlsx"
if (!file.exists(data_path)) data_path <- file.choose()

raw <- read_excel(data_path)

vars_pakai <- c("neighbourhood", "latitude", "longitude", "room_type",
                "price", "minimum_nights", "number_of_reviews",
                "availability_365")

df0 <- raw %>%
  select(all_of(vars_pakai)) %>%
  mutate(neighbourhood = as.factor(neighbourhood),
         room_type     = as.factor(room_type))

num_vars <- c("price", "minimum_nights", "number_of_reviews",
              "availability_365", "latitude", "longitude")
num4     <- c("price", "minimum_nights", "number_of_reviews",
              "availability_365")

# Ringkasan pengolahan data (dipakai di menu 1)
n_raw        <- nrow(raw)
ncol_raw     <- ncol(raw)
n_na         <- sum(is.na(df0))
n_price0     <- sum(df0$price == 0)
q_price      <- quantile(df0$price, c(.25, .75))
n_out_price  <- sum(df0$price > q_price[2] + 1.5 * diff(q_price))
n_min_ext    <- sum(df0$minimum_nights > 365)

# ---- 3. Fungsi bantu -----------------------------------------------
fm <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = ",")
fp <- function(p) ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 4))

skew <- function(x) { m <- mean(x); mean((x - m)^3) / (sqrt(mean((x - m)^2))^3) }
kurt <- function(x) { m <- mean(x); mean((x - m)^4) / (mean((x - m)^2)^2) - 3 }
mode_val <- function(x) { u <- unique(x); u[which.max(tabulate(match(x, u)))] }

desc_num <- function(x) {
  q   <- unname(quantile(x, c(.25, .5, .75)))
  iqr <- q[3] - q[1]
  out <- sum(x < q[1] - 1.5 * iqr | x > q[3] + 1.5 * iqr)
  data.frame(
    N = length(x), Mean = mean(x), Median = q[2], Modus = mode_val(x),
    SD = sd(x), Varians = var(x), CV_persen = sd(x) / abs(mean(x)) * 100,
    Min = min(x), Q1 = q[1], Q3 = q[3], Max = max(x),
    Rentang = max(x) - min(x), IQR = iqr,
    Skewness = skew(x), Kurtosis = kurt(x),
    Pencilan = out, Pct_Pencilan = out / length(x) * 100,
    row.names = NULL
  )
}

shape_txt <- function(s) {
  if (s > 1) "sangat miring ke kanan (right-skewed)"
  else if (s > 0.5) "miring ke kanan"
  else if (s < -1) "sangat miring ke kiri (left-skewed)"
  else if (s < -0.5) "miring ke kiri"
  else "relatif simetris"
}

cor_strength <- function(r) {
  a <- abs(r)
  if (a < 0.1) "sangat lemah / hampir tidak ada"
  else if (a < 0.3) "lemah"
  else if (a < 0.5) "sedang"
  else if (a < 0.7) "kuat"
  else "sangat kuat"
}

cat_tab <- function(f) {
  t <- sort(table(f), decreasing = TRUE)
  data.frame(Kategori  = names(t),
             Frekuensi = as.integer(t),
             Persen    = round(as.numeric(t) / sum(t) * 100, 2),
             Kumulatif = round(cumsum(as.numeric(t)) / sum(t) * 100, 2))
}

samp <- function(d, n = 5000) {
  set.seed(123)
  if (nrow(d) > n) d[sample.int(nrow(d), n), , drop = FALSE] else d
}

vif_manual <- function(m) {
  X <- model.matrix(m)[, -1, drop = FALSE]
  if (ncol(X) < 2) return(NULL)
  v <- diag(solve(cor(X)))
  data.frame(Variabel = colnames(X), VIF = round(v, 3), row.names = NULL)
}

# ---- 4. UI ----------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = span(icon("chart-pie"), " Airbnb NYC | EDA"), titleWidth = 300),

  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("1. Permasalahan & Dataset", tabName = "dataset",     icon = icon("database")),
      menuItem("2. Univariat",              tabName = "univariat",   icon = icon("chart-bar")),
      menuItem("3. Bivariat",               tabName = "bivariat",    icon = icon("chart-line")),
      menuItem("4. Multivariat",            tabName = "multivariat", icon = icon("cubes")),
      menuItem("5. Regresi Berganda",       tabName = "regresi",     icon = icon("calculator")),
      menuItem("6. Kesimpulan",             tabName = "kesimpulan",  icon = icon("flag-checkered"))
    ),
    hr(),
    div(style = "padding: 0 15px;",
        h4("Filter Data (berlaku di semua menu)"),
        checkboxGroupInput("f_room", "Tipe kamar (room_type)",
                           choices  = levels(df0$room_type),
                           selected = levels(df0$room_type)),
        sliderInput("f_price", "Rentang harga (USD/malam)",
                    min = 1, max = max(df0$price), value = c(1, 1000),
                    step = 1, pre = "$"),
        sliderInput("f_min", "Rentang minimum_nights",
                    min = 1, max = max(df0$minimum_nights), value = c(1, 365),
                    step = 1),
        helpText("Default: harga 0 dibuang, harga dibatasi <= $1000 dan ",
                 "minimum_nights <= 365 untuk menekan pencilan ekstrem.")
    )
  ),

  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;500;600;700&display=swap"), tags$style(HTML("
      /* ===== Tema navy ===== */
      .content-wrapper, .right-side {background-color:#183D46;}
      .skin-blue .main-header .logo {background-color:#12303A; color:#F3F0E6; font-weight:600;}
      .skin-blue .main-header .logo:hover {background-color:#12303A;}
      .skin-blue .main-header .navbar {background-color:#183D46;}
      .skin-blue .main-sidebar {background-color:#12303A;}
      .skin-blue .sidebar-menu>li>a {color:#DCE7EF;}
      .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a
        {background:#285F73; color:#FFFFFF; border-left-color:#C0707F;}
      .sidebar label, .sidebar h4, .sidebar .control-label {color:#F3F0E6;}
      .sidebar .help-block {color:#A9CBB9;}
      .sidebar hr {border-color:#285F73;}

      /* ===== Box ===== */
      .box {background:#FFFFFF; border-top:0; border-radius:4px;}
      .box.box-solid>.box-header {color:#FFFFFF;}
      .box.box-solid.box-primary {border:1px solid #285F73;}
      .box.box-solid.box-primary>.box-header {background:#285F73;}
      .box.box-solid.box-info {border:1px solid #477F7B;}
      .box.box-solid.box-info>.box-header {background:#477F7B;}
      .box.box-solid.box-success {border:1px solid #315B53;}
      .box.box-solid.box-success>.box-header {background:#315B53;}
      .box.box-solid.box-warning {border:1px solid #9E4A5E;}
      .box.box-solid.box-warning>.box-header {background:#9E4A5E;}
      .box-title {font-weight:600;}

      /* ===== Value box ===== */
      .small-box.bg-blue   {background-color:#285F73 !important;}
      .small-box.bg-aqua   {background-color:#477F7B !important;}
      .small-box.bg-green  {background-color:#6D9F91 !important;}
      .small-box.bg-yellow {background-color:#C0707F !important;}
      .small-box {color:#FFFFFF !important;}

      /* ===== Tab box ===== */
      .nav-tabs-custom {background:#F3F0E6; border-radius:4px;}
      .nav-tabs-custom>.nav-tabs>li.active {border-top-color:#C0707F;}
      .nav-tabs-custom>.nav-tabs>li.active>a {background:#FFFFFF; color:#183D46; font-weight:600;}
      .nav-tabs-custom>.nav-tabs>li>a {color:#285F73;}
      .nav-tabs-custom>.tab-content {background:#F3F0E6;}

      /* ===== Kotak interpretasi ===== */
      .interp {background:#F3F0E6; border-left:5px solid #C0707F; color:#183D46;
               padding:10px 14px; margin-top:8px; border-radius:3px;}

      /* ===== Tampilan lanjutan ===== */
      body, .main-header .logo, h1, h2, h3, h4, h5, label, .box-title, .sidebar-menu
        {font-family:'Poppins','Segoe UI',Arial,sans-serif;}
      .main-header .logo {background:linear-gradient(90deg,#12303A,#285F73) !important;
        letter-spacing:.3px;}
      .main-sidebar {background:linear-gradient(180deg,#12303A 0%,#183D46 100%) !important;}
      .sidebar-menu>li>a {border-radius:8px; margin:3px 10px; padding:11px 14px;
        transition:all .2s ease; border-left:0 !important;}
      .sidebar-menu>li.active>a {background:linear-gradient(90deg,#C0707F,#9E4A5E) !important;
        color:#fff !important; box-shadow:0 3px 10px rgba(0,0,0,.35);}
      .sidebar-menu>li:hover>a {transform:translateX(3px);}
      .box {box-shadow:0 6px 18px rgba(0,0,0,.28); transition:transform .2s, box-shadow .2s;
        overflow:hidden;}
      .box:hover {transform:translateY(-2px); box-shadow:0 10px 24px rgba(0,0,0,.38);}
      .box.box-solid>.box-header {border-bottom:3px solid #C0707F;}
      .small-box {border-radius:12px; box-shadow:0 6px 16px rgba(0,0,0,.3);
        transition:transform .2s;}
      .small-box:hover {transform:translateY(-3px);}
      .small-box h3 {font-weight:700;}
      .small-box .icon {opacity:.25;}
      .nav-tabs-custom {box-shadow:0 6px 18px rgba(0,0,0,.28); border-radius:10px;
        overflow:hidden;}
      .nav-tabs-custom>.nav-tabs>li>a {font-weight:500; padding:12px 18px;}
      .interp {box-shadow:0 2px 6px rgba(24,61,70,.12); line-height:1.6;}
      .hero {background:linear-gradient(120deg,#12303A 0%,#285F73 55%,#477F7B 100%);
        border:1px solid #477F7B; border-left:8px solid #C0707F; border-radius:14px;
        padding:28px 34px; margin-bottom:18px; color:#F3F0E6;
        box-shadow:0 10px 28px rgba(0,0,0,.4);}
      .hero h1 {margin:2px 0; font-weight:700; font-size:34px; color:#FFFFFF;}
      .hero h1.hero-sub {font-size:22px; font-weight:500; color:#A9CBB9;}
      .hero p {margin:12px 0 14px; font-size:15px; color:#DCE7EF; max-width:760px;}
      .hero-tag {display:inline-block; background:#C0707F; color:#fff; padding:3px 12px;
        border-radius:20px; font-size:12px; font-weight:600; letter-spacing:.5px;}
      .hero-chips span {display:inline-block; border:1px solid #A9CBB9; color:#F3F0E6;
        padding:3px 14px; border-radius:20px; margin-right:8px; font-size:12px;}
      table.dataTable thead th {background:#DCE7EF; color:#183D46;}
      table.dataTable tbody tr:hover {background:#F3F0E6 !important;}
      ::-webkit-scrollbar {width:9px;} ::-webkit-scrollbar-thumb {background:#477F7B; border-radius:6px;}

      /* ===== Slider ===== */
      .irs--shiny .irs-bar {background:#477F7B; border-color:#477F7B;}
      .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to
        {background:#C0707F;}
      .irs--shiny .irs-handle>i:first-child {background:#C0707F;}
      .btn-primary {background:#C0707F; border-color:#9E4A5E;}
      .btn-primary:hover {background:#9E4A5E;}
    "))),

    tabItems(

      # ============ MENU 1 : PERMASALAHAN & DATASET ============
      tabItem(tabName = "dataset",
        fluidRow(column(12,
          div(class = "hero",
            div(class = "hero-tag", "UTS Eksplorasi & Visualisasi Data Multivariat"),
            h1("New York City Airbnb"),
            h1(class = "hero-sub", "Apa yang membentuk harga sewa per malam?"),
            p("Eksplorasi univariat, bivariat, multivariat, dan regresi linear berganda
               pada puluhan ribu listing Airbnb di New York City."),
            div(class = "hero-chips",
                span("Univariat"), span("Bivariat"), span("Multivariat"), span("Regresi"))
          ))),
        fluidRow(
          valueBoxOutput("vb_n", width = 3),
          valueBoxOutput("vb_raw", width = 3),
          valueBoxOutput("vb_var", width = 3),
          valueBoxOutput("vb_na", width = 3)
        ),
        fluidRow(
          box(title = "Permasalahan & Latar Belakang", status = "primary",
              solidHeader = TRUE, width = 6,
              HTML("
              <p><b>Topik:</b> Faktor-faktor yang berkaitan dengan harga sewa listing Airbnb di Kota New York.</p>
              <p><b>Latar belakang.</b> Airbnb merupakan platform penyewaan akomodasi jangka pendek
              yang berkembang pesat di kota besar seperti New York City. Harga sewa per malam sangat
              bervariasi tergantung lokasi, tipe kamar, aturan menginap, popularitas, dan
              ketersediaan listing. Bagi pemilik (host), menentukan harga yang kompetitif sangat penting;
              bagi tamu, memahami faktor penentu harga membantu memilih akomodasi; bagi pengelola platform
              dan pemangku kebijakan, pola ini berguna untuk memahami dinamika pasar sewa jangka pendek.</p>
              <p><b>Rumusan masalah:</b></p>
              <ol>
                <li>Bagaimana karakteristik dan sebaran harga serta variabel lain pada listing Airbnb NYC?</li>
                <li>Bagaimana hubungan harga dengan tipe kamar, lokasi, jumlah ulasan, minimum malam, dan ketersediaan?</li>
                <li>Variabel apa yang berpengaruh signifikan terhadap harga dan seberapa baik model regresi menjelaskannya?</li>
              </ol>")),
          box(title = "Tujuan Analisis", status = "primary",
              solidHeader = TRUE, width = 6,
              HTML("
              <ol>
                <li>Mendeskripsikan karakteristik tiap variabel (numerik dan kategorik) secara univariat.</li>
                <li>Mengeksplorasi hubungan antar pasangan variabel (bivariat) : numerik-numerik,
                    numerik-kategorik, dan kategorik-kategorik.</li>
                <li>Mengeksplorasi pola gabungan tiga variabel atau lebih (peta spasial, bubble chart,
                    heatmap, dan PCA).</li>
                <li>Membangun model regresi linear berganda dengan <i>price</i> sebagai respons untuk
                    mengetahui pengaruh prediktor.</li>
                <li>Menyusun kesimpulan dan rekomendasi bagi host, calon tamu, dan pengelola platform.</li>
              </ol>
              <p><b>Sumber data:</b> <i>New York City Airbnb Open Data</i> (Kaggle, oleh dgomonov; berasal dari
              Inside Airbnb, data listing tahun 2019).<br>
              Tautan: <a href='https://www.kaggle.com/datasets/dgomonov/new-york-city-airbnb-open-data'
              target='_blank'>https://www.kaggle.com/datasets/dgomonov/new-york-city-airbnb-open-data</a></p>"))
        ),
        fluidRow(
          box(title = "Definisi Variabel yang Digunakan", status = "info",
              solidHeader = TRUE, width = 12,
              DTOutput("tbl_def"))
        ),
        fluidRow(
          box(title = "Pengolahan Data (data hilang, pencilan, transformasi)",
              status = "warning", solidHeader = TRUE, width = 12,
              DTOutput("tbl_clean"))
        ),
        fluidRow(
          box(title = "Pratinjau Data (setelah filter)", status = "info",
              solidHeader = TRUE, width = 12, collapsible = TRUE,
              DTOutput("tbl_preview"))
        )
      ),

      # ============ MENU 2 : UNIVARIAT ============
      tabItem(tabName = "univariat",
        kpi_row("uni"),
        tabBox(width = 12, id = "tab_uni",
          tabPanel("Variabel Numerik",
            fluidRow(
              box(title = "Statistik Deskriptif Seluruh Variabel Numerik",
                  status = "primary", solidHeader = TRUE, width = 12,
                  p("Ukuran yang disajikan: ", tags$b("pemusatan"), " (mean, median, modus), ",
                    tags$b("penyebaran"), " (SD, varians, CV, rentang, IQR), ",
                    tags$b("posisi"), " (min, Q1, Q3, maks), ",
                    tags$b("bentuk sebaran"), " (skewness, kurtosis), dan ",
                    tags$b("pencilan"), " (aturan 1,5 x IQR)."),
                  DTOutput("tbl_num_desc"))
            ),
            fluidRow(
              box(title = "Pengaturan", status = "info", solidHeader = TRUE, width = 3,
                  selectInput("uni_var", "Pilih variabel numerik", choices = num_vars),
                  sliderInput("uni_bins", "Jumlah bin histogram", 10, 100, 40),
                  checkboxInput("uni_log", "Skala log (log1p) untuk visual", FALSE),
                  helpText("Transformasi hanya mengubah tampilan grafik, bukan data.")),
              box(title = "Histogram", status = "primary", solidHeader = TRUE, width = 5,
                  plotlyOutput("uni_hist", height = 330)),
              box(title = "Boxplot", status = "primary", solidHeader = TRUE, width = 4,
                  plotlyOutput("uni_box", height = 330))
            ),
            fluidRow(
              box(title = "Q-Q Plot (uji kenormalan secara visual)", status = "primary",
                  solidHeader = TRUE, width = 5, plotlyOutput("uni_qq", height = 320)),
              box(title = "Interpretasi", status = "success", solidHeader = TRUE, width = 7,
                  uiOutput("uni_num_text"))
            )
          ),
          tabPanel("Variabel Kategorik",
            fluidRow(
              box(title = "Pengaturan", status = "info", solidHeader = TRUE, width = 3,
                  selectInput("uni_cat", "Pilih variabel kategorik",
                              choices = c("room_type", "neighbourhood")),
                  conditionalPanel("input.uni_cat == 'neighbourhood'",
                    sliderInput("uni_topn", "Tampilkan N kategori teratas", 5, 30, 15)),
                  helpText("Ukuran untuk kategorik: tabel frekuensi, proporsi (persen), ",
                           "persen kumulatif, modus, dan jumlah kategori.")),
              box(title = "Diagram Batang Frekuensi", status = "primary",
                  solidHeader = TRUE, width = 9, plotlyOutput("uni_bar", height = 400))
            ),
            fluidRow(
              box(title = "Tabel Frekuensi", status = "primary", solidHeader = TRUE,
                  width = 6, DTOutput("tbl_cat")),
              box(title = "Interpretasi", status = "success", solidHeader = TRUE,
                  width = 6, uiOutput("uni_cat_text"))
            )
          )
        )
      ),

      # ============ MENU 3 : BIVARIAT ============
      tabItem(tabName = "bivariat",
        kpi_row("bi"),
        tabBox(width = 12, id = "tab_bi",

          tabPanel("A. Numerik - Numerik",
            fluidRow(
              box(title = "Pengaturan", status = "info", solidHeader = TRUE, width = 3,
                  selectInput("bva_x", "Variabel X", choices = num4, selected = "availability_365"),
                  selectInput("bva_y", "Variabel Y", choices = num4, selected = "price"),
                  checkboxInput("bva_log", "Skala log (log1p) untuk X dan Y", TRUE),
                  radioButtons("bva_sm", "Garis tren",
                               choices = c("Linear" = "lm", "LOESS" = "loess")),
                  helpText("Plot menggunakan sampel acak maks. 5.000 titik agar ringan; ",
                           "korelasi dihitung dari seluruh data hasil filter.")),
              box(title = "Scatter Plot", status = "primary", solidHeader = TRUE, width = 9,
                  plotlyOutput("bva_plot", height = 400))
            ),
            fluidRow(
              box(title = "Koefisien Korelasi", status = "primary", solidHeader = TRUE,
                  width = 4, tableOutput("bva_cor")),
              box(title = "Interpretasi", status = "success", solidHeader = TRUE,
                  width = 8, uiOutput("bva_text"))
            ),
            fluidRow(
              box(title = "Heatmap Matriks Korelasi (seluruh variabel numerik)",
                  status = "primary", solidHeader = TRUE, width = 8,
                  plotlyOutput("bva_heat", height = 420)),
              box(title = "Pengaturan & Interpretasi", status = "success",
                  solidHeader = TRUE, width = 4,
                  radioButtons("bva_cm", "Metode korelasi",
                               choices = c("Spearman" = "spearman", "Pearson" = "pearson")),
                  uiOutput("bva_heat_text"))
            )
          ),

          tabPanel("B. Numerik - Kategorik",
            fluidRow(
              box(title = "Pengaturan", status = "info", solidHeader = TRUE, width = 3,
                  selectInput("bvb_var", "Variabel numerik", choices = num4),
                  selectInput("bvb_grp", "Variabel kelompok",
                              choices = c("room_type", "neighbourhood")),
                  conditionalPanel("input.bvb_grp == 'neighbourhood'",
                    sliderInput("bvb_top", "N neighbourhood terbanyak", 5, 15, 8)),
                  checkboxInput("bvb_log", "Skala log (log1p)", TRUE)),
              box(title = "Boxplot per Kelompok", status = "primary", solidHeader = TRUE,
                  width = 9, plotlyOutput("bvb_plot", height = 420))
            ),
            fluidRow(
              box(title = "Ringkasan per Kelompok (skala asli)", status = "primary",
                  solidHeader = TRUE, width = 7, DTOutput("bvb_tbl")),
              box(title = "Interpretasi (uji Kruskal-Wallis)", status = "success",
                  solidHeader = TRUE, width = 5, uiOutput("bvb_text"))
            )
          ),

          tabPanel("C. Kategorik - Kategorik",
            fluidRow(
              box(title = "Pengaturan", status = "info", solidHeader = TRUE, width = 3,
                  sliderInput("bvc_top", "N neighbourhood terbanyak", 5, 20, 10),
                  radioButtons("bvc_pos", "Tipe batang",
                               choices = c("Proporsi (100%)" = "fill", "Frekuensi" = "stack"))),
              box(title = "Neighbourhood x Room Type", status = "primary",
                  solidHeader = TRUE, width = 9, plotlyOutput("bvc_plot", height = 430))
            ),
            fluidRow(
              box(title = "Tabel Kontingensi", status = "primary", solidHeader = TRUE,
                  width = 6, DTOutput("bvc_tbl")),
              box(title = "Interpretasi (uji Chi-Square & Cramer's V)", status = "success",
                  solidHeader = TRUE, width = 6, uiOutput("bvc_text"))
            )
          )
        )
      ),

      # ============ MENU 4 : MULTIVARIAT ============
      tabItem(tabName = "multivariat",
        kpi_row("mv"),
        tabBox(width = 12, id = "tab_mv",

          tabPanel("A. Peta Spasial (lat, lon, tipe/harga)",
            fluidRow(
              box(title = "Pengaturan", status = "info", solidHeader = TRUE, width = 3,
                  radioButtons("map_col", "Warna titik berdasarkan",
                               choices = c("Tipe kamar" = "room_type",
                                           "Harga (kuantil)" = "price")),
                  sliderInput("map_n", "Jumlah titik (sampel)", 500, 5000, 2500, step = 500),
                  helpText("Melibatkan 3+ variabel: latitude, longitude, room_type / price, ",
                           "neighbourhood (popup).")),
              box(title = "Sebaran Spasial Listing", status = "primary", solidHeader = TRUE,
                  width = 9, leafletOutput("mv_map", height = 480))
            ),
            fluidRow(box(title = "Interpretasi", status = "success", solidHeader = TRUE,
                         width = 12, uiOutput("mv_map_text")))
          ),

          tabPanel("B. Bubble Chart (4 variabel)",
            fluidRow(
              box(title = "Pengaturan", status = "info", solidHeader = TRUE, width = 3,
                  selectInput("mvb_x", "Sumbu X", choices = num4, selected = "availability_365"),
                  selectInput("mvb_y", "Sumbu Y", choices = num4, selected = "price"),
                  selectInput("mvb_s", "Ukuran gelembung", choices = num4,
                              selected = "number_of_reviews"),
                  checkboxInput("mvb_log", "Skala log (log1p) untuk X dan Y", TRUE),
                  helpText("Warna = room_type. Sampel acak maks. 3.000 titik.")),
              box(title = "Bubble Chart", status = "primary", solidHeader = TRUE,
                  width = 9, plotlyOutput("mv_bubble", height = 470))
            ),
            fluidRow(box(title = "Interpretasi", status = "success", solidHeader = TRUE,
                         width = 12, uiOutput("mv_bubble_text")))
          ),

          tabPanel("C. Heatmap Median Harga",
            fluidRow(
              box(title = "Pengaturan", status = "info", solidHeader = TRUE, width = 3,
                  sliderInput("mvh_top", "N neighbourhood terbanyak", 5, 25, 15),
                  helpText("Median harga untuk kombinasi neighbourhood x room_type ",
                           "(3 variabel: neighbourhood, room_type, price).")),
              box(title = "Heatmap Median Harga", status = "primary", solidHeader = TRUE,
                  width = 9, plotlyOutput("mv_heat", height = 480))
            ),
            fluidRow(box(title = "Interpretasi", status = "success", solidHeader = TRUE,
                         width = 12, uiOutput("mv_heat_text")))
          ),

          tabPanel("D. PCA (Biplot)",
            fluidRow(
              box(title = "Pengaturan", status = "info", solidHeader = TRUE, width = 3,
                  checkboxInput("pca_log", "Log-kan price, minimum_nights, number_of_reviews", TRUE),
                  helpText("PCA dilakukan pada 6 variabel numerik yang telah distandarisasi."),
                  tableOutput("pca_var")),
              box(title = "Biplot PC1 vs PC2", status = "primary", solidHeader = TRUE,
                  width = 5, plotlyOutput("pca_biplot", height = 430)),
              box(title = "Scree Plot", status = "primary", solidHeader = TRUE,
                  width = 4, plotlyOutput("pca_scree", height = 430))
            ),
            fluidRow(box(title = "Interpretasi", status = "success", solidHeader = TRUE,
                         width = 12, uiOutput("pca_text")))
          )
        )
      ),

      # ============ MENU 5 : REGRESI BERGANDA ============
      tabItem(tabName = "regresi",
        kpi_row("reg"),
        fluidRow(
          box(title = "Spesifikasi Model", status = "info", solidHeader = TRUE, width = 4,
              p(tags$b("Respons (Y): "), "price (harga per malam)."),
              checkboxGroupInput("preds", "Prediktor (X)",
                  choices  = c("minimum_nights", "number_of_reviews", "availability_365",
                               "room_type", "latitude", "longitude"),
                  selected = c("minimum_nights", "number_of_reviews", "availability_365",
                               "room_type", "latitude", "longitude")),
              checkboxInput("reg_logy", "Transformasi log pada respons: ln(price)", TRUE),
              checkboxInput("reg_logx", "Transformasi log1p pada minimum_nights & number_of_reviews", FALSE),
              helpText("Kategori acuan room_type: Entire home/apt. Pilih minimal 2 prediktor.")),
          box(title = "Alasan Pemilihan Variabel", status = "info", solidHeader = TRUE, width = 8,
              HTML("
              <ul>
              <li><b>Respons: price</b> - variabel numerik kontinu yang menjadi fokus masalah
                  (penentuan harga sewa).</li>
              <li><b>room_type</b> (kategorik) - tipe kamar menentukan kapasitas dan privasi sehingga diduga
                  mempengaruhi harga; diubah menjadi variabel dummy.</li>
              <li><b>latitude &amp; longitude</b> - proksi lokasi; harga umumnya berbeda antar kawasan.
                  (<i>neighbourhood</i> tidak dimasukkan karena memiliki 200+ kategori sehingga
                  menghasilkan ratusan dummy dan model sulit ditafsirkan.)</li>
              <li><b>minimum_nights</b> - aturan minimal menginap; listing jangka panjang cenderung berbeda harga.</li>
              <li><b>number_of_reviews</b> - proksi popularitas / lamanya listing aktif.</li>
              <li><b>availability_365</b> - proksi tingkat ketersediaan/pemanfaatan listing.</li>
              </ul>
              <p>Transformasi log pada respons dianjurkan karena <i>price</i> sangat miring ke kanan,
              sehingga membantu memenuhi asumsi kenormalan dan kehomogenan ragam sisaan.</p>"))
        ),
        tabBox(width = 12, id = "tab_reg",
          tabPanel("Persamaan & Ukuran Model",
            fluidRow(
              box(title = "Persamaan Model Terestimasi", status = "primary",
                  solidHeader = TRUE, width = 12, uiOutput("reg_eq"))),
            fluidRow(
              box(title = "Ukuran Kebaikan Model", status = "primary", solidHeader = TRUE,
                  width = 5, tableOutput("reg_fit")),
              box(title = "Interpretasi Kemampuan Model & Uji Simultan (Uji F)",
                  status = "success", solidHeader = TRUE, width = 7, uiOutput("reg_fit_text")))
          ),
          tabPanel("Estimasi Koefisien & Uji Parsial",
            fluidRow(
              box(title = "Tabel Koefisien (uji t, selang kepercayaan 95%)", status = "primary",
                  solidHeader = TRUE, width = 12, DTOutput("reg_coef"))),
            fluidRow(
              box(title = "Interpretasi Koefisien", status = "success", solidHeader = TRUE,
                  width = 12, uiOutput("reg_coef_text")))
          ),
          tabPanel("Uji Asumsi",
            fluidRow(
              box(title = "Residual vs Fitted", status = "primary", solidHeader = TRUE,
                  width = 6, plotlyOutput("reg_rf", height = 340)),
              box(title = "Q-Q Plot Residual", status = "primary", solidHeader = TRUE,
                  width = 6, plotlyOutput("reg_qq", height = 340))),
            fluidRow(
              box(title = "Multikolinearitas (VIF) & Uji Breusch-Pagan", status = "primary",
                  solidHeader = TRUE, width = 5, tableOutput("reg_vif"), uiOutput("reg_bp")),
              box(title = "Interpretasi Asumsi", status = "success", solidHeader = TRUE,
                  width = 7, uiOutput("reg_asm_text")))
          ),
          tabPanel("Visualisasi Hasil Model",
            fluidRow(
              box(title = "Aktual vs Prediksi", status = "primary", solidHeader = TRUE,
                  width = 6, plotlyOutput("reg_av", height = 380)),
              box(title = "Efek Marginal Prediktor (variabel lain ditahan pada rata-rata)",
                  status = "primary", solidHeader = TRUE, width = 6,
                  uiOutput("reg_eff_sel"), plotlyOutput("reg_eff", height = 330))),
            fluidRow(
              box(title = "Interpretasi", status = "success", solidHeader = TRUE,
                  width = 12, uiOutput("reg_vis_text")))
          ),
          tabPanel("Prediksi Harga",
            fluidRow(
              box(title = "Masukkan Karakteristik Listing", status = "info",
                  solidHeader = TRUE, width = 4,
                  selectInput("pr_room", "room_type", choices = levels(df0$room_type)),
                  numericInput("pr_min", "minimum_nights", 3, min = 1),
                  numericInput("pr_rev", "number_of_reviews", 10, min = 0),
                  numericInput("pr_av", "availability_365", 100, min = 0, max = 365),
                  numericInput("pr_lat", "latitude", 40.73, step = 0.01),
                  numericInput("pr_lon", "longitude", -73.95, step = 0.01),
                  actionButton("btn_pred", "Prediksi", class = "btn-primary")),
              box(title = "Hasil Prediksi", status = "success", solidHeader = TRUE,
                  width = 8, uiOutput("pr_out")))
          ),
          tabPanel("Keterbatasan Model",
            box(title = "Keterbatasan Model Regresi Linear Berganda yang Digunakan",
                status = "warning", solidHeader = TRUE, width = 12,
                HTML("
                <ol>
                <li><b>Asumsi klasik</b> (normalitas, homoskedastisitas, linearitas) kerap tidak terpenuhi sempurna
                    pada data harga yang miring dan berpencilan; transformasi log hanya mengurangi, tidak
                    menghilangkan masalah tersebut.</li>
                <li><b>Lokasi hanya diwakili latitude dan longitude secara linear</b>, padahal pengaruh lokasi
                    bersifat non-linear/berkelompok (mis. pusat Manhattan). <i>neighbourhood</i> tidak dimasukkan.</li>
                <li><b>Variabel penting tidak tersedia</b> pada model: fasilitas, ukuran/jumlah kamar, kualitas dan
                    skor ulasan, musim, foto, status host, dsb. sehingga R<sup>2</sup> terbatas dan terdapat
                    <i>omitted variable bias</i>.</li>
                <li><b>Observasi tidak independen sepenuhnya</b>: satu host dapat memiliki banyak listing dan
                    listing berdekatan cenderung berkorelasi spasial.</li>
                <li><b>Hubungan bersifat asosiasi, bukan sebab-akibat.</b></li>
                <li><b>Data hanya snapshot satu periode</b> (2019) dan telah difilter (default price &le; $1000,
                    minimum_nights &le; 365), sehingga kesimpulan tidak mencakup listing ekstrem.</li>
                <li><b>Ukuran sampel besar</b> membuat hampir semua koefisien signifikan secara statistik; perlu
                    dilihat juga besar efek (praktis), bukan hanya nilai-p.</li>
                </ol>"))
          )
        )
      ),

      # ============ MENU 6 : KESIMPULAN ============
      tabItem(tabName = "kesimpulan",
        fluidRow(
          box(title = "Kesimpulan Utama", status = "primary", solidHeader = TRUE,
              width = 12, uiOutput("kes_main"))),
        fluidRow(
          box(title = "Rekomendasi", status = "success", solidHeader = TRUE,
              width = 12, uiOutput("kes_rec")))
      )
    )
  )
)

# ---- 5. SERVER ------------------------------------------------------
server <- function(input, output, session) {

  # ---------- data terfilter ----------
  dat <- reactive({
    req(input$f_room, input$f_price, input$f_min)
    d <- df0 %>%
      filter(room_type %in% input$f_room,
             price >= input$f_price[1], price <= input$f_price[2],
             minimum_nights >= input$f_min[1], minimum_nights <= input$f_min[2])
    validate(need(nrow(d) > 50,
                  "Data hasil filter < 50 observasi. Longgarkan filter di sidebar."))
    d$room_type <- droplevels(d$room_type)
    d
  })


  # ---------- KPI strip di tiap menu ----------
  for (pfx in c("uni", "bi", "mv", "reg")) local({
    p0 <- pfx
    output[[paste0(p0, "_k1")]] <- renderValueBox(
      valueBox(fm(nrow(dat()), 0), "Listing dianalisis", icon = icon("home"), color = "blue"))
    output[[paste0(p0, "_k2")]] <- renderValueBox(
      valueBox(paste0("$", fm(median(dat()$price), 0)), "Median harga per malam",
               icon = icon("dollar-sign"), color = "aqua"))
    output[[paste0(p0, "_k3")]] <- renderValueBox(
      valueBox(fm(mean(dat()$availability_365), 0), "Rata-rata hari tersedia / tahun",
               icon = icon("calendar"), color = "green"))
    output[[paste0(p0, "_k4")]] <- renderValueBox(
      valueBox(fm(nlevels(droplevels(dat()$neighbourhood)), 0), "Neighbourhood",
               icon = icon("map-pin"), color = "yellow"))
  })

  # ================== MENU 1 ==================
  output$vb_n   <- renderValueBox(valueBox(fm(nrow(dat()), 0), "Observasi (setelah filter)",
                                           icon = icon("list"), color = PAL$teal))
  output$vb_raw <- renderValueBox(valueBox(fm(n_raw, 0), "Observasi data asli",
                                           icon = icon("database"), color = "aqua"))
  output$vb_var <- renderValueBox(valueBox(paste(length(vars_pakai), "dari", ncol_raw),
                                           "Variabel digunakan / total variabel",
                                           icon = icon("table"), color = "green"))
  output$vb_na  <- renderValueBox(valueBox(n_na, "Data hilang (8 variabel terpilih)",
                                           icon = icon("exclamation-triangle"), color = "yellow"))

  output$tbl_def <- renderDT({
    def <- data.frame(
      Variabel = vars_pakai,
      Tipe = c("Kategorik (nominal)", "Numerik (kontinu)", "Numerik (kontinu)",
               "Kategorik (nominal)", "Numerik (rasio, respons)", "Numerik (diskrit)",
               "Numerik (diskrit)", "Numerik (diskrit)"),
      Definisi = c(
        paste0("Nama lingkungan/kawasan lokasi listing (", nlevels(df0$neighbourhood), " kategori)."),
        "Koordinat lintang lokasi listing (derajat).",
        "Koordinat bujur lokasi listing (derajat).",
        paste0("Tipe listing: ", paste(levels(df0$room_type), collapse = ", "), "."),
        "Harga sewa per malam (USD).",
        "Jumlah minimum malam yang harus dipesan tamu.",
        "Jumlah total ulasan yang diterima listing.",
        "Jumlah hari listing tersedia untuk dipesan dalam setahun (0-365)."),
      Peran = c("Eksplorasi", "Eksplorasi / prediktor", "Eksplorasi / prediktor",
                "Prediktor", "Respons", "Prediktor", "Prediktor", "Prediktor"))
    datatable(def, rownames = FALSE, options = list(dom = "t", pageLength = 10))
  })

  output$tbl_clean <- renderDT({
    cl <- data.frame(
      Aspek = c("Data hilang", "Harga = 0", "Pencilan harga",
                "minimum_nights ekstrem", "Sebaran miring", "Variabel kategorik"),
      Temuan = c(
        paste0(n_na, " nilai hilang pada 8 variabel terpilih."),
        paste0(n_price0, " listing berharga $0."),
        paste0(fm(n_out_price, 0), " listing (", fm(n_out_price / n_raw * 100, 1),
               "%) di atas batas pencilan 1,5xIQR; harga maks = $", fm(max(df0$price), 0), "."),
        paste0(n_min_ext, " listing dengan minimum_nights > 365 (maks = ",
               max(df0$minimum_nights), ")."),
        "price, minimum_nights, dan number_of_reviews sangat miring ke kanan.",
        paste0("neighbourhood memiliki ", nlevels(df0$neighbourhood), " kategori.")),
      Penanganan = c(
        "Tidak diperlukan imputasi.",
        "Dibuang (filter harga minimum = $1) karena tidak masuk akal dan tidak dapat di-log.",
        "Default filter harga <= $1000; dapat diubah di sidebar. Pada regresi dipakai ln(price).",
        "Default filter minimum_nights <= 365 (data dengan nilai tidak realistis).",
        "Log/log1p opsional pada visualisasi dan regresi.",
        "Visualisasi dan analisis memakai N kategori terbanyak."))
    datatable(cl, rownames = FALSE, options = list(dom = "t"))
  })

  output$tbl_preview <- renderDT({
    datatable(head(dat(), 200), rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 8))
  })

  # ================== MENU 2 : UNIVARIAT ==================
  output$tbl_num_desc <- renderDT({
    d <- dat()
    tb <- do.call(rbind, lapply(num_vars, function(v) cbind(Variabel = v, desc_num(d[[v]]))))
    tb <- tb %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
    datatable(tb, rownames = FALSE,
              options = list(scrollX = TRUE, dom = "t", pageLength = 10))
  })

  uni_x <- reactive({
    x <- dat()[[input$uni_var]]
    if (isTRUE(input$uni_log)) log1p(x) else x
  })
  uni_lab <- reactive(if (isTRUE(input$uni_log)) paste0("log(1 + ", input$uni_var, ")") else input$uni_var)

  output$uni_hist <- renderPlotly({
    x <- uni_x()
    h <- hist(x, breaks = input$uni_bins, plot = FALSE)
    df <- data.frame(mid = h$mids, n = h$counts,
                     lo = h$breaks[-length(h$breaks)], hi = h$breaks[-1])
    df$txt <- paste0("Rentang: ", fm(df$lo, 2), " sampai ", fm(df$hi, 2),
                     "<br>Jumlah listing: ", fm(df$n, 0),
                     "<br>Persentase: ", fm(df$n / sum(df$n) * 100, 2), "%")
    p <- ggplot(df, aes(mid, n, text = txt)) +
      geom_col(width = diff(h$breaks)[1] * 0.95, fill = PAL$blue) +
      geom_vline(xintercept = mean(x), color = PAL$rose, linetype = "dashed") +
      geom_vline(xintercept = median(x), color = PAL$darkgreen, linetype = "dotted", linewidth = 1) +
      labs(title = paste("Histogram", input$uni_var),
           subtitle = "Rose putus-putus = rata-rata; hijau titik = median",
           x = uni_lab(), y = "Jumlah listing") +
      theme_dash()
    sp(ggplotly(p, tooltip = "text"))
  })

  output$uni_box <- renderPlotly({
    x <- uni_x()
    plot_ly(y = x, type = "box", name = input$uni_var, boxpoints = "outliers",
            marker = list(color = PAL$rose, size = 3, opacity = 0.4),
            line = list(color = PAL$blue), fillcolor = "rgba(71,127,123,0.45)") %>%
      layout(title = list(text = paste("<b>Boxplot", input$uni_var, "</b>")),
             yaxis = list(title = uni_lab()), showlegend = FALSE) %>%
      sp()
  })

  output$uni_qq <- renderPlotly({
    x <- sort(uni_x()); n <- length(x)
    idx <- unique(round(seq(1, n, length.out = min(n, 3000))))
    th <- qnorm(ppoints(n))[idx]; sm <- x[idx]
    q <- unname(quantile(x, c(.25, .75))); tq <- qnorm(c(.25, .75))
    slope <- diff(q) / diff(tq); int <- q[1] - slope * tq[1]
    plot_ly() %>%
      add_markers(x = th, y = sm, name = "Data",
                  marker = list(color = PAL$teal, opacity = 0.5, size = 5),
                  text = paste0("Kuantil teoretis: ", fm(th, 2), "<br>Nilai sampel: ", fm(sm, 2)),
                  hoverinfo = "text") %>%
      add_lines(x = range(th), y = int + slope * range(th), name = "Garis normal",
                line = list(color = PAL$rose, dash = "dash"), hoverinfo = "none") %>%
      layout(title = list(text = paste("<b>Q-Q Plot", input$uni_var, "</b>")),
             xaxis = list(title = "Kuantil teoretis (Normal)"),
             yaxis = list(title = "Kuantil sampel"), showlegend = FALSE) %>%
      sp()
  })

  output$uni_num_text <- renderUI({
    x <- dat()[[input$uni_var]]; s <- desc_num(x)
    arah <- if (s$Mean > s$Median * 1.05) "Rata-rata lebih besar dari median sehingga data terseret nilai-nilai besar."
            else if (s$Mean < s$Median * 0.95) "Rata-rata lebih kecil dari median sehingga data terseret nilai-nilai kecil."
            else "Rata-rata dan median relatif berdekatan."
    kurt_t <- if (s$Kurtosis > 1) "berekor tebal (banyak nilai ekstrem)" else if (s$Kurtosis < -1) "berekor tipis" else "mendekati normal pada ekor"
    div(class = "interp", HTML(sprintf(
      "Variabel <b>%s</b> memiliki rata-rata <b>%s</b>, median <b>%s</b>, dan modus <b>%s</b>. %s
       Sebaran data <b>%s</b> (skewness = %s) dan %s (kurtosis berlebih = %s).
       Simpangan baku %s dengan koefisien variasi %s%%, rentang %s sampai %s, IQR = %s.
       Terdapat <b>%s pencilan (%s%%)</b> menurut aturan 1,5xIQR. Pada histogram, nilai pencilan
       tampak sebagai ekor panjang, dan pada boxplot sebagai titik-titik di luar kumis.
       Q-Q plot yang menyimpang dari garis merah menunjukkan sebaran tidak normal.",
      input$uni_var, fm(s$Mean, 2), fm(s$Median, 2), fm(s$Modus, 2), arah,
      shape_txt(s$Skewness), fm(s$Skewness, 2), kurt_t, fm(s$Kurtosis, 2),
      fm(s$SD, 2), fm(s$CV_persen, 1), fm(s$Min, 2), fm(s$Max, 2), fm(s$IQR, 2),
      fm(s$Pencilan, 0), fm(s$Pct_Pencilan, 2))))
  })

  cat_data <- reactive({
    d <- dat()
    if (input$uni_cat == "room_type") {
      cat_tab(d$room_type)
    } else {
      cat_tab(d$neighbourhood)
    }
  })

  output$tbl_cat <- renderDT({
    tb <- cat_data()
    if (input$uni_cat == "neighbourhood") tb <- head(tb, 100)
    datatable(tb, rownames = FALSE, options = list(pageLength = 8, dom = "tip"))
  })

  output$uni_bar <- renderPlotly({
    tb <- cat_data()
    if (input$uni_cat == "neighbourhood") tb <- head(tb, input$uni_topn)
    tb$Kategori <- factor(tb$Kategori, levels = rev(tb$Kategori))
    p <- ggplot(tb, aes(x = Kategori, y = Frekuensi, fill = Kategori,
                        text = paste0(Kategori, "<br>Frekuensi: ", Frekuensi, "<br>", Persen, "%"))) +
      geom_col(show.legend = FALSE) + coord_flip() +
      fill_scale(levels(tb$Kategori)) +
      labs(title = paste("Frekuensi", input$uni_cat), x = input$uni_cat, y = "Frekuensi") +
      theme_dash()
    sp(ggplotly(p, tooltip = "text"))
  })

  output$uni_cat_text <- renderUI({
    tb <- cat_data()
    rare <- sum(tb$Persen < 1)
    extra <- if (input$uni_cat == "neighbourhood")
      sprintf("Terdapat %d kategori yang proporsinya kurang dari 1%% sehingga distribusi sangat tersebar; 10 kategori teratas mencakup %s%% listing.",
              rare, fm(tb$Kumulatif[min(10, nrow(tb))], 1))
    else
      sprintf("Proporsi kategori terkecil (%s) hanya %s%%, sehingga kategori ini relatif jarang.",
              tb$Kategori[nrow(tb)], fm(tb$Persen[nrow(tb)], 2))
    div(class = "interp", HTML(sprintf(
      "Variabel <b>%s</b> memiliki <b>%d kategori</b>. Modus (kategori terbanyak) adalah <b>%s</b> dengan %s listing (%s%%). %s",
      input$uni_cat, nrow(tb), tb$Kategori[1], fm(tb$Frekuensi[1], 0), fm(tb$Persen[1], 2), extra)))
  })

  # ================== MENU 3 : BIVARIAT ==================
  # ---- A. numerik-numerik
  output$bva_plot <- renderPlotly({
    d <- samp(dat(), 5000)
    x <- d[[input$bva_x]]; y <- d[[input$bva_y]]
    if (isTRUE(input$bva_log)) { x <- log1p(x); y <- log1p(y) }
    df <- data.frame(x = x, y = y, room_type = d$room_type)
    df$txt <- paste0("<b>", d$neighbourhood, "</b><br>Tipe kamar: ", d$room_type,
                     "<br>", input$bva_x, ": ", fm(d[[input$bva_x]], 0),
                     "<br>", input$bva_y, ": ", fm(d[[input$bva_y]], 0))
    xl <- if (isTRUE(input$bva_log)) paste0("log(1 + ", input$bva_x, ")") else input$bva_x
    yl <- if (isTRUE(input$bva_log)) paste0("log(1 + ", input$bva_y, ")") else input$bva_y
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_point(aes(color = room_type, text = txt), alpha = 0.45, size = 1.4) +
      geom_smooth(method = input$bva_sm, formula = y ~ x, color = PAL$navy, se = TRUE) +
      scale_color_manual(values = col_room) +
      labs(title = paste("Scatter plot", input$bva_y, "vs", input$bva_x),
           x = xl, y = yl, color = "Tipe kamar") +
      theme_dash()
    sp(ggplotly(p, tooltip = "text"))
  })

  bva_cors <- reactive({
    d <- dat(); x <- d[[input$bva_x]]; y <- d[[input$bva_y]]
    pe <- suppressWarnings(cor.test(x, y, method = "pearson"))
    sp <- suppressWarnings(cor(x, y, method = "spearman"))
    list(r = unname(pe$estimate), p = pe$p.value, rho = sp)
  })

  output$bva_cor <- renderTable({
    cc <- bva_cors()
    data.frame(Ukuran = c("Pearson (r)", "Nilai-p Pearson", "Spearman (rho)"),
               Nilai = c(fm(cc$r, 4), fp(cc$p), fm(cc$rho, 4)))
  })

  output$bva_text <- renderUI({
    cc <- bva_cors()
    if (input$bva_x == input$bva_y) return(div(class = "interp", "Pilih dua variabel yang berbeda."))
    arah <- if (cc$r > 0) "positif (X naik, Y cenderung naik)" else "negatif (X naik, Y cenderung turun)"
    div(class = "interp", HTML(sprintf(
      "Hubungan <b>%s</b> dan <b>%s</b> berarah <b>%s</b> dengan kekuatan <b>%s</b> (Pearson r = %s; Spearman rho = %s).
       Hubungan %s secara statistik (nilai-p %s). Jika Spearman jauh berbeda dari Pearson, artinya hubungan
       bersifat monoton tetapi tidak linear atau dipengaruhi pencilan. Perhatikan pula pola titik: banyak titik
       menumpuk pada nilai 0 atau nilai kecil yang menandakan sebaran miring.",
      input$bva_x, input$bva_y, arah, cor_strength(cc$r), fm(cc$r, 3), fm(cc$rho, 3),
      if (cc$p < 0.05) "signifikan" else "tidak signifikan", fp(cc$p))))
  })

  heat_cm <- reactive(cor(dat()[, num_vars], method = input$bva_cm))

  output$bva_heat <- renderPlotly({
    cm <- round(heat_cm(), 3); v <- colnames(cm)
    ann <- expand.grid(i = seq_along(v), j = seq_along(v))
    annotations <- lapply(seq_len(nrow(ann)), function(k)
      list(x = v[ann$j[k]], y = v[ann$i[k]], text = sprintf("%.2f", cm[ann$i[k], ann$j[k]]),
           showarrow = FALSE, font = list(color = PAL$navy, size = 12)))
    plot_ly(x = v, y = v, z = cm, type = "heatmap", zmin = -1, zmax = 1,
            colorscale = list(list(0, PAL$blue), list(0.5, PAL$cream), list(1, PAL$deeprose)),
            hovertemplate = "%{y} vs %{x}<br>Korelasi: %{z}<extra></extra>") %>%
      layout(title = list(text = paste0("<b>Matriks korelasi ", input$bva_cm, "</b>")),
             yaxis = list(autorange = "reversed"), annotations = annotations) %>%
      sp()
  })

  output$bva_heat_text <- renderUI({
    cm <- heat_cm(); diag(cm) <- NA
    sub <- cm[num4, num4]
    idx <- which(abs(sub) == max(abs(sub), na.rm = TRUE), arr.ind = TRUE)[1, ]
    pr  <- cm["price", num4[-1]]
    top <- names(pr)[which.max(abs(pr))]
    div(class = "interp", HTML(sprintf(
      "Pasangan variabel (di antara 4 variabel non-koordinat) dengan korelasi terkuat adalah <b>%s</b> dan <b>%s</b>
       (r = %s). Variabel yang paling berkorelasi dengan <b>price</b> adalah <b>%s</b> (r = %s). Warna merah =
       korelasi positif, biru = negatif.",
      rownames(sub)[idx[1]], colnames(sub)[idx[2]], fm(sub[idx[1], idx[2]], 3),
      top, fm(pr[top], 3))))
  })

  # ---- B. numerik-kategorik
  bvb_df <- reactive({
    d <- dat(); v <- input$bvb_var
    if (input$bvb_grp == "room_type") {
      g <- as.character(d$room_type)
    } else {
      top <- names(sort(table(d$neighbourhood), decreasing = TRUE))[1:input$bvb_top]
      d <- d[d$neighbourhood %in% top, ]
      g <- as.character(d$neighbourhood)
    }
    data.frame(raw = d[[v]], g = g, stringsAsFactors = FALSE)
  })

  output$bvb_plot <- renderPlotly({
    df <- bvb_df()
    df$x <- if (isTRUE(input$bvb_log)) log1p(df$raw) else df$raw
    df$g <- reorder(df$g, df$x, FUN = median)
    lv <- levels(df$g)
    cols <- if (all(lv %in% names(col_room))) col_room[lv] else
      setNames(colorRampPalette(c(PAL$blue, PAL$green, PAL$sand, PAL$rose))(length(lv)), lv)
    yl <- if (isTRUE(input$bvb_log)) paste0("log(1 + ", input$bvb_var, ")") else input$bvb_var
    plot_ly(df, x = ~x, y = ~g, color = ~g, colors = cols, type = "box", orientation = "h",
            boxpoints = "outliers", marker = list(size = 3, opacity = 0.35)) %>%
      layout(title = list(text = paste0("<b>", input$bvb_var, " menurut ", input$bvb_grp, "</b>")),
             xaxis = list(title = yl), yaxis = list(title = input$bvb_grp), showlegend = FALSE) %>%
      sp()
  })

  output$bvb_tbl <- renderDT({
    tb <- bvb_df() %>% group_by(Kelompok = g) %>%
      summarise(N = n(), Mean = mean(raw), Median = median(raw), SD = sd(raw),
                Min = min(raw), Maks = max(raw), .groups = "drop") %>%
      arrange(desc(Median)) %>% mutate(across(where(is.numeric), ~ round(.x, 2)))
    datatable(tb, rownames = FALSE, options = list(dom = "tip", pageLength = 8, scrollX = TRUE))
  })

  output$bvb_text <- renderUI({
    df <- bvb_df()
    kw <- kruskal.test(raw ~ factor(g), data = df)
    med <- tapply(df$raw, df$g, median)
    div(class = "interp", HTML(sprintf(
      "Median <b>%s</b> tertinggi pada <b>%s</b> (%s) dan terendah pada <b>%s</b> (%s).
       Uji Kruskal-Wallis: H = %s, nilai-p %s, sehingga perbedaan antar kelompok <b>%s</b> pada taraf 5%%.
       Boxplot memperlihatkan perbedaan median, lebar kotak (variasi), dan banyaknya pencilan
       pada tiap kelompok. Uji non-parametrik dipilih karena sebaran tidak normal.",
      input$bvb_var, names(which.max(med)), fm(max(med), 1), names(which.min(med)), fm(min(med), 1),
      fm(unname(kw$statistic), 1), fp(kw$p.value),
      if (kw$p.value < 0.05) "signifikan" else "tidak signifikan")))
  })

  # ---- C. kategorik-kategorik
  bvc_d <- reactive({
    d <- dat()
    top <- names(sort(table(d$neighbourhood), decreasing = TRUE))[1:input$bvc_top]
    d <- d[d$neighbourhood %in% top, ]
    d$neighbourhood <- droplevels(d$neighbourhood)
    d
  })

  output$bvc_plot <- renderPlotly({
    d <- bvc_d()
    tb <- as.data.frame(table(neighbourhood = d$neighbourhood, room_type = d$room_type),
                        stringsAsFactors = FALSE)
    tb <- tb %>% group_by(neighbourhood) %>%
      mutate(tot = sum(Freq), pct = Freq / tot * 100) %>% ungroup()
    tb$txt <- paste0("<b>", tb$neighbourhood, "</b><br>Tipe kamar: ", tb$room_type,
                     "<br>Jumlah: ", fm(tb$Freq, 0), "<br>Proporsi: ", fm(tb$pct, 1),
                     "% dari ", fm(tb$tot, 0), " listing")
    tb$neighbourhood <- reorder(tb$neighbourhood, tb$tot)
    p <- ggplot(tb, aes(neighbourhood, Freq, fill = room_type, text = txt)) +
      geom_col(position = if (input$bvc_pos == "fill") "fill" else "stack") +
      coord_flip() + scale_fill_manual(values = col_room) +
      labs(title = "Komposisi room_type pada neighbourhood terpopuler",
           x = "neighbourhood", y = if (input$bvc_pos == "fill") "Proporsi" else "Jumlah listing",
           fill = "Tipe kamar") +
      theme_dash()
    sp(ggplotly(p, tooltip = "text"))
  })

  output$bvc_tbl <- renderDT({
    d <- bvc_d()
    tb <- as.data.frame.matrix(table(d$neighbourhood, d$room_type))
    tb <- cbind(neighbourhood = rownames(tb), tb)
    datatable(tb, rownames = FALSE, options = list(dom = "tip", pageLength = 6, scrollX = TRUE))
  })

  output$bvc_text <- renderUI({
    d <- bvc_d(); tb <- table(d$neighbourhood, d$room_type)
    ch <- suppressWarnings(chisq.test(tb))
    v  <- sqrt(unname(ch$statistic) / (sum(tb) * min(nrow(tb) - 1, ncol(tb) - 1)))
    pr <- prop.table(tb, 1)
    ent <- if ("Entire home/apt" %in% colnames(pr)) pr[, "Entire home/apt"] else pr[, 1]
    nm  <- if ("Entire home/apt" %in% colnames(pr)) "Entire home/apt" else colnames(pr)[1]
    kuat <- if (v < 0.1) "sangat lemah" else if (v < 0.3) "lemah" else if (v < 0.5) "sedang" else "kuat"
    div(class = "interp", HTML(sprintf(
      "Uji Chi-Square: X<sup>2</sup> = %s, db = %d, nilai-p %s, sehingga ada hubungan yang <b>%s</b> antara
       neighbourhood dan room_type. Cramer's V = <b>%s</b> (asosiasi <b>%s</b>). Proporsi <i>%s</i> tertinggi di
       <b>%s</b> (%s%%) dan terendah di <b>%s</b> (%s%%), menunjukkan perbedaan karakter pasar tiap kawasan.",
      fm(unname(ch$statistic), 1), as.integer(ch$parameter), fp(ch$p.value),
      if (ch$p.value < 0.05) "signifikan" else "tidak signifikan",
      fm(v, 3), kuat, nm, names(which.max(ent)), fm(max(ent) * 100, 1),
      names(which.min(ent)), fm(min(ent) * 100, 1))))
  })

  # ================== MENU 4 : MULTIVARIAT ==================
  output$mv_map <- renderLeaflet({
    d <- samp(dat(), input$map_n)
    popup <- paste0("<b>", d$neighbourhood, "</b><br>Tipe: ", d$room_type,
                    "<br>Harga: $", d$price, "<br>Min. malam: ", d$minimum_nights)
    if (input$map_col == "room_type") {
      pal <- colorFactor(unname(col_room[levels(d$room_type)]), levels(d$room_type))
      leaflet(d) %>% addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(~longitude, ~latitude, radius = 3, stroke = FALSE,
                         fillOpacity = 0.6, color = ~pal(room_type), popup = popup) %>%
        addLegend("bottomright", pal = pal, values = ~room_type, title = "Tipe kamar")
    } else {
      pal <- colorQuantile(c(PAL$mint, PAL$sand, "#E3B7BE", PAL$rose, PAL$deeprose), d$price, n = 5)
      leaflet(d) %>% addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(~longitude, ~latitude, radius = 3, stroke = FALSE,
                         fillOpacity = 0.7, color = ~pal(price), popup = popup) %>%
        addLegend("bottomright", pal = pal, values = ~price, title = "Harga (kuantil)")
    }
  })

  output$mv_map_text <- renderUI({
    d <- dat()
    med <- tapply(d$price, d$room_type, median)
    nb  <- d %>% group_by(neighbourhood) %>% summarise(n = n(), med = median(price), .groups = "drop") %>%
      filter(n >= 30) %>% arrange(desc(med))
    div(class = "interp", HTML(sprintf(
      "Peta memperlihatkan pola spasial listing berdasarkan koordinat. Titik terkonsentrasi pada kawasan
       tertentu (kepadatan tinggi) dan sebagian kawasan jarang listing. Ubah warna ke <i>Harga (kuantil)</i>
       untuk melihat apakah warna gelap (harga tinggi) mengelompok pada area tertentu, serta ke <i>Tipe kamar</i>
       untuk melihat kawasan yang didominasi tipe tertentu. Median harga menurut tipe kamar: %s.
       Neighbourhood dengan median harga tertinggi (min. 30 listing): <b>%s</b> ($%s). Klik titik untuk
       melihat detail listing.",
      paste(sprintf("%s = $%s", names(med), fm(med, 0)), collapse = "; "),
      nb$neighbourhood[1], fm(nb$med[1], 0))))
  })

  output$mv_bubble <- renderPlotly({
    d <- samp(dat(), 3000)
    x <- d[[input$mvb_x]]; y <- d[[input$mvb_y]]
    if (isTRUE(input$mvb_log)) { x <- log1p(x); y <- log1p(y) }
    sz <- d[[input$mvb_s]]
    tip <- paste0("<b>", d$neighbourhood, "</b><br>Tipe kamar: ", d$room_type,
                  "<br>", input$mvb_x, ": ", fm(d[[input$mvb_x]], 0),
                  "<br>", input$mvb_y, ": ", fm(d[[input$mvb_y]], 0),
                  "<br>", input$mvb_s, " (ukuran): ", fm(sz, 0))
    plot_ly(x = x, y = y, color = d$room_type, size = sz, sizes = c(4, 30), colors = col_room,
            type = "scatter", mode = "markers",
            marker = list(opacity = 0.5, sizemode = "diameter", line = list(width = 0.5, color = "#FFFFFF")),
            text = tip, hoverinfo = "text") %>%
      layout(title = list(text = paste0("<b>", input$mvb_y, " vs ", input$mvb_x,
                                        "</b> (ukuran = ", input$mvb_s, ")")),
             xaxis = list(title = if (isTRUE(input$mvb_log)) paste0("log(1+", input$mvb_x, ")") else input$mvb_x),
             yaxis = list(title = if (isTRUE(input$mvb_log)) paste0("log(1+", input$mvb_y, ")") else input$mvb_y),
             legend = list(title = list(text = "Tipe kamar"))) %>%
      sp()
  })

  output$mv_bubble_text <- renderUI({
    d <- dat()
    zero <- mean(d$availability_365 == 0) * 100
    div(class = "interp", HTML(sprintf(
      "Grafik menggabungkan 4 variabel: dua sumbu, ukuran gelembung, dan warna tipe kamar. Amati apakah
       satu warna (tipe kamar) berada pada posisi harga yang lebih tinggi/rendah, apakah gelembung besar
       (listing dengan banyak ulasan) berkumpul pada rentang harga atau ketersediaan tertentu, dan apakah
       terdapat kelompok titik yang terpisah. Catatan: %s%% listing memiliki availability_365 = 0
       (tidak tersedia), sehingga terbentuk barisan titik pada sisi kiri grafik bila X = availability_365.",
      fm(zero, 1))))
  })

  output$mv_heat <- renderPlotly({
    d <- dat()
    top <- names(sort(table(d$neighbourhood), decreasing = TRUE))[1:input$mvh_top]
    tb <- d %>% filter(neighbourhood %in% top) %>%
      group_by(neighbourhood, room_type) %>%
      summarise(Median = median(price), N = n(), .groups = "drop")
    p <- ggplot(tb, aes(x = room_type, y = neighbourhood, fill = Median,
                        text = paste0(neighbourhood, " - ", room_type, "<br>Median: $", Median, "<br>N: ", N))) +
      geom_tile(color = "white") +
      geom_text(aes(label = Median), size = 3) +
      scale_fill_gradient(low = PAL$cream, high = PAL$deeprose, name = "Median ($)") +
      labs(title = "Median harga: neighbourhood x room_type", x = "Tipe kamar", y = "neighbourhood") +
      theme_dash()
    sp(ggplotly(p, tooltip = "text"))
  })

  output$mv_heat_text <- renderUI({
    d <- dat()
    top <- names(sort(table(d$neighbourhood), decreasing = TRUE))[1:input$mvh_top]
    tb <- d %>% filter(neighbourhood %in% top) %>%
      group_by(neighbourhood, room_type) %>%
      summarise(Median = median(price), N = n(), .groups = "drop") %>% filter(N >= 10)
    mx <- tb[which.max(tb$Median), ]; mn <- tb[which.min(tb$Median), ]
    div(class = "interp", HTML(sprintf(
      "Heatmap menunjukkan bahwa harga ditentukan oleh <b>kombinasi</b> lokasi dan tipe kamar (interaksi).
       Dalam gambar ini (kombinasi dengan minimal 10 listing), median tertinggi adalah <b>%s - %s</b> ($%s)
       dan terendah <b>%s - %s</b> ($%s). Pada setiap neighbourhood, warna cenderung makin pekat dari
       baris kamar bersama/privat ke <i>Entire home/apt</i>, dan antar neighbourhood terlihat perbedaan
       tingkat harga yang jelas. Sel kosong menandakan kombinasi tidak ada.",
      mx$neighbourhood, mx$room_type, fm(mx$Median, 0),
      mn$neighbourhood, mn$room_type, fm(mn$Median, 0))))
  })

  pca_obj <- reactive({
    d <- dat()
    lg <- isTRUE(input$pca_log)
    X <- data.frame(
      price = if (lg) log(d$price) else d$price,
      minimum_nights = if (lg) log1p(d$minimum_nights) else d$minimum_nights,
      number_of_reviews = if (lg) log1p(d$number_of_reviews) else d$number_of_reviews,
      availability_365 = d$availability_365,
      latitude = d$latitude, longitude = d$longitude)
    list(p = prcomp(X, scale. = TRUE), room = d$room_type)
  })

  output$pca_var <- renderTable({
    p <- pca_obj()$p
    ve <- p$sdev^2 / sum(p$sdev^2)
    data.frame(Komponen = paste0("PC", seq_along(ve)),
               Varians_persen = round(ve * 100, 2),
               Kumulatif_persen = round(cumsum(ve) * 100, 2))
  })

  output$pca_biplot <- renderPlotly({
    o <- pca_obj(); p <- o$p
    ve <- p$sdev^2 / sum(p$sdev^2)
    sc <- data.frame(PC1 = p$x[, 1], PC2 = p$x[, 2], room_type = o$room)
    sc <- samp(sc, 4000)
    L <- as.data.frame(p$rotation[, 1:2]); L$var <- rownames(L)
    k <- 0.85 * min(max(abs(sc$PC1)) / max(abs(L$PC1)), max(abs(sc$PC2)) / max(abs(L$PC2)))
    pl <- plot_ly() %>%
      add_markers(data = sc, x = ~PC1, y = ~PC2, color = ~room_type, colors = col_room,
                  marker = list(size = 5, opacity = 0.4),
                  text = ~paste0("Tipe kamar: ", room_type, "<br>PC1: ", round(PC1, 2),
                                 "<br>PC2: ", round(PC2, 2)),
                  hoverinfo = "text")
    for (i in seq_len(nrow(L))) {
      pl <- pl %>%
        add_segments(x = 0, y = 0, xend = L$PC1[i] * k, yend = L$PC2[i] * k,
                     line = list(color = PAL$navy, width = 2.5), showlegend = FALSE,
                     text = paste0("<b>", L$var[i], "</b><br>Loading PC1: ", round(L$PC1[i], 3),
                                   "<br>Loading PC2: ", round(L$PC2[i], 3)),
                     hoverinfo = "text") %>%
        add_annotations(x = L$PC1[i] * k * 1.1, y = L$PC2[i] * k * 1.1, text = L$var[i],
                        showarrow = FALSE, font = list(color = PAL$deeprose, size = 12))
    }
    pl %>% layout(title = list(text = "<b>Biplot PCA</b>"),
                  xaxis = list(title = sprintf("PC1 (%.1f%%)", ve[1] * 100)),
                  yaxis = list(title = sprintf("PC2 (%.1f%%)", ve[2] * 100)),
                  legend = list(title = list(text = "Tipe kamar"))) %>%
      sp()
  })

  output$pca_scree <- renderPlotly({
    p <- pca_obj()$p
    ve <- p$sdev^2 / sum(p$sdev^2) * 100
    pcs <- paste0("PC", seq_along(ve))
    df <- data.frame(PC = factor(pcs, levels = pcs), ve = ve)
    df$txt <- paste0("<b>", df$PC, "</b><br>Varians: ", fm(ve, 2), "%<br>Kumulatif: ",
                     fm(cumsum(ve), 2), "%")
    g <- ggplot(df, aes(PC, ve, text = txt, group = 1)) +
      geom_col(fill = PAL$blue) + geom_line(color = PAL$rose) + geom_point(color = PAL$rose, size = 2) +
      labs(title = "Scree plot", x = "Komponen utama", y = "Varians dijelaskan (%)") +
      theme_dash()
    sp(ggplotly(g, tooltip = "text"))
  })

  output$pca_text <- renderUI({
    p <- pca_obj()$p
    ve <- p$sdev^2 / sum(p$sdev^2) * 100
    r1 <- p$rotation[, 1]; r2 <- p$rotation[, 2]
    t1 <- names(sort(abs(r1), decreasing = TRUE))[1:2]
    t2 <- names(sort(abs(r2), decreasing = TRUE))[1:2]
    div(class = "interp", HTML(sprintf(
      "PC1 menjelaskan <b>%s%%</b> dan PC2 <b>%s%%</b> varians (kumulatif <b>%s%%</b>). PC1 paling dipengaruhi
       oleh <b>%s</b> dan <b>%s</b>; PC2 paling dipengaruhi oleh <b>%s</b> dan <b>%s</b>. Panah yang
       searah menandakan variabel berkorelasi positif, berlawanan arah berkorelasi negatif, dan saling tegak
       lurus hampir tidak berkorelasi. Sebaran titik per warna menunjukkan apakah tipe kamar membentuk
       kelompok pada ruang komponen utama. Jika varians kumulatif dua komponen pertama rendah, variabel-variabel
       ini relatif independen sehingga reduksi dimensi kurang efektif (lihat scree plot).",
      fm(ve[1], 1), fm(ve[2], 1), fm(sum(ve[1:2]), 1), t1[1], t1[2], t2[1], t2[2])))
  })

  # ================== MENU 5 : REGRESI ==================
  mdl_df <- reactive({
    d <- dat()
    if ("Entire home/apt" %in% levels(d$room_type))
      d$room_type <- relevel(d$room_type, ref = "Entire home/apt")
    if (isTRUE(input$reg_logx)) {
      d$minimum_nights <- log1p(d$minimum_nights)
      d$number_of_reviews <- log1p(d$number_of_reviews)
    }
    d$y <- if (isTRUE(input$reg_logy)) log(d$price) else d$price
    d
  })

  preds_eff <- reactive({
    p <- input$preds
    d <- dat()
    if ("room_type" %in% p && nlevels(d$room_type) < 2) p <- setdiff(p, "room_type")
    validate(need(length(p) >= 2, "Pilih minimal 2 prediktor (room_type butuh >= 2 kategori terpilih di filter)."))
    p
  })

  fit <- reactive({
    d <- mdl_df()
    f <- as.formula(paste("y ~", paste(preds_eff(), collapse = " + ")))
    lm(f, data = d)
  })

  ylab <- reactive(if (isTRUE(input$reg_logy)) "ln(price)" else "price")

  output$reg_eq <- renderUI({
    m <- fit(); co <- coef(m)
    terms <- paste0(ifelse(co[-1] >= 0, " + ", " - "),
                    formatC(abs(co[-1]), format = "g", digits = 4), " &times; ", names(co)[-1])
    div(class = "interp", HTML(paste0(
      "<b>&#375;(", ylab(), ") = ", formatC(co[1], format = "g", digits = 4),
      paste(terms, collapse = ""), "</b>",
      "<br><small>Model: ", ylab(), " sebagai respons; prediktor: ", paste(preds_eff(), collapse = ", "),
      ". Kategori acuan room_type = Entire home/apt.</small>")))
  })

  output$reg_fit <- renderTable({
    m <- fit(); s <- summary(m); f <- s$fstatistic
    data.frame(
      Ukuran = c("Jumlah observasi (n)", "R-squared", "Adjusted R-squared", "Residual Std. Error",
                 "RMSE (skala model)", "Statistik F", "Nilai-p uji F", "AIC"),
      Nilai = c(fm(nobs(m), 0), fm(s$r.squared, 4), fm(s$adj.r.squared, 4), fm(s$sigma, 4),
                fm(sqrt(mean(resid(m)^2)), 4),
                sprintf("%s (db1 = %d, db2 = %d)", fm(f[1], 2), as.integer(f[2]), as.integer(f[3])),
                fp(pf(f[1], f[2], f[3], lower.tail = FALSE)), fm(AIC(m), 1)))
  })

  output$reg_fit_text <- renderUI({
    m <- fit(); s <- summary(m); f <- s$fstatistic
    pF <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    kat <- if (s$adj.r.squared < 0.2) "rendah" else if (s$adj.r.squared < 0.5) "sedang" else "tinggi"
    div(class = "interp", HTML(sprintf(
      "<b>Uji simultan (F):</b> nilai-p %s, artinya model secara keseluruhan <b>%s</b> pada taraf 5%%
       (minimal satu prediktor berpengaruh terhadap %s).<br>
       <b>Kemampuan model:</b> R<sup>2</sup> = %s dan Adjusted R<sup>2</sup> = %s; sekitar <b>%s%%</b> keragaman
       %s dijelaskan oleh prediktor dalam model, tergolong <b>%s</b>. Sisanya (%s%%) dijelaskan faktor lain
       di luar model (mis. fasilitas, ukuran kamar, kualitas listing). Residual standard error = %s
       (%s).",
      fp(pf(f[1], f[2], f[3], lower.tail = FALSE)),
      if (pF < 0.05) "signifikan" else "tidak signifikan", ylab(),
      fm(s$r.squared, 4), fm(s$adj.r.squared, 4), fm(s$adj.r.squared * 100, 1), ylab(), kat,
      fm((1 - s$adj.r.squared) * 100, 1), fm(s$sigma, 4),
      if (isTRUE(input$reg_logy)) "dalam satuan log" else "dalam USD")))
  })

  output$reg_coef <- renderDT({
    m <- fit(); s <- summary(m)$coefficients; ci <- confint(m)
    tb <- data.frame(Variabel = rownames(s), Estimasi = s[, 1], Std_Error = s[, 2],
                     t_hitung = s[, 3], Nilai_p = s[, 4],
                     CI_2.5 = ci[, 1], CI_97.5 = ci[, 2], row.names = NULL)
    tb$Signif <- cut(tb$Nilai_p, c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                     labels = c("***", "**", "*", ".", "ns"))
    if (isTRUE(input$reg_logy))
      tb$Efek_persen <- ifelse(tb$Variabel == "(Intercept)", NA, round((exp(tb$Estimasi) - 1) * 100, 3))
    tb <- tb %>% mutate(across(c(Estimasi, Std_Error, t_hitung, CI_2.5, CI_97.5), ~ round(.x, 5)),
                        Nilai_p = signif(Nilai_p, 3))
    datatable(tb, rownames = FALSE, options = list(dom = "t", scrollX = TRUE, pageLength = 15))
  })

  output$reg_coef_text <- renderUI({
    m <- fit(); s <- summary(m)$coefficients
    ref <- levels(mdl_df()$room_type)[1]
    items <- lapply(2:nrow(s), function(i) {
      nm <- rownames(s)[i]; b <- s[i, 1]; p <- s[i, 4]
      sig <- if (p < 0.05) "signifikan" else "tidak signifikan"
      arah <- if (b > 0) "lebih tinggi" else "lebih rendah"
      naik <- if (b > 0) "naik" else "turun"
      efek <- if (isTRUE(input$reg_logy))
        paste0(fm(abs((exp(b) - 1) * 100), 2), "% (harga median)") else paste0("$", fm(abs(b), 3))
      if (startsWith(nm, "room_type")) {
        lvl <- sub("room_type", "", nm)
        tags$li(HTML(sprintf("<b>%s</b>: dibandingkan <i>%s</i> (acuan) dan variabel lain tetap, harga listing
          bertipe <i>%s</i> rata-rata <b>%s sekitar %s</b> (b = %s; nilai-p %s; <b>%s</b>).",
          nm, ref, lvl, arah, efek, fm(b, 4), fp(p), sig)))
      } else {
        satuan <- if (isTRUE(input$reg_logx) && nm %in% c("minimum_nights", "number_of_reviews"))
          "1 satuan (skala log1p)" else "1 satuan"
        tags$li(HTML(sprintf("<b>%s</b>: setiap kenaikan %s, dengan variabel lain tetap, harga rata-rata
          <b>%s sekitar %s</b> (b = %s; nilai-p %s; <b>%s</b>).",
          nm, satuan, naik, efek, fm(b, 5), fp(p), sig)))
      }
    })
    div(class = "interp",
        p(tags$b("Uji parsial (uji t)"), ": H0: koefisien = 0. Tolak H0 jika nilai-p < 0.05."),
        tags$ul(items),
        p(tags$small("Intercept adalah nilai respons ketika semua prediktor = 0 / kategori acuan; ",
                     "sering tidak bermakna praktis (mis. latitude/longitude = 0).")))
  })

  output$reg_rf <- renderPlotly({
    m <- fit()
    df <- samp(data.frame(fit = fitted(m), res = resid(m)), 5000)
    df$txt <- paste0("Fitted: ", fm(df$fit, 3), "<br>Residual: ", fm(df$res, 3))
    p <- ggplot(df, aes(fit, res)) + geom_point(aes(text = txt), alpha = 0.35, color = PAL$teal) +
      geom_hline(yintercept = 0, color = PAL$rose, linetype = "dashed") +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = PAL$navy) +
      labs(title = "Residual vs Fitted", x = paste("Nilai fitted", ylab()), y = "Residual") +
      theme_dash()
    sp(ggplotly(p, tooltip = "text"))
  })

  output$reg_qq <- renderPlotly({
    r <- sort(resid(fit())); n <- length(r)
    idx <- unique(round(seq(1, n, length.out = min(n, 3000))))
    th <- qnorm(ppoints(n))[idx]; sm <- r[idx]
    q <- unname(quantile(r, c(.25, .75))); tq <- qnorm(c(.25, .75))
    slope <- diff(q) / diff(tq); int <- q[1] - slope * tq[1]
    plot_ly() %>%
      add_markers(x = th, y = sm, marker = list(color = PAL$teal, opacity = 0.5, size = 5),
                  text = paste0("Kuantil teoretis: ", fm(th, 2), "<br>Residual: ", fm(sm, 3)),
                  hoverinfo = "text") %>%
      add_lines(x = range(th), y = int + slope * range(th),
                line = list(color = PAL$rose, dash = "dash"), hoverinfo = "none") %>%
      layout(title = list(text = "<b>Q-Q Plot Residual</b>"),
             xaxis = list(title = "Kuantil teoretis"), yaxis = list(title = "Kuantil residual"),
             showlegend = FALSE) %>%
      sp()
  })

  output$reg_vif <- renderTable({ vif_manual(fit()) })

  bp_res <- reactive({
    m <- fit(); X <- model.matrix(m)[, -1, drop = FALSE]
    aux <- lm(resid(m)^2 ~ X)
    stat <- nobs(m) * summary(aux)$r.squared
    list(stat = stat, df = ncol(X), p = pchisq(stat, ncol(X), lower.tail = FALSE))
  })

  output$reg_bp <- renderUI({
    b <- bp_res()
    HTML(sprintf("<b>Uji Breusch-Pagan</b> (homoskedastisitas): LM = %s, db = %d, nilai-p %s.",
                 fm(b$stat, 2), b$df, fp(b$p)))
  })

  output$reg_asm_text <- renderUI({
    v <- vif_manual(fit()); b <- bp_res()
    vmax <- if (is.null(v)) NA else max(v$VIF)
    div(class = "interp", HTML(sprintf(
      "<ul>
       <li><b>Linearitas &amp; homoskedastisitas</b> (Residual vs Fitted): sebaran residual yang acak di sekitar nol
           dengan lebar konstan mengindikasikan asumsi terpenuhi; pola melengkung atau melebar (corong) menandakan
           pelanggaran. Uji Breusch-Pagan nilai-p %s sehingga ragam sisaan <b>%s</b> homogen.</li>
       <li><b>Normalitas residual</b> (Q-Q Plot): titik yang mengikuti garis merah berarti residual mendekati normal;
           penyimpangan di ekor menandakan residual berekor tebal/miring. Dengan n besar, inferensi tetap
           relatif andal berkat teorema limit pusat.</li>
       <li><b>Multikolinearitas</b> (VIF): VIF maksimum = %s; %s (acuan umum VIF &lt; 5 atau &lt; 10).</li>
       </ul>",
      fp(b$p), if (b$p < 0.05) "<u>tidak</u>" else "",
      if (is.na(vmax)) "-" else fm(vmax, 2),
      if (is.na(vmax)) "tidak dihitung" else if (vmax < 5) "tidak ada indikasi multikolinearitas serius"
      else if (vmax < 10) "terdapat multikolinearitas moderat" else "terdapat multikolinearitas serius")))
  })

  output$reg_av <- renderPlotly({
    m <- fit(); d <- mdl_df()
    df <- data.frame(aktual = d$y, prediksi = fitted(m), nb = d$neighbourhood, room = d$room_type)
    df <- samp(df, 5000)
    usd <- function(z) if (isTRUE(input$reg_logy)) exp(z) else z
    df$txt <- paste0("<b>", df$nb, "</b><br>Tipe kamar: ", df$room,
                     "<br>Harga aktual: $", fm(usd(df$aktual), 0),
                     "<br>Harga prediksi: $", fm(usd(df$prediksi), 0))
    p <- ggplot(df, aes(prediksi, aktual)) +
      geom_point(aes(text = txt, color = room), alpha = 0.4) +
      geom_abline(slope = 1, intercept = 0, color = PAL$rose, linetype = "dashed") +
      scale_color_manual(values = col_room) +
      labs(title = "Nilai aktual vs prediksi", x = paste("Prediksi", ylab()),
           y = paste("Aktual", ylab()), color = "Tipe kamar") +
      theme_dash()
    sp(ggplotly(p, tooltip = "text"))
  })

  output$reg_eff_sel <- renderUI({
    selectInput("eff_var", "Pilih prediktor", choices = preds_eff())
  })

  output$reg_eff <- renderPlotly({
    req(input$eff_var, input$eff_var %in% preds_eff())
    m <- fit(); d <- mdl_df(); v <- input$eff_var
    base <- data.frame(minimum_nights = mean(d$minimum_nights),
                       number_of_reviews = mean(d$number_of_reviews),
                       availability_365 = mean(d$availability_365),
                       latitude = mean(d$latitude), longitude = mean(d$longitude),
                       room_type = factor(levels(d$room_type)[1], levels = levels(d$room_type)))
    bt <- function(z) if (isTRUE(input$reg_logy)) exp(z) else z
    yl <- if (isTRUE(input$reg_logy)) "Prediksi harga (USD, median)" else "Prediksi harga (USD)"
    if (v == "room_type") {
      nd <- base[rep(1, nlevels(d$room_type)), ]
      nd$room_type <- factor(levels(d$room_type), levels = levels(d$room_type))
      pr <- predict(m, nd, interval = "confidence")
      df <- data.frame(x = levels(d$room_type), fit = bt(pr[, 1]), lwr = bt(pr[, 2]), upr = bt(pr[, 3]))
      df$txt <- paste0("<b>", df$x, "</b><br>Prediksi: $", fm(df$fit, 2),
                       "<br>CI 95%: $", fm(df$lwr, 2), " sampai $", fm(df$upr, 2))
      p <- ggplot(df, aes(x, fit, fill = x, text = txt)) + geom_col(show.legend = FALSE) +
        geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = PAL$navy) +
        scale_fill_manual(values = col_room) +
        labs(title = "Efek room_type terhadap harga", x = "room_type", y = yl) + theme_dash()
    } else {
      g <- seq(quantile(d[[v]], 0.01), quantile(d[[v]], 0.99), length.out = 100)
      nd <- base[rep(1, 100), ]; nd[[v]] <- g
      pr <- predict(m, nd, interval = "confidence")
      xg <- if (isTRUE(input$reg_logx) && v %in% c("minimum_nights", "number_of_reviews")) expm1(g) else g
      df <- data.frame(x = xg, fit = bt(pr[, 1]), lwr = bt(pr[, 2]), upr = bt(pr[, 3]))
      df$txt <- paste0(v, ": ", fm(df$x, 2), "<br>Prediksi harga: $", fm(df$fit, 2),
                       "<br>CI 95%: $", fm(df$lwr, 2), " sampai $", fm(df$upr, 2))
      p <- ggplot(df, aes(x, fit)) +
        geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25, fill = PAL$teal) +
        geom_line(color = PAL$navy, linewidth = 1) +
        geom_point(aes(text = txt), color = PAL$rose, size = 1.2, alpha = 0.7) +
        labs(title = paste("Efek", v, "terhadap harga"), x = v, y = yl) + theme_dash()
    }
    sp(ggplotly(p, tooltip = "text"))
  })

  output$reg_vis_text <- renderUI({
    div(class = "interp", HTML(
      "<b>Aktual vs Prediksi:</b> titik yang rapat di sekitar garis merah (y = x) menunjukkan prediksi akurat; sebaran
       melebar menunjukkan masih banyak keragaman harga yang tidak dijelaskan model (konsisten dengan nilai R<sup>2</sup>).<br>
       <b>Efek marginal:</b> kurva menunjukkan perubahan harga prediksi saat satu prediktor berubah dan prediktor lain
       ditahan pada rata-rata (kategori acuan untuk room_type). Kemiringan kurva = arah dan besar efek, pita biru = selang
       kepercayaan 95%. Kurva yang makin curam menunjukkan pengaruh lebih besar."))
  })

  pred_res <- eventReactive(input$btn_pred, {
    m <- fit(); d <- mdl_df()
    nd <- data.frame(
      minimum_nights = if (isTRUE(input$reg_logx)) log1p(input$pr_min) else input$pr_min,
      number_of_reviews = if (isTRUE(input$reg_logx)) log1p(input$pr_rev) else input$pr_rev,
      availability_365 = input$pr_av, latitude = input$pr_lat, longitude = input$pr_lon,
      room_type = factor(input$pr_room, levels = levels(d$room_type)))
    validate(need(!is.na(nd$room_type), "room_type tidak ada pada data terfilter."))
    predict(m, nd, interval = "prediction")
  })

  output$pr_out <- renderUI({
    if (input$btn_pred == 0) return(div(class = "interp", "Isi nilai di sebelah kiri lalu klik Prediksi."))
    pr <- pred_res(); bt <- function(z) if (isTRUE(input$reg_logy)) exp(z) else z
    div(class = "interp", HTML(sprintf(
      "Prediksi harga per malam: <b>$%s</b><br>Selang prediksi 95%%: $%s sampai $%s.<br>
       <small>%s Selang prediksi lebar menandakan harga listing individual sangat bervariasi.</small>",
      fm(bt(pr[1, 1]), 2), fm(bt(pr[1, 2]), 2), fm(bt(pr[1, 3]), 2),
      if (isTRUE(input$reg_logy)) "Prediksi dikembalikan dari skala log (nilai median)." else "")))
  })

  # ================== MENU 6 : KESIMPULAN ==================
  fit_final <- reactive({
    d <- dat()
    d$room_type <- droplevels(d$room_type)
    if ("Entire home/apt" %in% levels(d$room_type))
      d$room_type <- relevel(d$room_type, ref = "Entire home/apt")
    d$ly <- log(d$price)
    if (nlevels(d$room_type) >= 2) {
      lm(ly ~ minimum_nights + number_of_reviews + availability_365 + room_type + latitude + longitude, data = d)
    } else {
      lm(ly ~ minimum_nights + number_of_reviews + availability_365 + latitude + longitude, data = d)
    }
  })

  output$kes_main <- renderUI({
    d <- dat(); sp <- desc_num(d$price)
    med <- tapply(d$price, d$room_type, median)
    nb <- d %>% group_by(neighbourhood) %>% summarise(n = n(), med = median(price), .groups = "drop") %>%
      filter(n >= 30) %>% arrange(desc(med))
    rho <- sapply(c("minimum_nights", "number_of_reviews", "availability_365"),
                  function(v) cor(d$price, d[[v]], method = "spearman"))
    m <- fit_final(); s <- summary(m); co <- s$coefficients[-1, , drop = FALSE]
    sigv <- rownames(co)[co[, 4] < 0.05]
    pos <- rownames(co)[co[, 4] < 0.05 & co[, 1] > 0]
    neg <- rownames(co)[co[, 4] < 0.05 & co[, 1] < 0]
    div(
      p(tags$i("Catatan: angka di bawah mengikuti filter data aktif di sidebar; model memakai ln(price) dengan 6 prediktor (spesifikasi baku).")),
      tags$ol(
        tags$li(HTML(sprintf("<b>Data.</b> Analisis memakai %s listing dan 8 variabel (numerik dan kategorik). Tidak ada data hilang
          pada variabel terpilih; harga $0 dibuang, dan pencilan ekstrem dikendalikan dengan filter serta transformasi log.",
          fm(nrow(d), 0)))),
        tags$li(HTML(sprintf("<b>Univariat.</b> Harga memiliki median $%s dan rata-rata $%s, sebaran <b>%s</b> (skewness %s) dengan
          %s%% pencilan; artinya sebagian kecil listing mahal menarik rata-rata ke atas, sehingga median lebih representatif.",
          fm(sp$Median, 0), fm(sp$Mean, 0), shape_txt(sp$Skewness), fm(sp$Skewness, 2), fm(sp$Pct_Pencilan, 1)))),
        tags$li(HTML(sprintf("<b>Bivariat.</b> Median harga menurut tipe kamar: %s. Median harga tertinggi antar neighbourhood
          (min. 30 listing) ada di <b>%s</b> ($%s). Korelasi Spearman harga dengan minimum_nights = %s, number_of_reviews = %s,
          availability_365 = %s.",
          paste(sprintf("%s $%s", names(med), fm(med, 0)), collapse = ", "),
          nb$neighbourhood[1], fm(nb$med[1], 0),
          fm(rho[1], 3), fm(rho[2], 3), fm(rho[3], 3)))),
        tags$li(HTML("<b>Multivariat.</b> Peta, bubble chart, heatmap, dan PCA menunjukkan bahwa harga dipengaruhi
          kombinasi lokasi dan tipe kamar, serta terdapat kelompok listing dengan karakter berbeda (mis. listing tersedia
          sepanjang tahun, listing tanpa ulasan).")),
        tags$li(HTML(sprintf("<b>Regresi berganda.</b> Model ln(price) memiliki R<sup>2</sup> = %s (adj. R<sup>2</sup> = %s) dan
          signifikan secara simultan (uji F, nilai-p %s). Prediktor signifikan (&alpha; = 5%%): %s. Prediktor berkoefisien positif: %s;
          negatif: %s. Daya jelas model tergolong %s sehingga masih banyak faktor lain yang menentukan harga.",
          fm(s$r.squared, 3), fm(s$adj.r.squared, 3),
          fp(pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)),
          if (length(sigv)) paste(sigv, collapse = ", ") else "tidak ada",
          if (length(pos)) paste(pos, collapse = ", ") else "-",
          if (length(neg)) paste(neg, collapse = ", ") else "-",
          if (s$adj.r.squared < 0.2) "rendah" else if (s$adj.r.squared < 0.5) "sedang" else "tinggi")))
      )
    )
  })

  output$kes_rec <- renderUI({
    tags$ol(
      tags$li(HTML("<b>Bagi host:</b> tentukan harga dengan membandingkan listing sejenis (tipe kamar dan kawasan yang sama)
        menggunakan median, bukan rata-rata, karena sebaran harga miring. Hasil heatmap dan model regresi dapat dijadikan acuan awal.")),
      tags$li(HTML("<b>Bagi calon tamu:</b> pilih tipe kamar dan kawasan sesuai anggaran; gunakan peta dan boxplot untuk menemukan
        kawasan dengan harga relatif rendah dan banyak pilihan.")),
      tags$li(HTML("<b>Bagi pengelola platform / pembuat kebijakan:</b> perhatikan listing berharga atau minimum menginap ekstrem
        serta listing yang tidak pernah tersedia (availability_365 = 0), karena dapat mengganggu kualitas data dan pemantauan pasar.")),
      tags$li(HTML("<b>Bagi analisis lanjutan:</b> tambahkan variabel fasilitas, ukuran kamar, skor ulasan, dan variabel waktu;
        pertimbangkan model non-linear atau spasial (mis. GAM, random forest, regresi spasial) serta memasukkan neighbourhood/borough
        sebagai efek acak untuk meningkatkan daya jelas.")),
      tags$li(HTML("<b>Kehati-hatian:</b> hasil bersifat asosiasi pada data tahun 2019 dan sebaiknya tidak ditafsirkan sebagai sebab-akibat;
        lihat menu Regresi &gt; Keterbatasan Model."))
    )
  })
}

shinyApp(ui, server)
