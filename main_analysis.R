## - Input: Web of Science plaintext export files (savedrecs*.txt)
## - Output: tables (CSV) + figures (PNG/TIFF/PDF/EPS)

## 0) Basic settings

Sys.setenv(LANGUAGE = "en")
options(stringsAsFactors = FALSE)


## 1) Packages

needed_pkgs <- c(
  "bibliometrix", "ggplot2", "dplyr", "tidyr", "stringr", "readr",
  "openxlsx", "igraph", "Matrix", "scales", "ragg", "patchwork",
  "forcats", "grid", "tibble", "rlang"
)

install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, dependencies = TRUE)
    }
  }
}
install_if_missing(needed_pkgs)
suppressPackageStartupMessages(invisible(lapply(needed_pkgs, library, character.only = TRUE)))
message("bibliometrix version: ", as.character(packageVersion("bibliometrix")))


## 2) User configuration


wos_dir <- file.path("data", "wos_export")
stopifnot(dir.exists(wos_dir))

years_use <- 2015:2025
timespan_str <- paste0(min(years_use), "–", max(years_use))


wos_pattern <- "^savedrecs( \\(\\d+\\))?\\.txt$"


USE_RUN_STAMP <- FALSE


set.seed(20260201)

## 3) Output folders 

run_tag <- paste0("_ANALYSIS_RUN_", min(years_use), "_", max(years_use))
if (USE_RUN_STAMP) {
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  run_tag <- paste0(run_tag, "_", stamp)
}


run_dir <- file.path("outputs", run_tag)

out_dir  <- file.path(run_dir, "tables")
plot_dir <- file.path(run_dir, "figures")
log_dir  <- file.path(run_dir, "logs")

dir.create(out_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(log_dir,  showWarnings = FALSE, recursive = TRUE)

message("WoS export directory: ", normalizePath(wos_dir, winslash = "/", mustWork = TRUE))
message("Run directory: ", normalizePath(run_dir, winslash = "/", mustWork = TRUE))

writeLines(capture.output(sessionInfo()), con = file.path(log_dir, "sessionInfo.txt"))



## 4) Global plot style

COL_PRIMARY <- "#398CAF"
COL_ALPHA   <- "#E76F51"
COL_DOSIM   <- "#2A9D8F"
COL_TOTAL   <- "grey35"
COL_SCP     <- "#C8D6E5"
COL_GRID    <- "grey90"
COL_OTHER   <- "grey60"
COL_OUTCOME <- "#6F7C91"

FONT_FAMILY <- ifelse(.Platform$OS.type == "windows", "Arial", "sans")

theme_pub <- function(base_size = 12, legend_pos = "top") {
  ggplot2::theme_minimal(base_size = base_size, base_family = FONT_FAMILY) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = base_size + 4),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = base_size),
      axis.title = ggplot2::element_text(face = "plain", size = base_size + 1),
      axis.text  = ggplot2::element_text(color = "black", size = base_size),
      panel.grid.major = ggplot2::element_line(color = COL_GRID, linewidth = 0.6),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = legend_pos,
      legend.title = ggplot2::element_text(face = "plain", size = base_size),
      legend.text  = ggplot2::element_text(size = base_size),
      plot.margin = ggplot2::margin(t = 10, r = 14, b = 10, l = 12)
    )
}

save_plot_all <- function(plot_obj, filename_base, width_in = 7.2, height_in = 7.9, dpi_png = 600) {
  filename_base <- gsub("\\s+", "", filename_base)
  
  ragg::agg_png(
    filename = file.path(plot_dir, paste0(filename_base, ".png")),
    width = width_in, height = height_in, units = "in", res = dpi_png, background = "white"
  )
  print(plot_obj); grDevices::dev.off()
  
  ragg::agg_tiff(
    filename = file.path(plot_dir, paste0(filename_base, ".tiff")),
    width = width_in, height = height_in, units = "in", res = dpi_png,
    compression = "lzw", background = "white"
  )
  print(plot_obj); grDevices::dev.off()
  
  ggplot2::ggsave(
    filename = file.path(plot_dir, paste0(filename_base, ".pdf")),
    plot = plot_obj, width = width_in, height = height_in, units = "in",
    device = grDevices::cairo_pdf, bg = "white"
  )
  
  tryCatch({
    ggplot2::ggsave(
      filename = file.path(plot_dir, paste0(filename_base, ".eps")),
      plot = plot_obj, width = width_in, height = height_in, units = "in",
      device = grDevices::cairo_ps, fallback_resolution = 600, bg = "white"
    )
  }, error = function(e) {
    message("[WARN] EPS export failed (ignored): ", e$message)
  })
}


## 5) Helper functions

normalize_text <- function(x){
  x %>%
    stringr::str_replace_all("[\u2010\u2011\u2012\u2013\u2014\u2212]", "-") %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()
}

detect_terms_vec <- function(x_norm, terms_regex_vec){
  pat <- stringr::regex(paste(terms_regex_vec, collapse = "|"), ignore_case = TRUE)
  stringr::str_detect(x_norm, pat)
}

split_authors <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  trimws(unlist(strsplit(x, ";", fixed = TRUE)))
}

get_first_author <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- stringr::str_squish(x)
  vapply(strsplit(x, ";", fixed = TRUE), function(v) {
    if (length(v) == 0) return("")
    stringr::str_trim(v[1])
  }, FUN.VALUE = character(1))
}

extract_doi <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- stringr::str_trim(x)
  x <- stringr::str_replace_all(x, "^https?://(dx\\.)?doi\\.org/", "")
  x <- stringr::str_replace_all(x, "\\s+", "")
  x <- tolower(x)
  m <- stringr::str_match(x, "(10\\.[0-9]{4,9}/[^\\s\"<>]+)")
  out <- ifelse(!is.na(m[, 2]), m[, 2], x)
  stringr::str_replace_all(out, "[\\.,;\\)\\]]+$", "")
}

norm_text_simple <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- stringr::str_replace_all(x, "[\u2010\u2011\u2012\u2013\u2014\u2212]", "-")
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  stringr::str_trim(x)
}

norm_title_key <- function(x) {
  x <- ifelse(is.na(x), "", x)
  tolower(gsub("[^A-Za-z0-9]+", "", x))
}

get_cagr <- function(df, years_vec) {
  df2 <- df %>% dplyr::filter(PY %in% years_vec)
  start_year <- min(years_vec)
  end_year   <- max(years_vec)
  start_n <- df2$Documents[df2$PY == start_year]
  end_n   <- df2$Documents[df2$PY == end_year]
  start_n <- ifelse(length(start_n) == 0, 0, start_n)
  end_n   <- ifelse(length(end_n) == 0, 0, end_n)
  n_period <- length(years_vec) - 1
  if (start_n <= 0 || end_n <= 0 || n_period <= 0) return(NA_real_)
  ((end_n / start_n)^(1 / n_period) - 1) * 100
}


## 6) Import WoS data 

files <- list.files(path = wos_dir, pattern = wos_pattern, full.names = TRUE)

if (length(files) == 0) {
  message("[WARN] No savedrecs*.txt matched; reading all .txt files in directory.")
  files <- list.files(path = wos_dir, pattern = "\\.txt$", full.names = TRUE)
}
if (length(files) == 0) stop("No WoS .txt export files found. Please check wos_dir.")

M <- bibliometrix::convert2df(file = files, dbsource = "wos", format = "plaintext")
if ("PY" %in% names(M)) M$PY <- suppressWarnings(as.integer(M$PY))

message("Raw records: ", nrow(M))
message("Raw year range: ", min(M$PY, na.rm = TRUE), " - ", max(M$PY, na.rm = TRUE))

## 7) Filter by time window

idx_outside <- which(!is.na(M$PY) & !(M$PY %in% years_use))
message("Records outside ", timespan_str, ": ", length(idx_outside))

if (length(idx_outside) > 0) {
  keep_cols <- intersect(c("AU","TI","SO","PY","DT","DI"), names(M))
  M_outside <- M[idx_outside, keep_cols, drop = FALSE]
  write.csv(M_outside, file.path(out_dir, "Records_outside_time_window.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
}

M <- M %>% dplyr::filter(!is.na(PY) & PY %in% years_use)
message("Filtered records: ", nrow(M))


## 8) De-duplication

M_original <- M
M_original$orig_id <- seq_len(nrow(M_original))
M <- M_original

M$DI <- ifelse(is.na(M$DI), "", M$DI)
doi_clean <- extract_doi(M$DI)

title_key <- norm_title_key(M$TI)
fa_key    <- tolower(get_first_author(M$AU))
comp_key  <- paste(title_key, fa_key, M$PY, sep = "_")

dup_doi <- duplicated(doi_clean) & doi_clean != ""
if (any(dup_doi)) {
  dup_all <- which(doi_clean %in% doi_clean[dup_doi] & doi_clean != "")
  write.csv(M[dup_all, ], file.path(out_dir, "Suspected_duplicates_by_DOI.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
}

no_doi <- doi_clean == ""
dup_key <- duplicated(comp_key) & no_doi
if (any(dup_key)) {
  dup_all <- which(comp_key %in% comp_key[dup_key] & no_doi)
  write.csv(M[dup_all, ], file.path(out_dir, "Suspected_duplicates_by_title_author_year.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
}

remove_doi_duplicates <- TRUE
remove_key_duplicates <- TRUE

M_dedup <- M

if (remove_doi_duplicates) {
  keep <- !duplicated(doi_clean) | doi_clean == ""
  M_dedup <- M_dedup[keep, ]
}

if (remove_key_duplicates) {
  doi2 <- extract_doi(ifelse(is.na(M_dedup$DI), "", M_dedup$DI))
  no_doi2 <- doi2 == ""
  key2 <- paste(
    norm_title_key(M_dedup$TI),
    tolower(get_first_author(M_dedup$AU)),
    M_dedup$PY,
    sep = "_"
  )
  keep2 <- !duplicated(key2) | !no_doi2
  M_dedup <- M_dedup[keep2, ]
}

message("Before dedup: ", nrow(M_original))
message("After  dedup: ", nrow(M_dedup))
message("Removed: ", nrow(M_original) - nrow(M_dedup))

removed_ids <- setdiff(M_original$orig_id, M_dedup$orig_id)
if (length(removed_ids) > 0) {
  removed_df <- M_original[M_original$orig_id %in% removed_ids, ]
  write.csv(removed_df, file.path(out_dir, "Removed_records_after_dedup.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
}
M <- M_dedup


## 9) Metadata extraction 

M <- bibliometrix::metaTagExtraction(M, Field = "AU_CO", sep = ";")
M <- bibliometrix::metaTagExtraction(M, Field = "AU_UN", sep = ";")

if ("AU_CO" %in% names(M)) {
  M$AU_CO <- gsub("Taiwan|Hong Kong|Macau|People's Republic of China|Peoples R China|Mainland China",
                  "China", M$AU_CO, ignore.case = TRUE)
  M$AU_CO <- gsub("England|Scotland|Wales|Northern Ireland",
                  "United Kingdom", M$AU_CO, ignore.case = TRUE)
  M$AU_CO <- gsub("U\\.S\\.A\\.|\\bUSA\\b|United States of America",
                  "United States", M$AU_CO, ignore.case = TRUE)
}


## 10) Unified keyword field

M$DE <- ifelse(is.na(M$DE), "", M$DE)
M$ID <- ifelse(is.na(M$ID), "", M$ID)

combine_keywords <- function(de, id) {
  x <- c(unlist(strsplit(de, ";", fixed = TRUE)), unlist(strsplit(id, ";", fixed = TRUE)))
  x <- trimws(x)
  x <- x[x != ""]
  x <- unique(x)
  paste(x, collapse = ";")
}
M$KW_ALL <- mapply(combine_keywords, M$DE, M$ID)


## 11) Alpha/Beta mention flags (title/abstract/keywords)

beta_terms <- c(
  "\\b177\\s*lu\\b|\\b177lu\\b|lu\\s*-?\\s*177|lutetium\\s*-?\\s*177",
  "\\b90\\s*y\\b|\\b90y\\b|y\\s*-?\\s*90|yttrium\\s*-?\\s*90",
  "\\b161\\s*tb\\b|\\b161tb\\b|tb\\s*-?\\s*161|terbium\\s*-?\\s*161",
  "\\b131\\s*i\\b|\\b131i\\b|i\\s*-?\\s*131|iodine\\s*-?\\s*131",
  "\\b67\\s*cu\\b|\\b67cu\\b|cu\\s*-?\\s*67|copper\\s*-?\\s*67",
  "\\b188\\s*re\\b|\\b188re\\b|re\\s*-?\\s*188|rhenium\\s*-?\\s*188",
  "beta\\s*-?\\s*emitt?ing|beta\\s*emitter(s)?|beta\\s*-?\\s*therapy"
)

alpha_terms <- c(
  "\\b225\\s*ac\\b|\\b225ac\\b|ac\\s*-?\\s*225|actinium\\s*-?\\s*225",
  "\\b213\\s*bi\\b|\\b213bi\\b|bi\\s*-?\\s*213|bismuth\\s*-?\\s*213",
  "\\b211\\s*at\\b|\\b211at\\b|at\\s*-?\\s*211|astatine\\s*-?\\s*211",
  "\\b227\\s*th\\b|\\b227th\\b|th\\s*-?\\s*227|thorium\\s*-?\\s*227",
  "\\b212\\s*pb\\b|\\b212pb\\b|pb\\s*-?\\s*212|lead\\s*-?\\s*212",
  "targeted\\s*alpha\\s*therapy|alpha\\s*-?\\s*therapy|alpha\\s*-?\\s*emitt?ing|alpha\\s*emitter(s)?"
)

if (!"AB" %in% names(M)) M$AB <- ""
M$AB     <- ifelse(is.na(M$AB), "", M$AB)
M$TI     <- ifelse(is.na(M$TI), "", M$TI)
M$KW_ALL <- ifelse(is.na(M$KW_ALL), "", M$KW_ALL)
M$PY     <- suppressWarnings(as.integer(M$PY))
M$TC     <- suppressWarnings(as.numeric(M$TC)); M$TC[is.na(M$TC)] <- 0

text_all_norm <- normalize_text(paste(M$TI, M$AB, M$KW_ALL, sep = " "))

M$Flag_Beta  <- detect_terms_vec(text_all_norm, beta_terms)
M$Flag_Alpha <- detect_terms_vec(text_all_norm, alpha_terms)

LBL_BETA  <- "Beta-emitting RLT"
LBL_ALPHA <- "Alpha therapy"
LBL_OTHER <- "Other/unspecified"

M$Therapy_Class <- dplyr::case_when(
  M$Flag_Alpha ~ LBL_ALPHA,  # alpha-priority scheme
  M$Flag_Beta  ~ LBL_BETA,
  TRUE         ~ LBL_OTHER
)

M$Therapy_Class <- factor(as.character(M$Therapy_Class),
                          levels = c(LBL_BETA, LBL_ALPHA, LBL_OTHER))


## 12) Table S1 

tableS1_core <- M %>%
  dplyr::group_by(Therapy_Class) %>%
  dplyr::summarise(
    `Documents, n` = dplyr::n(),
    `Total citations` = sum(TC, na.rm = TRUE),
    `Mean citations` = round(mean(TC, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Therapy_Class) %>%
  dplyr::mutate(`Therapy Class` = as.character(Therapy_Class)) %>%
  dplyr::select(`Therapy Class`, `Documents, n`, `Total citations`, `Mean citations`)

tableS1_total <- tableS1_core %>%
  dplyr::summarise(
    `Therapy Class` = "Total",
    `Documents, n` = sum(`Documents, n`),
    `Total citations` = sum(`Total citations`),
    `Mean citations` = round(`Total citations` / `Documents, n`, 2)
  )

Table_S1 <- dplyr::bind_rows(tableS1_core, tableS1_total)

write.csv(Table_S1, file.path(out_dir, "TableS1.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
message("Saved: outputs/Table_S1.csv")


## 13) Core bibliometrix summary

results <- bibliometrix::biblioAnalysis(M, sep = ";")
S <- tryCatch(
  summary(results, k = 20, pause = FALSE),
  error = function(e1) {
    message("[WARN] summary(results, k=20) failed: ", e1$message)
    tryCatch(summary(results), error = function(e2) NULL)
  }
)
if (!is.null(S)) {
  capture.output(S, file = file.path(out_dir, "Bibliometrix_Summary.txt"))
  message("Saved: outputs/Bibliometrix_Summary.txt")
}


## 14) Table 1

total_docs <- nrow(M)

annual_counts_tbl1 <- M %>%
  dplyr::filter(!is.na(PY), PY %in% years_use) %>%
  dplyr::count(PY, name = "Documents") %>%
  dplyr::arrange(PY)

annual_growth_rate <- get_cagr(annual_counts_tbl1, years_use)
annual_growth_rate_str <- ifelse(is.na(annual_growth_rate), "NA", sprintf("%.2f", annual_growth_rate))

doc_types_str <- if ("DT" %in% names(M)) {
  dt <- ifelse(is.na(M$DT) | M$DT == "", "Unknown", M$DT)
  dt <- stringr::str_replace_all(dt, "\\s*;\\s*", "; ")
  dt_tab <- sort(table(dt), decreasing = TRUE)
  paste0(names(dt_tab), " (", as.integer(dt_tab), ")", collapse = "; ")
} else "NA"

languages_str <- if ("LA" %in% names(M)) {
  la <- ifelse(is.na(M$LA) | M$LA == "", "Unknown", M$LA)
  la_tab <- sort(table(la), decreasing = TRUE)
  paste0(names(la_tab), " (", as.integer(la_tab), ")", collapse = "; ")
} else "NA"

total_citations <- sum(M$TC, na.rm = TRUE)
avg_citations_per_doc <- ifelse(total_docs > 0, total_citations / total_docs, NA_real_)

all_authors <- unique(unlist(lapply(M$AU, split_authors)))
all_authors <- all_authors[all_authors != ""]
total_authors <- length(all_authors)

authors_per_paper <- vapply(M$AU, function(x) length(split_authors(x)), FUN.VALUE = integer(1))
multi_authored_idx <- which(authors_per_paper >= 2)
collab_index <- if (length(multi_authored_idx) == 0) NA_real_ else sum(authors_per_paper[multi_authored_idx]) / length(multi_authored_idx)
collab_index_str <- ifelse(is.na(collab_index), "NA", sprintf("%.2f", collab_index))

table1_dataset_overview <- data.frame(
  Metric = c(
    "Timespan","Total documents","Annual growth rate (%)","Document types","Languages",
    "Total citations","Average citations per document","Total authors","Collaboration index"
  ),
  Value = c(
    timespan_str, as.character(total_docs), annual_growth_rate_str, doc_types_str, languages_str,
    as.character(total_citations), sprintf("%.2f", avg_citations_per_doc),
    as.character(total_authors), collab_index_str
  ),
  stringsAsFactors = FALSE
)

write.csv(table1_dataset_overview, file.path(out_dir, "Table_1.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
message("Saved: outputs/Table1.csv")


## 15) Table S6 (Top journals)

table3_journals <- M %>%
  dplyr::mutate(SO = dplyr::if_else(is.na(SO) | SO == "", "Unknown", SO)) %>%
  dplyr::group_by(SO) %>%
  dplyr::summarise(
    Publications = dplyr::n(),
    Total_Citations = sum(TC),
    Mean_Citations = round(mean(TC), 2),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(Publications), dplyr::desc(Total_Citations)) %>%
  dplyr::slice(1:10) %>%
  dplyr::rename(Journal = SO)

write.csv(table3_journals, file.path(out_dir, "Table S6_Journals_with_Citations.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
message("Saved: outputs/Table_S6.csv")


## 16) Table S7 (Top countries + SCP/MCP)

if (!"AU_CO" %in% colnames(M)) stop("Field AU_CO not found. metaTagExtraction may have failed.")

table4_countries <- M %>%
  dplyr::mutate(paper_id = dplyr::row_number(), AU_CO = dplyr::if_else(is.na(AU_CO), "", AU_CO)) %>%
  tidyr::separate_rows(AU_CO, sep = ";") %>%
  dplyr::mutate(Country = stringr::str_trim(AU_CO)) %>%
  dplyr::filter(Country != "") %>%
  dplyr::distinct(paper_id, Country, .keep_all = TRUE) %>%
  dplyr::group_by(paper_id) %>%
  dplyr::mutate(n_countries = dplyr::n_distinct(Country)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    SCP_flag = dplyr::if_else(n_countries == 1, 1L, 0L),
    MCP_flag = dplyr::if_else(n_countries > 1, 1L, 0L)
  ) %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(
    Publications = dplyr::n(),
    Total_Citations = sum(TC),
    Mean_Citations = round(mean(TC), 2),
    SCP = sum(SCP_flag),
    MCP = sum(MCP_flag),
    MCP_pct = paste0(round(100 * MCP / Publications), "%"),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(Publications), dplyr::desc(Total_Citations)) %>%
  dplyr::slice(1:10)

write.csv(table4_countries, file.path(out_dir, "Table_S7.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
message("Saved: outputs/Table S7_Top10 countries.csv")


## 17) Table S8 (Most cited documents)

table7_most_cited <- M %>%
  dplyr::mutate(
    Title = dplyr::if_else(is.na(TI), "", TI),
    Journal = dplyr::if_else(is.na(SO), "", SO),
    Year = PY
  ) %>%
  dplyr::arrange(dplyr::desc(TC)) %>%
  dplyr::transmute(
    Authors = AU,
    Title,
    Year,
    Journal,
    Total_Citations = TC,
    DOI = extract_doi(DI)
  ) %>%
  dplyr::slice(1:10)

write.csv(table7_most_cited, file.path(out_dir, "Table_S8.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
message("Saved: outputs/Table_S8.csv")


## 18) Figure 2

ADD_TIMELINE <- TRUE
SHOW_VLINES  <- FALSE   # TRUE: overlay dashed vlines in a/b core; FALSE: show only in timeline strip


COL_MILESTONE <- "grey55"   # light grey for strip
COL_CONN      <- "grey70"   # connector lines lighter
COL_BASELINE  <- "grey80"

milestones <- tibble::tibble(
  year  = c(2019, 2021, 2022, 2023),
  label = c(
    "EANM\nguidance",
    "VISION;\nTheraP",
    "FDA/EMA\nPluvicto",
    "EANM/SNMMI\nGuidelines"
  )
) %>%
  dplyr::mutate(
   
    row   = c(1, 1, 2, 1),
    y0    = 0.78,
   
    y_lab = dplyr::if_else(row == 1, 0.44, 0.22)
  )

x_expand <- ggplot2::expansion(mult = c(0.02, 0.22))

year_pub <- data.frame(Year = years_use) %>%
  dplyr::left_join(
    M %>%
      dplyr::filter(!is.na(PY), PY %in% years_use) %>%
      dplyr::count(PY, name = "Publications") %>%
      dplyr::rename(Year = PY),
    by = "Year"
  ) %>%
  dplyr::mutate(Publications = ifelse(is.na(Publications), 0, Publications)) %>%
  dplyr::arrange(Year)

y_max_a <- max(year_pub$Publications, na.rm = TRUE)
label_offset <- y_max_a * 0.045
y_upper_a <- y_max_a + label_offset + max(14, round(y_max_a * 0.10))

p_fig2a <- ggplot2::ggplot(year_pub, ggplot2::aes(x = Year, y = Publications)) +
  { if (ADD_TIMELINE && SHOW_VLINES)
    ggplot2::geom_vline(
      data = milestones,
      ggplot2::aes(xintercept = year),
      inherit.aes = FALSE,
      linetype = "dashed", linewidth = 0.30,
      color = COL_MILESTONE, alpha = 0.45
    ) else NULL
  } +
  ggplot2::geom_col(fill = COL_PRIMARY, width = 0.75, alpha = 0.95) +
  ggplot2::geom_text(
    ggplot2::aes(y = Publications + label_offset, label = Publications),
    size = 3.6, colour = "black", vjust = 0
  ) +
  ggplot2::scale_x_continuous(breaks = years_use, expand = x_expand) +
  ggplot2::scale_y_continuous(
    limits = c(0, y_upper_a),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(x = NULL, y = "Number of publications", tag = "a") +
  theme_pub(base_size = 12, legend_pos = "none") +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    plot.tag = ggplot2::element_text(face = "bold", size = 14),
    plot.tag.position = c(0.01, 0.99),
    plot.margin = ggplot2::margin(t = 10, r = 14, b = 6, l = 12)
  )

trend_class <- M %>%
  dplyr::filter(!is.na(PY), PY %in% years_use) %>%
  dplyr::count(PY, Therapy_Class, name = "Publications") %>%
  dplyr::filter(Therapy_Class %in% c(LBL_BETA, LBL_ALPHA, LBL_OTHER)) %>%
  tidyr::complete(
    PY = years_use,
    Therapy_Class = c(LBL_BETA, LBL_ALPHA, LBL_OTHER),
    fill = list(Publications = 0)
  ) %>%
  dplyr::arrange(PY, Therapy_Class)

trend_total <- trend_class %>%
  dplyr::group_by(PY) %>%
  dplyr::summarise(Publications = sum(Publications), .groups = "drop") %>%
  dplyr::mutate(Therapy_Class = "Total (alpha + beta + other)")

plot2b_all <- dplyr::bind_rows(trend_class, trend_total) %>%
  dplyr::mutate(
    Therapy_Class = factor(
      Therapy_Class,
      levels = c(LBL_BETA, LBL_ALPHA, LBL_OTHER, "Total (alpha + beta + other)")
    )
  )

col_vals <- c(COL_PRIMARY, COL_ALPHA, COL_OTHER, COL_TOTAL)
names(col_vals) <- levels(plot2b_all$Therapy_Class)

lt_vals <- c("solid", "solid", "solid", "dashed")
names(lt_vals) <- levels(plot2b_all$Therapy_Class)

y_max_b <- max(plot2b_all$Publications, na.rm = TRUE)

p_fig2b_core <- ggplot2::ggplot(
  plot2b_all,
  ggplot2::aes(x = PY, y = Publications,
               colour = Therapy_Class, linetype = Therapy_Class, group = Therapy_Class)
) +
  { if (ADD_TIMELINE && SHOW_VLINES)
    ggplot2::geom_vline(
      data = milestones,
      ggplot2::aes(xintercept = year),
      inherit.aes = FALSE,
      linetype = "dashed", linewidth = 0.30,
      color = COL_MILESTONE, alpha = 0.45
    ) else NULL
  } +
  ggplot2::geom_line(linewidth = 1.10) +
  ggplot2::geom_point(
    data = dplyr::filter(plot2b_all, Therapy_Class != "Total (alpha + beta + other)"),
    size = 2.1, show.legend = FALSE
  ) +
  ggplot2::scale_x_continuous(breaks = years_use, labels = as.character(years_use), expand = x_expand) +
  ggplot2::scale_y_continuous(
    limits = c(0, y_max_b * 1.06),
    expand = ggplot2::expansion(mult = c(0.02, 0.02))
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::scale_colour_manual(values = col_vals, drop = FALSE) +
  ggplot2::scale_linetype_manual(values = lt_vals, drop = FALSE) +
  ggplot2::labs(
    x = NULL, y = "Number of publications",
    colour = NULL, linetype = NULL, tag = "b"
  ) +
  theme_pub(base_size = 12, legend_pos = "top") +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    plot.tag = ggplot2::element_text(face = "bold", size = 14),
    plot.tag.position = c(0.01, 0.99),
    
    legend.text = ggplot2::element_text(size = 10.5),
    legend.key.width = grid::unit(1.15, "cm"),
    legend.spacing.x = grid::unit(0.18, "cm"),
  
    axis.text.x  = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    
    plot.margin = ggplot2::margin(t = 10, r = 14, b = 0, l = 12)
  ) +
  ggplot2::guides(
    colour   = ggplot2::guide_legend(nrow = 2, byrow = TRUE, override.aes = list(linewidth = 1.2)),
    linetype = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
  )

p_timeline <- ggplot2::ggplot(milestones) +

  ggplot2::geom_segment(
    ggplot2::aes(x = min(years_use), xend = max(years_use), y = y0, yend = y0),
    inherit.aes = FALSE,
    linewidth = 0.40, color = COL_BASELINE
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(x = year, xend = year, y = y0, yend = y0 - 0.055),
    inherit.aes = FALSE,
    linewidth = 0.40, color = COL_BASELINE
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(x = year, xend = year, y = y0 - 0.055, yend = y_lab + 0.045),
    inherit.aes = FALSE,
    linewidth = 0.28, color = COL_CONN
  ) +
  ggplot2::geom_label(
    ggplot2::aes(x = year, y = y_lab, label = label),
    inherit.aes = FALSE,
    label.size = 0,  # no outline
    label.padding = grid::unit(0.10, "lines"),
    label.r = grid::unit(0.12, "lines"),
    fill = "white",
    color = "grey25",
    size = 2.85,
    lineheight = 0.95,
    hjust = 0.5, vjust = 0.5
  ) +
  ggplot2::scale_x_continuous(
    breaks = years_use,
    labels = as.character(years_use),
    expand = x_expand
  ) +
  ggplot2::scale_y_continuous(limits = c(0.06, 0.90), breaks = NULL) +
  ggplot2::labs(x = "Year", y = NULL) +
  ggplot2::coord_cartesian(clip = "off") +
  theme_pub(base_size = 12, legend_pos = "none") +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.y  = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    plot.margin  = ggplot2::margin(t = 1, r = 30, b = 8, l = 12)
  )
p_fig2b <- p_fig2b_core / p_timeline +
  patchwork::plot_layout(heights = c(1, 0.44))

p_fig2 <- p_fig2a / p_fig2b +
  patchwork::plot_layout(heights = c(1, 1.46))

save_plot_all(p_fig2, "Figure_2", width_in = 7.2, height_in = 7.9, dpi_png = 600)

message("Saved: plots/Figure_2")





## 19) Figure 3 

text_all_norm2 <- normalize_text(paste(M$TI, M$AB, M$KW_ALL, sep = " "))

core_pattern <- stringr::regex(
  paste0("\\b(",
         "dosimet(r(y|ic)|ry)",
         "|absorbed\\s*(dose|dosimetr(y|ic))",
         "|radiation\\s*dosimet(r(y|ic)|ry)",
         "|mird",
         "|olinda",
         "|time(\\s*-\\s*|\\s+)activity",
         "|residence\\s*time",
         "|s\\s*value(s)?",
         "|voxel(\\s*-\\s*|\\s+)based\\s*dosimet(r(y|ic)|ry)",
         ")\\b"),
  ignore_case = TRUE
)
personal_pattern <- stringr::regex("\\b(patient\\s*-?\\s*specific|personalized|individuali[sz]ed|personalization)\\b",
                                   ignore_case = TRUE)
anchor_pattern <- stringr::regex(
  paste0("\\b(",
         "dose(s)?",
         "|dosimet(r(y|ic)|ry)",
         "|absorbed",
         "|mird",
         "|olinda",
         "|time(\\s*-\\s*|\\s+)activity",
         "|residence\\s*time",
         "|s\\s*value(s)?",
         "|radiation\\s*dose",
         "|radiation\\s*dosimet(r(y|ic)|ry)",
         ")\\b"),
  ignore_case = TRUE
)

core_hit   <- stringr::str_detect(text_all_norm2, core_pattern)
pers_hit   <- stringr::str_detect(text_all_norm2, personal_pattern)
anchor_hit <- stringr::str_detect(text_all_norm2, anchor_pattern)

M$Flag_Dosimetry <- core_hit | (pers_hit & anchor_hit)

trend_dos <- M %>%
  dplyr::filter(!is.na(PY), PY %in% years_use) %>%
  dplyr::group_by(PY) %>%
  dplyr::summarise(
    Total         = dplyr::n(),
    DosimetryDocs = sum(Flag_Dosimetry, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::complete(PY = years_use, fill = list(Total = 0, DosimetryDocs = 0)) %>%
  dplyr::mutate(Share = dplyr::if_else(Total > 0, DosimetryDocs / Total, 0)) %>%
  dplyr::arrange(PY)

pA_dos <- ggplot2::ggplot(trend_dos, ggplot2::aes(x = PY, y = Share)) +
  ggplot2::geom_line(linewidth = 1.10, color = COL_DOSIM) +
  ggplot2::geom_point(size = 2.3, color = COL_DOSIM) +
  ggplot2::geom_smooth(method = "loess", formula = y ~ x, span = 0.75, se = FALSE,
                       linewidth = 0.95, linetype = "dashed", color = COL_TOTAL) +
  ggplot2::scale_x_continuous(breaks = years_use, labels = as.character(years_use),
                              expand = ggplot2::expansion(mult = c(0.01, 0.02))) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                              limits = c(0, NA),
                              expand = ggplot2::expansion(mult = c(0.02, 0.06))) +
  ggplot2::labs(x = NULL, y = "Dosimetry-tagged publications (%)") +
  theme_pub(base_size = 12, legend_pos = "none") +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

pB_dos <- ggplot2::ggplot(trend_dos, ggplot2::aes(x = PY)) +
  ggplot2::geom_line(ggplot2::aes(y = Total), linewidth = 1.05, linetype = "dashed", color = COL_TOTAL, alpha = 0.85) +
  ggplot2::geom_line(ggplot2::aes(y = DosimetryDocs), linewidth = 1.10, color = COL_DOSIM) +
  ggplot2::geom_point(ggplot2::aes(y = DosimetryDocs), size = 2.2, color = COL_DOSIM) +
  ggplot2::scale_x_continuous(breaks = years_use, labels = as.character(years_use),
                              expand = ggplot2::expansion(mult = c(0.01, 0.02))) +
  ggplot2::labs(x = "Year", y = "Number of publications") +
  theme_pub(base_size = 12, legend_pos = "none") +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

p_fig3 <- pA_dos / pB_dos +
  patchwork::plot_layout(heights = c(1, 1)) +
  patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", size = 14),
                 plot.tag.position = c(0.01, 0.99))

save_plot_all(p_fig3, "Figure_3", width_in = 10.5, height_in = 8.2, dpi_png = 600)
message("Saved: plots/Figure_3")


## 20) Figure 4 

top_sources <- sort(results$Sources, decreasing = TRUE)
top_n <- 10
top_sources_df <- data.frame(
  Journal = names(top_sources)[1:min(top_n, length(top_sources))],
  Publications = as.integer(top_sources[1:min(top_n, length(top_sources))]),
  stringsAsFactors = FALSE
)

to_title_if_allcaps <- function(x) {
  is_allcaps <- !stringr::str_detect(x, "[a-z]") & stringr::str_detect(x, "[A-Z]")
  ifelse(is_allcaps, stringr::str_to_title(stringr::str_to_lower(x)), x)
}

top_sources_df <- top_sources_df %>%
  dplyr::mutate(
    Journal_disp = to_title_if_allcaps(Journal),
    Journal_wrap = stringr::str_wrap(Journal_disp, width = 26)
  ) %>%
  dplyr::arrange(Publications)

top_sources_df$Journal_wrap <- factor(top_sources_df$Journal_wrap, levels = top_sources_df$Journal_wrap)

x_max <- max(top_sources_df$Publications, na.rm = TRUE)
nudge_val <- max(1, round(x_max * 0.015))
x_limit_upper <- x_max + max(12, round(x_max * 0.12))

p_fig4a <- ggplot2::ggplot(top_sources_df, ggplot2::aes(x = Publications, y = Journal_wrap)) +
  ggplot2::geom_col(fill = COL_PRIMARY, width = 0.75, alpha = 0.95) +
  ggplot2::geom_text(ggplot2::aes(label = Publications), hjust = 0, nudge_x = nudge_val,
                     size = 3.8, color = "black") +
  ggplot2::scale_x_continuous(limits = c(0, x_limit_upper), expand = ggplot2::expansion(mult = c(0, 0.02))) +
  ggplot2::labs(x = "Number of publications", y = "Source") +
  theme_pub(base_size = 12, legend_pos = "none") +
  ggplot2::theme(
    plot.margin = ggplot2::margin(t = 6, r = 22, b = 10, l = 18),
    axis.text.y  = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(r = 10)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 18))
  )

top_countries <- table4_countries %>%
  dplyr::transmute(
    Country = as.character(Country),
    SCP, MCP,
    Documents = Publications,
    MCP_pct = suppressWarnings(as.numeric(stringr::str_remove(MCP_pct, "%")))
  )

plot_long <- top_countries %>%
  dplyr::select(Country, SCP, MCP) %>%
  tidyr::pivot_longer(cols = c(SCP, MCP), names_to = "Type", values_to = "Count")

top_countries <- top_countries %>%
  dplyr::arrange(Documents) %>%
  dplyr::mutate(Country = factor(Country, levels = Country))
plot_long <- plot_long %>%
  dplyr::mutate(Country = factor(Country, levels = levels(top_countries$Country)))

x_max_c <- max(top_countries$Documents, na.rm = TRUE)
top_countries <- top_countries %>%
  dplyr::mutate(
    MCP_pct    = ifelse(is.na(MCP_pct), round(100 * MCP / Documents), MCP_pct),
    PctLabel   = paste0(MCP_pct, "%"),
    PctX       = SCP + MCP / 2,
    PctColor   = ifelse(MCP <= 1, "grey20", "white"),
    TotalX     = Documents + max(1, round(x_max_c * 0.04)),
    TotalLabel = Documents
  )

gap_total_c <- max(40, round(x_max_c * 0.20))
x_limit_upper_c <- x_max_c + gap_total_c

p_fig4b <- ggplot2::ggplot(plot_long, ggplot2::aes(x = Count, y = Country, fill = Type)) +
  ggplot2::geom_col(width = 0.75, color = "white", linewidth = 0.8, alpha = 0.98) +
  ggplot2::geom_text(
    data = top_countries,
    ggplot2::aes(x = PctX, y = Country, label = PctLabel),
    inherit.aes = FALSE, hjust = 0.5, size = 3.8, color = top_countries$PctColor
  ) +
  ggplot2::geom_text(
    data = top_countries,
    ggplot2::aes(x = TotalX, y = Country, label = TotalLabel),
    inherit.aes = FALSE, hjust = 0, size = 3.8, color = "grey20"
  ) +
  ggplot2::scale_x_continuous(limits = c(0, x_limit_upper_c), expand = ggplot2::expansion(mult = c(0, 0.01))) +
  ggplot2::scale_fill_manual(
    values = c("SCP" = COL_SCP, "MCP" = COL_PRIMARY),
    breaks = c("SCP", "MCP"),
    labels = c("Single-country publications (SCP)", "Multiple-country publications (MCP)")
  ) +
  ggplot2::labs(x = "Number of publications", y = "Country", fill = NULL) +
  theme_pub(base_size = 12, legend_pos = "top") +
  ggplot2::theme(
    plot.margin = ggplot2::margin(t = 6, r = 24, b = 10, l = 18),
    axis.text.y  = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(r = 10)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 18))
  )

p_fig4 <- p_fig4a / p_fig4b +
  patchwork::plot_layout(heights = c(1, 1.15)) +
  patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", size = 14),
                 plot.tag.position = c(0.01, 0.99))

save_plot_all(p_fig4, "Figure_4", width_in = 13.2, height_in = 13.0, dpi_png = 600)
message("Saved: plots/Figure_4")


## 21) Table S5 

LBL_AONLY     <- "Alpha-only"
LBL_BONLY     <- "Beta-only"
LBL_BOTH      <- "Both (alpha+beta)"
LBL_OTHER_CAN <- LBL_OTHER

M <- M %>%
  dplyr::mutate(
    Class_Dual = dplyr::case_when(
      Flag_Alpha & Flag_Beta  ~ LBL_BOTH,
      Flag_Alpha & !Flag_Beta ~ LBL_AONLY,
      !Flag_Alpha & Flag_Beta ~ LBL_BONLY,
      TRUE                    ~ LBL_OTHER_CAN
    )
  )

M_sens <- M %>%
  dplyr::mutate(PY = suppressWarnings(as.integer(PY))) %>%
  dplyr::filter(!is.na(PY), PY %in% years_use)

both_by_year_tbl <- M_sens %>%
  dplyr::mutate(
    Overlap_Class = dplyr::case_when(
      Flag_Alpha & Flag_Beta  ~ LBL_BOTH,
      Flag_Alpha & !Flag_Beta ~ LBL_AONLY,
      !Flag_Alpha & Flag_Beta ~ LBL_BONLY,
      TRUE                    ~ LBL_OTHER_CAN
    )
  ) %>%
  dplyr::count(PY, Overlap_Class, name = "Records") %>%
  tidyr::complete(
    PY = years_use,
    Overlap_Class = c(LBL_AONLY, LBL_BONLY, LBL_BOTH, LBL_OTHER_CAN),
    fill = list(Records = 0)
  ) %>%
  tidyr::pivot_wider(names_from = Overlap_Class, values_from = Records) %>%
  dplyr::mutate(
    Total_All      = .data[[LBL_AONLY]] + .data[[LBL_BONLY]] + .data[[LBL_BOTH]] + .data[[LBL_OTHER_CAN]],
    Both_Count     = .data[[LBL_BOTH]],
    Both_Share_All = dplyr::if_else(Total_All > 0, Both_Count / Total_All, 0)
  ) %>%
  dplyr::transmute(
    Year = PY,
    Total_publications_n = Total_All,
    Both_alpha_beta_comentioning_n = Both_Count,
    Share_of_annual_output_pct = round(100 * Both_Share_All, 1)
  ) %>%
  dplyr::arrange(Year)

write.csv(both_by_year_tbl, file.path(out_dir, "Table_S5.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
message("Saved: outputs/Table_S5.csv")


## 22) Table 2 

top_n_per_domain <- 20

need_cols <- c("TI","KW_ALL","TC","PY","AU","DI")
missing_cols <- setdiff(need_cols, names(M))
if (length(missing_cols) > 0) stop(paste("Missing required fields:", paste(missing_cols, collapse = ", ")))

has_SO <- "SO" %in% names(M)
has_DT <- "DT" %in% names(M)

M_use <- M %>%
  dplyr::mutate(
    TI     = ifelse(is.na(TI), "", TI),
    KW_ALL = ifelse(is.na(KW_ALL), "", KW_ALL),
    AU     = ifelse(is.na(AU), "", AU),
    PY     = suppressWarnings(as.integer(PY)),
    TC     = suppressWarnings(as.numeric(TC)),
    SO     = if (has_SO) ifelse(is.na(SO), "", SO) else "",
    DT     = if (has_DT) ifelse(is.na(DT), "", DT) else "",
    DOI    = extract_doi(DI)
  )
M_use$TC[is.na(M_use$TC)] <- 0

is_article <- if (has_DT) {
  dt_norm <- norm_text_simple(M_use$DT)
  stringr::str_detect(dt_norm, "\\barticle\\b")
} else {
  rep(TRUE, nrow(M_use))
}

TI_norm <- norm_text_simple(M_use$TI)
KW_norm <- norm_text_simple(M_use$KW_ALL)
SO_norm <- if (has_SO) norm_text_simple(M_use$SO) else rep("", nrow(M_use))
TEXT_TKSO <- stringr::str_squish(paste(TI_norm, KW_norm, SO_norm))

pat_guideline <- stringr::regex(
  "(guideline|guidelines|consensus|recommendation|recommendations|position\\s*paper|appropriate\\s*use\\s*criteria|procedure\\s*standard|standard\\s*operating\\s*procedure|\\bsop\\b)",
  ignore_case = TRUE
)
pat_review_excl <- stringr::regex("(systematic\\s+review|meta-analys|pooled\\s+analysis)", ignore_case = TRUE)

pat_preclinical <- stringr::regex(
  paste(
    "preclinical|in\\s*vivo|in\\s*vitro|xenograft|murine|mouse\\b|mice\\b|animal\\b",
    "biodistribution|pharmacokinet|radiolabel|radiochem|radiochemistry",
    "dota\\b|dota-?conjugated|chelator|label(l)?ing",
    "psma\\s*inhibitor|ligand\\s*(development|design)|lead\\s*optimization|structure-activity|\\bsar\\b",
    "preclinical\\s*evaluation",
    sep = "|"
  ),
  ignore_case = TRUE
)

pat_clinical_excl <- stringr::regex(
  "(patient|patients|metastatic|mcrpc|castration-?resistant|trial|phase\\s*(i|ii|iii|1|2|3)\\b|randomi[sz]ed|prospective)",
  ignore_case = TRUE
)

pat_trial <- stringr::regex(
  "trial\\b|clinical\\s*trial|phase\\s*(i|ii|1|2)\\b|prospective|open-?label|randomi[sz]ed|multicenter|multi-?centre|single-?centre",
  ignore_case = TRUE
)

pat_lu177   <- stringr::regex("(\\b177\\s*lu\\b|\\b177lu\\b|lu-?177|lutetium\\s*-?\\s*177|lutetium-?177)", ignore_case = TRUE)
pat_psma617 <- stringr::regex("(psma-?617|psma\\s*-?\\s*617|vipivotide\\s*t(?:e|a)traxetan|pluvicto)", ignore_case = TRUE)
pat_mcrpc   <- stringr::regex("(mcrpc|metastatic\\s+castration-?resistant|castration-?resistant)", ignore_case = TRUE)

is_nejm_source <- stringr::str_detect(SO_norm, "\\bnew england journal of medicine\\b|\\bnew\\s*engl\\s*j\\s*med\\b") |
  stringr::str_detect(M_use$DOI, "^10\\.1056/")
is_sartor_vision_main <- stringr::str_detect(M_use$DOI, "^10\\.1056/nejmoa2107322$")

flag_pivotal_beta <- is_article &
  stringr::str_detect(TEXT_TKSO, pat_lu177) &
  stringr::str_detect(TEXT_TKSO, pat_psma617) &
  stringr::str_detect(TEXT_TKSO, pat_mcrpc) &
  is_nejm_source &
  !stringr::str_detect(TEXT_TKSO, pat_guideline) &
  !stringr::str_detect(TEXT_TKSO, pat_review_excl)
flag_pivotal_beta <- flag_pivotal_beta | (is_article & is_sartor_vision_main)

flag_trial <- is_article &
  stringr::str_detect(TEXT_TKSO, pat_trial) &
  !stringr::str_detect(TEXT_TKSO, pat_guideline) &
  !stringr::str_detect(TEXT_TKSO, pat_review_excl) &
  !flag_pivotal_beta

pat_psma <- stringr::regex("(\\bpsma\\b|prostate-?specific\\s*membrane\\s*antigen|psma-?617)", ignore_case = TRUE)
pat_ac225 <- stringr::regex("(actinium-?225|\\bac-?225\\b|\\b225ac\\b)", ignore_case = TRUE)
pat_alpha_term <- stringr::regex("(alpha\\s*-?therapy|targeted\\s*alpha\\s*therapy)", ignore_case = TRUE)

flag_alpha_psma <- is_article &
  stringr::str_detect(TEXT_TKSO, pat_psma) &
  (stringr::str_detect(TEXT_TKSO, pat_ac225) | stringr::str_detect(TEXT_TKSO, pat_alpha_term)) &
  !stringr::str_detect(TEXT_TKSO, pat_guideline) &
  !stringr::str_detect(TEXT_TKSO, pat_review_excl)

flag_pre <- is_article &
  stringr::str_detect(TI_norm, pat_preclinical) &
  !stringr::str_detect(TI_norm, pat_clinical_excl) &
  !flag_trial & !flag_pivotal_beta & !flag_alpha_psma

domains_4 <- c(
  "Foundational preclinical ligand development",
  "Clinical translation / trial evidence",
  "Pivotal clinical evidence (beta-RLT)",
  "Emergence of alpha therapy"
)

Assigned_4 <- dplyr::case_when(
  flag_pre          ~ domains_4[1],
  flag_trial        ~ domains_4[2],
  flag_pivotal_beta ~ domains_4[3],
  flag_alpha_psma   ~ domains_4[4],
  TRUE ~ NA_character_
)

fa_vec <- get_first_author(M_use$AU)

pool_4 <- tibble::tibble(
  Milestone_domain = Assigned_4,
  Year = M_use$PY,
  Title = M_use$TI,
  Total_citations = M_use$TC,
  DOI = M_use$DOI,
  FirstAuthor = fa_vec,
  is_article = is_article
) %>%
  dplyr::filter(!is.na(Milestone_domain), is_article) %>%
  dplyr::mutate(Milestone_domain = factor(Milestone_domain, levels = domains_4))

candidates_topN <- pool_4 %>%
  dplyr::arrange(Milestone_domain, dplyr::desc(Total_citations), Year, Title) %>%
  dplyr::group_by(Milestone_domain) %>%
  dplyr::slice_head(n = top_n_per_domain) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    Milestone_domain = as.character(Milestone_domain),
    `First author (Year)` = ifelse(FirstAuthor == "" | is.na(FirstAuthor),
                                   paste0("NA (", Year, ")"),
                                   paste0(FirstAuthor, " (", Year, ")")),
    Title, Year,
    `Total citations` = Total_citations,
    DOI
  )

write.csv(
  candidates_topN,
  file.path(out_dir, paste0("Table2", top_n_per_domain, "_per_domain.csv")),
  row.names = FALSE, fileEncoding = "UTF-8"
)

Table2 <- pool_4 %>%
  dplyr::group_by(Milestone_domain) %>%
  dplyr::arrange(dplyr::desc(Total_citations), Year, Title, .by_group = TRUE) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    Milestone_domain = as.character(Milestone_domain),
    `First author (Year)` = ifelse(FirstAuthor == "" | is.na(FirstAuthor),
                                   paste0("NA (", Year, ")"),
                                   paste0(FirstAuthor, " (", Year, ")")),
    Title,
    `Total citations` = Total_citations,
    DOI
  )

missing_domains <- setdiff(domains_4, Table2$Milestone_domain)
if (length(missing_domains) > 0) {
  fill_rows <- tibble::tibble(
    Milestone_domain = missing_domains,
    `First author (Year)` = NA_character_,
    Title = NA_character_,
    `Total citations` = NA_real_,
    DOI = NA_character_
  )
  Table2 <- dplyr::bind_rows(Table2, fill_rows)
}

Table2 <- Table2 %>%
  dplyr::mutate(Milestone_domain = factor(Milestone_domain, levels = domains_4)) %>%
  dplyr::arrange(Milestone_domain) %>%
  dplyr::mutate(Milestone_domain = as.character(Milestone_domain))

write.csv(
  Table2,
  file.path(out_dir, "Table2.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

message("Saved: outputs/Table2.csv")



## 23) Figure 5 

GROUP_LEVELS <- c(
  "Clinical Translation (β-RLT)",
  "Dosimetry & Imaging",
  "Rise of α-Therapy",
  "Outcomes & Comparators"
)

KW_DICT <- tibble::tribble(
  ~Group,                         ~keyword_std,            ~Label,                 ~order_in_group,
  "Clinical Translation (β-RLT)",  "lu-177 (beta-rlt)",      "Lu-177 (β-RLT)",        1,
  "Clinical Translation (β-RLT)",  "mcrpc",                  "mCRPC",                2,
  "Clinical Translation (β-RLT)",  "radioligand therapy",    "Radioligand Therapy",  3,
  "Clinical Translation (β-RLT)",  "theranostics",           "Theranostics",         4,
  "Clinical Translation (β-RLT)",  "radionuclide therapy",   "Radionuclide Therapy", 5,
  
  "Dosimetry & Imaging",           "dosimetry",              "Dosimetry",            1,
  "Dosimetry & Imaging",           "pet/ct",                 "PET/CT",               2,
  "Dosimetry & Imaging",           "pet",                    "PET",                  3,
  
  "Rise of α-Therapy",             "alpha-therapy",          "α-Therapy",            1,
  
  "Outcomes & Comparators",        "efficacy",               "Efficacy",             1,
  "Outcomes & Comparators",        "safety",                 "Safety",               2,
  "Outcomes & Comparators",        "survival",               "Survival",             3,
  "Outcomes & Comparators",        "cabazitaxel",            "Cabazitaxel",          4,
  "Outcomes & Comparators",        "chemotherapy",           "Chemotherapy",         5
) %>%
  dplyr::mutate(Group = factor(Group, levels = GROUP_LEVELS))

LABEL_LEVELS_TOP  <- KW_DICT %>% dplyr::arrange(Group, order_in_group) %>% dplyr::pull(Label)
LABEL_LEVELS_PLOT <- rev(LABEL_LEVELS_TOP)

split_kw <- function(x) {
  x <- ifelse(is.na(x), "", x)
  stringr::str_split(x, "\\s*;\\s*|\\s*,\\s*|\\s*\\|\\s*") |>
    lapply(stringr::str_squish) |>
    lapply(function(v) v[v != ""])
}

clean_kw <- function(x) {
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "[\u2010-\u2015]", "-")
  x <- stringr::str_replace_all(x, "[^a-z0-9\\-\\s/\\(\\)]", " ")
  stringr::str_squish(x)
}

standardize_to_target <- function(k_raw) {
  k <- clean_kw(k_raw)
  
  k <- stringr::str_replace_all(k, "\\bpet\\s*/\\s*ct\\b", "pet/ct")
  k <- stringr::str_replace_all(k, "\\bprostate\\s*specific\\s*membrane\\s*antigen\\b", "psma")
  
  k <- stringr::str_replace_all(
    k,
    "\\b(ac\\s*-?\\s*225|actinium\\s*-?\\s*225|targeted\\s*alpha\\s*-?therapy|alpha\\s*-?therapy)\\b",
    "alpha-therapy"
  )
  
  lu177_hit <- stringr::str_detect(
    k,
    "\\b(177\\s*lu|lu\\s*177|lu-?177|lutetium\\s*-?177|177lutetium|lu-?177-?psma|lu-?177-?psma-?617)\\b"
  )
  
  dplyr::case_when(
    lu177_hit ~ "lu-177 (beta-rlt)",
    stringr::str_detect(k, "\\bmcrpc\\b") ~ "mcrpc",
    stringr::str_detect(k, "\\bradioligand therapy\\b") ~ "radioligand therapy",
    stringr::str_detect(k, "\\btheranostics\\b") ~ "theranostics",
    stringr::str_detect(k, "\\bradionuclide therapy\\b") ~ "radionuclide therapy",
    
    stringr::str_detect(k, "\\bdosimetr") ~ "dosimetry",
    stringr::str_detect(k, "\\babsorbed dose\\b") ~ "dosimetry",
    stringr::str_detect(k, "\\b(mird|s value|s-value)\\b") ~ "dosimetry",
    stringr::str_detect(k, "\\btime\\s*-?\\s*activity\\b") ~ "dosimetry",
    
    k == "pet/ct" ~ "pet/ct",
    k == "pet" ~ "pet",
    k == "alpha-therapy" ~ "alpha-therapy",
    
    k == "survival" ~ "survival",
    k == "safety" ~ "safety",
    k == "efficacy" ~ "efficacy",
    k == "cabazitaxel" ~ "cabazitaxel",
    k == "chemotherapy" ~ "chemotherapy",
    TRUE ~ NA_character_
  )
}

M <- M %>% dplyr::mutate(PY = suppressWarnings(as.integer(PY)), KW_ALL = as.character(KW_ALL))
years_use <- sort(unique(as.integer(years_use)))

kw_long <- tibble::tibble(
  DocID = seq_len(nrow(M)),
  Year  = M$PY,
  KW    = split_kw(M$KW_ALL)
) %>%
  dplyr::filter(!is.na(Year), Year %in% years_use) %>%
  tidyr::unnest(KW) %>%
  dplyr::mutate(keyword_std = standardize_to_target(KW)) %>%
  dplyr::filter(!is.na(keyword_std), keyword_std %in% KW_DICT$keyword_std) %>%
  dplyr::distinct(DocID, Year, keyword_std)

kw_counts <- kw_long %>%
  dplyr::count(Year, keyword_std, name = "Publications")

plot_df_heat <- tidyr::expand_grid(KW_DICT, Year = years_use) %>%
  dplyr::left_join(kw_counts, by = c("Year", "keyword_std")) %>%
  dplyr::mutate(
    Publications = tidyr::replace_na(Publications, 0L),
    Label = factor(Label, levels = LABEL_LEVELS_PLOT)
  )

COL_LOW  <- "white"
COL_HIGH <- "#388CAC"
MAX_LEGEND <- 125
LEG_BREAKS <- c(0, 25, 50, 75, 100, 125)

p_fig5 <- ggplot2::ggplot(plot_df_heat, ggplot2::aes(Year, Label, fill = Publications)) +
  ggplot2::geom_tile(color = "white", linewidth = 0.4) +
  ggplot2::facet_grid(Group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  ggplot2::scale_x_continuous(breaks = years_use, expand = ggplot2::expansion(mult = c(0.01, 0.02))) +
  ggplot2::scale_fill_gradient(
    low = COL_LOW, high = COL_HIGH,
    limits = c(0, MAX_LEGEND),
    oob = scales::squish,
    breaks = LEG_BREAKS,
    name = "Publications"
  ) +
  ggplot2::labs(
    title = "Temporal Evolution of Top Keywords in PSMA Radioligand Therapy",
    x = "Year", y = NULL
  ) +
  theme_pub(base_size = 12, legend_pos = "right") +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    strip.placement = "outside",
    strip.background = ggplot2::element_blank(),
    strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold", size = 12,
                                              margin = ggplot2::margin(r = 14), hjust = 0.5),
    axis.text.y = ggplot2::element_text(size = 10),
    axis.text.x = ggplot2::element_text(size = 11),
    plot.title  = ggplot2::element_text(face = "bold", size = 18, hjust = 0.5),
    axis.ticks = ggplot2::element_blank(),
    panel.spacing.y = grid::unit(1.1, "lines"),
    plot.margin = ggplot2::margin(t = 8, r = 10, b = 8, l = 18)
  )

save_plot_all(p_fig5, "Figure_5", width_in = 15, height_in = 7.6, dpi_png = 600)
message("Saved: plots/Figure_5")


## 24) Figure S1 

message("Running Figure S1 (sensitivity analysis)...")

lighten_hex <- function(hex, amount = 0.45) {
  rgb <- grDevices::col2rgb(hex)
  new_rgb <- rgb + (255 - rgb) * amount
  grDevices::rgb(new_rgb[1,]/255, new_rgb[2,]/255, new_rgb[3,]/255)
}
COL_ALPHA_LIGHT <- lighten_hex(COL_ALPHA,   amount = 0.45)
COL_BETA_LIGHT  <- lighten_hex(COL_PRIMARY, amount = 0.45)
COL_BOTH_COL    <- "#9B59B6"

LBL_ALPHA_CAN <- LBL_ALPHA
LBL_BETA_CAN  <- LBL_BETA
LBL_AONLY     <- "Alpha-only"
LBL_BONLY     <- "Beta-only"
LBL_BOTH      <- "Both (alpha+beta)"
LBL_OTHER_CAN <- LBL_OTHER

M <- M %>%
  dplyr::mutate(
    Class_Aprio = dplyr::case_when(
      Flag_Alpha ~ LBL_ALPHA_CAN,
      Flag_Beta  ~ LBL_BETA_CAN,
      TRUE       ~ LBL_OTHER_CAN
    ),
    Class_Bprio = dplyr::case_when(
      Flag_Beta  ~ LBL_BETA_CAN,
      Flag_Alpha ~ LBL_ALPHA_CAN,
      TRUE       ~ LBL_OTHER_CAN
    ),
    Class_Dual = dplyr::case_when(
      Flag_Alpha & Flag_Beta  ~ LBL_BOTH,
      Flag_Alpha & !Flag_Beta ~ LBL_AONLY,
      !Flag_Alpha & Flag_Beta ~ LBL_BONLY,
      TRUE                    ~ LBL_OTHER_CAN
    )
  )

annual_counts_by_class <- function(df, class_col, years_vec, keep_levels) {
  df %>%
    dplyr::filter(!is.na(PY), PY %in% years_vec) %>%
    dplyr::transmute(PY, Class = .data[[class_col]]) %>%
    dplyr::filter(Class %in% keep_levels) %>%
    dplyr::count(PY, Class, name = "Pubs") %>%
    tidyr::complete(
      PY = years_vec,
      Class = factor(keep_levels, levels = keep_levels),
      fill = list(Pubs = 0)
    ) %>%
    dplyr::mutate(Class = factor(as.character(Class), levels = keep_levels)) %>%
    dplyr::arrange(PY, Class)
}

years_vec <- years_use
keep_AB <- c(LBL_ALPHA_CAN, LBL_BETA_CAN)
keep_C  <- c(LBL_AONLY, LBL_BONLY, LBL_BOTH)

trend_A <- annual_counts_by_class(M, "Class_Aprio", years_vec, keep_AB) %>%
  dplyr::mutate(Scheme = "A: alpha-priority (main)")
trend_B <- annual_counts_by_class(M, "Class_Bprio", years_vec, keep_AB) %>%
  dplyr::mutate(Scheme = "B: beta-priority")
trend_C <- annual_counts_by_class(M, "Class_Dual",  years_vec, keep_C) %>%
  dplyr::mutate(Scheme = "C: dual-label")

trend_all <- dplyr::bind_rows(trend_A, trend_B, trend_C)

legend_levels <- c(LBL_ALPHA_CAN, LBL_BETA_CAN, LBL_AONLY, LBL_BONLY, LBL_BOTH)
trend_all$Class <- factor(as.character(trend_all$Class), levels = legend_levels)

color_map <- c(
  "Alpha therapy"     = COL_ALPHA,
  "Beta-emitting RLT" = COL_PRIMARY,
  "Alpha-only"        = COL_ALPHA_LIGHT,
  "Beta-only"         = COL_BETA_LIGHT,
  "Both (alpha+beta)" = COL_BOTH_COL
)

bad <- setdiff(unique(as.character(trend_all$Class)), names(color_map))
bad <- bad[!is.na(bad)]
if (length(bad) > 0) stop("Unmapped Class labels: ", paste(bad, collapse = " | "))

p_figS1 <- ggplot2::ggplot(trend_all, ggplot2::aes(x = PY, y = Pubs, color = Class, group = Class)) +
  ggplot2::geom_line(linewidth = 1.05) +
  ggplot2::geom_point(size = 2.0, show.legend = FALSE) +
  ggplot2::facet_wrap(~ Scheme, ncol = 1, scales = "free_y") +
  ggplot2::scale_x_continuous(breaks = years_vec) +
  ggplot2::labs(
    title = "Sensitivity analysis: impact of classification scheme on alpha/beta trends",
    x = "Year", y = "Publications", color = NULL
  ) +
  theme_pub(base_size = 12, legend_pos = "top") +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
  ggplot2::scale_color_manual(values = color_map, breaks = names(color_map), na.translate = FALSE)

save_plot_all(p_figS1, "Figure_S1", width_in = 10.5, height_in = 11.5, dpi_png = 600)
message("Saved: plots/Figure_S1")


## 27) Figure S3

message("Running Figure S3 (keyword centroid drift)...")

EARLY_Y0 <- 2015; EARLY_Y1 <- 2019
LATE_Y0  <- 2020; LATE_Y1  <- 2025

pal_group <- c(
  "Clinical Translation (β-RLT)" = COL_PRIMARY,
  "Dosimetry & Imaging"          = COL_SCP,
  "Rise of α-Therapy"            = COL_ALPHA,
  "Outcomes & Comparators"       = COL_OUTCOME
)

kw_long2 <- tibble::tibble(
  DocID = seq_len(nrow(M)),
  Year  = M$PY,
  KW    = split_kw(M$KW_ALL)
) %>%
  dplyr::filter(!is.na(Year), Year %in% years_use) %>%
  tidyr::unnest(KW) %>%
  dplyr::mutate(keyword_std = standardize_to_target(KW)) %>%
  dplyr::filter(!is.na(keyword_std), keyword_std %in% KW_DICT$keyword_std) %>%
  dplyr::distinct(DocID, Year, keyword_std)

kw_year <- kw_long2 %>%
  dplyr::count(Year, keyword_std, name = "n_docs")

centroid_tbl <- kw_year %>%
  dplyr::mutate(
    era = dplyr::case_when(
      Year >= EARLY_Y0 & Year <= EARLY_Y1 ~ "early",
      Year >= LATE_Y0  & Year <= LATE_Y1  ~ "late",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(era)) %>%
  dplyr::group_by(keyword_std, era) %>%
  dplyr::summarise(
    total = sum(n_docs),
    centroid = sum(Year * n_docs) / sum(n_docs),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = era, values_from = c(total, centroid), names_sep = "__") %>%
  dplyr::rename(
    total_early = total__early,
    total_late  = total__late,
    c_early     = centroid__early,
    c_late      = centroid__late
  )

plot_df_cd <- KW_DICT %>%
  dplyr::left_join(centroid_tbl, by = "keyword_std") %>%
  dplyr::filter(!is.na(c_early), !is.na(c_late), total_early > 0, total_late > 0) %>%
  dplyr::mutate(
    Label_wrapped = stringr::str_wrap(Label, width = 22)
  )

LABEL_LEVELS_WRAPPED <- KW_DICT %>%
  dplyr::arrange(Group, order_in_group) %>%
  dplyr::mutate(Label_wrapped = stringr::str_wrap(Label, width = 22)) %>%
  dplyr::pull(Label_wrapped)

plot_df_cd <- plot_df_cd %>%
  dplyr::mutate(Label_wrapped = factor(Label_wrapped, levels = rev(LABEL_LEVELS_WRAPPED)))

p_figS3 <- ggplot2::ggplot(plot_df_cd, ggplot2::aes(y = Label_wrapped)) +
  ggplot2::geom_vline(xintercept = LATE_Y0, linetype = "dashed", color = "grey65", linewidth = 0.45) +
  ggplot2::geom_segment(
    ggplot2::aes(x = c_early, xend = c_late, yend = Label_wrapped),
    color = "grey25", linewidth = 0.75, lineend = "round",
    arrow = ggplot2::arrow(type = "closed", length = grid::unit(2.4, "mm"))
  ) +
  ggplot2::geom_point(ggplot2::aes(x = c_early), color = "grey20", size = 2.4) +
  ggplot2::geom_point(ggplot2::aes(x = c_late, fill = Group), shape = 21, size = 3.2,
                      color = "grey20", stroke = 0.35) +
  ggplot2::scale_fill_manual(values = pal_group, guide = "none") +
  ggplot2::facet_grid(Group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  ggplot2::scale_x_continuous(limits = c(2015, 2025), breaks = 2015:2025,
                              expand = ggplot2::expansion(mult = c(0.01, 0.02))) +
  ggplot2::labs(
    title = "Keyword Centroid Drift (Frequency-Weighted)",
    subtitle = "2015–2019 → 2020–2025 (Keywords present in both periods)",
    x = "Centroid Year (Frequency-Weighted)",
    y = NULL
  ) +
  theme_pub(base_size = 13, legend_pos = "none") +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(color = "grey86", linewidth = 0.8),
    
    strip.placement = "outside",
    strip.background = ggplot2::element_blank(),
    strip.text.y.left = ggplot2::element_text(face = "bold", size = 14, angle = 0,
                                              margin = ggplot2::margin(r = 14), hjust = 0),
    
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 22),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 13, color = "grey25"),
    
    axis.text.y = ggplot2::element_text(size = 12, color = "grey25"),
    axis.text.x = ggplot2::element_text(size = 12, color = "grey25"),
    axis.title.x = ggplot2::element_text(size = 16),
    
    panel.spacing.y = grid::unit(1.1, "lines"),
    plot.margin = ggplot2::margin(t = 10, r = 18, b = 10, l = 36)
  )

save_plot_all(p_figS3, "Figure_S3", width_in = 13.5, height_in = 7.2, dpi_png = 600)
message("Saved: plots/Figure_S3")



