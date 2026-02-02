This repository contains a single, one-click R script (main_analysis.R) to reproduce the main tables and figures for a bibliometric/scientometric study of PSMA radioligand therapy (RLT) in prostate cancer (2015–2025), using Web of Science (WoS) Core Collection Plain Text exports.
Quick start
1) Place WoS Plain Text export file(s) under:
./data/wos_export/
2) Run the script from the repository root:
source("main_analysis.R")
3) Output locations:
Outputs will be written to the repository folders: ./outputs/ (tables) and ./plots/ (figures).
Within that run folder, outputs are written to:
• ./outputs/  (tables)
• ./plots/    (figures)
Requirements
R environment (tested example):
• R version: 4.5.0 (2025-04-11 ucrt)
• OS: Windows 11 x64 (build 26100)
• Platform: x86_64-w64-mingw32/x64
Required R packages:
• bibliometrix (5.2.0)
• dplyr (1.1.4)
• tidyr (1.3.1)
• stringr (1.6.0)
• tibble (3.3.0)
• ggplot2 (4.0.1)
• scales (1.4.0)
• openxlsx (4.2.8.1)
• patchwork (1.3.2)
• ragg (1.5.0)
What the script reproduces
Core workflow:
• Reads WoS Plain Text exports via bibliometrix::convert2df().
• Filters publications by year (default: 2015–2025).
• Deduplicates records: first by DOI; if DOI is missing, by normalized Title + First Author + Year + Source.
• Builds bibliometric summaries and generates publication / modality / dosimetry trend plots.
• Exports main manuscript tables and figures, plus selected supplementary tables and figures.
Main manuscript tables (CSV) — ./outputs/
• Table1.csv
• Table2.csv
Main manuscript figures — ./plots/ (PNG + TIFF + PDF + EPS, 600 dpi)
• Figure2.
• Figure3.
• Figure4.
• Figure5.
Supplementary tables (CSV) — ./outputs/
• Table_S1.csv
• Table_S5.csv
• Table_S6.csv
• Table_S7.csv
• Table_S8.csv
Supplementary figures — ./plots/ (PNG + TIFF + PDF + EPS, 600 dpi)
• FigureS1
• FigureS3
Notes
This README intentionally lists only the main manuscript and supplementary tables/figures produced by the script, and excludes workflow/diagnostic exports (e.g., screening logs, duplicate inspection tables, and other intermediate files).
For questions or issues, please open a GitHub Issue in this repository.
