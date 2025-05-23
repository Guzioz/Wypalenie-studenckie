# --- Wczytywanie danych ---
library(readxl)
ankieta <- read_excel("Wypalenie-wsrod-osob-studiujacych-2025-05-09.xlsx")
ankieta <- data.frame(ankieta)

# --- Pakiety ---
required_packages <- c("dplyr", "ggplot2", "finalfit", "VIM", "validate", 
                       "errorlocate", "tidyverse", "ggcorrplot", "forcats",
                       "ggthemes", "dlookr", "editrules", "hrbrthemes", "plotly",
                       "ISLR", "gapminder", "kableExtra", "ggstatsplot", "gtsummary",
                       "readr", "rmarkdown", "moments", "knitr", "writexl")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
sapply(required_packages, install_if_missing)
lapply(required_packages, library, character.only = TRUE)

# --- Czyszczenie danych ---
# Usuń kolumny, które są w całości NA
ankieta <- ankieta[, colSums(!is.na(ankieta)) > 0]

# Usuń kolumnę "data", jeśli istnieje
if ("data" %in% colnames(ankieta)) {
  ankieta <- ankieta[, colnames(ankieta) != "data"]
}

# Usuń wiersze, w których pierwsza kolumna to "Nie"
ankieta <- ankieta[ankieta[[1]] != "Nie", ]

# Usuń wiersze z NA w 11. kolumnie
ankieta <- ankieta[!is.na(ankieta[[11]]), ]

# Uczyń kolumny 21:n jako liczbowe, przekształcając "5 lub więcej" na "5"
ankieta[, 21:ncol(ankieta)] <- lapply(ankieta[, 21:ncol(ankieta)], function(col) {
  col <- as.character(col)
  col[col == "5 lub więcej"] <- "5"
  as.numeric(col)
})

# --- Zamiana ocen 1 ↔ 5, 2 ↔ 4
kolumny_do_zmiany <- c(
  "Czy.czujesz.że.masz.wsparcie.w.swoich.znajomych.ze.studiów.",
  "Czy.uważasz.że.masz.wsparcie.u.rodziny.i.lub.swoich.znajomych.spoza.studiów.",
  "Czy.uważasz.że.masz.dobre.relacje.z.prowadzącymi.",
  "Jak.oceniasz.jakość.swojego.snu.",
  "Czy.rozwijasz.swoje.pasje.poza.naukowo.",
  "Jak.często.jesteś.aktywny.fizycznie."
)

ankieta[kolumny_do_zmiany] <- lapply(ankieta[kolumny_do_zmiany], function(x) {
  x <- as.numeric(x)
  ifelse(x == 1, 5,
         ifelse(x == 2, 4,
                ifelse(x == 4, 2,
                       ifelse(x == 5, 1, x))))
})

kolumny_do_zmiany[!kolumny_do_zmiany %in% colnames(ankieta)]
colnames(ankieta)

# --- Wyliczenie wskaźnika wypalenia ---
ankieta$wypalenie_studenckie <- rowSums(ankieta[, 21:ncol(ankieta)], na.rm = TRUE)
