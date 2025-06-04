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

#sumowanie wartości dla wypalenia emocjonalnego
ankieta$wyczerpanie_emocjonalne <- rowSums(ankieta[, c(
  "Jak.bardzo.czujesz.się.przytłoczony.nadmiarem.obowiązków.",
  "Czy.uważasz.że.praca.w.grupie.sprawia.ci.trudność.",
  "U.jakiej.części.twoich.znajomych.zauważasz.oznaki.wypalenia.",
  "Jak.często.czujesz.się.zmęczony.a.fizycznie.",
  "Jak.często.czujesz.się.emocjonalnie.wyczerpany.",
  "Czy.odczuwasz.przewlekły.stres."
)])

#sumowanie wartości dla satysfakcji z osiągnięć
ankieta$satysfakcja_z_osiagniec <- rowSums(ankieta[, c(
  "Czy.uważasz..że.ilość.nauki.jest.powiązana.z.wynikami.",
  "Czy.czujesz.że.masz.wsparcie.w.swoich.znajomych.ze.studiów.",
  "Czy.uważasz.że.masz.wsparcie.u.rodziny.i.lub.swoich.znajomych.spoza.studiów.",
  "Czy.uważasz.że.masz.dobre.relacje.z.prowadzącymi."
)])

#sumowanie wartości dla depersonalizacji
ankieta$depersonalizacja <- rowSums(ankieta[, c(
  "Jak.często.uważasz..że.kwestionujesz.swoje.decyzje.",
  "Czy.uważasz.że.masz.tendencje.do.przepracowywania.się.",
  "Jak.często.odkładasz.zadania.na.później."
)])
#Obliczanie progu wysokiego dla wyczerpania emocjonalnego
prog_wyczerpanie <- mean(ankieta$wyczerpanie_emocjonalne, na.rm = TRUE) + 
  sd(ankieta$wyczerpanie_emocjonalne, na.rm = TRUE) * 0.5
#obliczanie prgu wyokiego dla depersonalizacji
prog_depersonaliazcja <- mean(ankieta$depersonalizacja, na.rm = TRUE) + 
  sd(ankieta$depersonalizacja, na.rm = TRUE) * 1.25
#obliczanie progu wysokiego dla satysfakcji z osiągnięć
prog_satysfakcja <- mean(ankieta$satysfakcja_z_osiagniec, na.rm = TRUE) + 
  sd(ankieta$satysfakcja_z_osiagniec, na.rm = TRUE) * 0.5



klasyfikuj_wyczerpanie <- function(w, d, s) {
  w_przekroczone <- w > prog_wyczerpanie
  d_przekroczone <- d > prog_depersonalizacja
  s_przekroczone <- s > prog_satysfakcja
  
  if (w_przekroczone & d_przekroczone & !s_przekroczone) {
    return("wysokie")
  } else if (
    ((w_przekroczone | d_przekroczone) & !s_przekroczone) |
    (w_przekroczone & d_przekroczone & s_przekroczone)
  ) {
    return("umiarkowane")
  } else {
    return("niskie")
  }
}

#dodawanie nowej kolumny z ogólnym wyczerpaniem
ankieta$Wyczerpanie.studenta <- mapply(
  klasyfikuj_wyczerpanie,
  ankieta$Wyczerpanie.emocjonalne,
  ankieta$Depersonalizacja,
  ankieta$Satysfakcja.z.osiągnięć
)
