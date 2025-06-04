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
ankieta <- hotdeck(ankieta)
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
# Oblicz progi
prog_wyczerpanie <- mean(ankieta$wyczerpanie_emocjonalne, na.rm = TRUE) + 
  sd(ankieta$wyczerpanie_emocjonalne, na.rm = TRUE)

prog_depersonalizacja <- mean(ankieta$depersonalizacja, na.rm = TRUE) + 
  sd(ankieta$depersonalizacja, na.rm = TRUE)

prog_satysfakcja <- mean(ankieta$satysfakcja_z_osiagniec, na.rm = TRUE) + 
  sd(ankieta$satysfakcja_z_osiagniec, na.rm = TRUE) 

# Prawidłowo działająca funkcja klasyfikująca
klasyfikuj_wyczerpanie <- function(w, d, s) {
  w_przekroczone <- w > prog_wyczerpanie
  d_przekroczone <- d > prog_depersonalizacja
  s_przekroczone <- s > prog_satysfakcja
  
  if (is.na(w) | is.na(d) | is.na(s)) {
    return(NA)  # obsługa braków danych
  } else if (w_przekroczone & d_przekroczone & !s_przekroczone) {
    return("wysokie")
  } else if (((w_przekroczone | d_przekroczone) & !s_przekroczone) |
             (w_przekroczone & d_przekroczone & s_przekroczone)) {
    return("umiarkowane")
  } else {
    return("niskie")
  }
}

# Klasyfikacja – działa teraz poprawnie
ankieta$Wyczerpanie.studenta <- mapply(
  klasyfikuj_wyczerpanie,
  ankieta$wyczerpanie_emocjonalne,
  ankieta$depersonalizacja,
  ankieta$satysfakcja_z_osiagniec
)
table(ankieta$Wyczerpanie.studenta)

ggplot(ankieta, aes(x = Wyczerpanie.studenta,
                 fill = as.factor(Czy.uważasz.że.masz.tendencje.do.przepracowywania.się.))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Tendencja do przepracowywania się wg poziomu ryzyka wypalenia",
    x = "Ryzyko wypalenia",
    y = "Procent odpowiedzi",
    fill = "Tendencja do przepracowywania się"
  ) +
  theme_minimal()

# ANOVA: Wyczerpanie emocjonalne
anova_emocjonalne <- aov(wyczerpanie_emocjonalne ~ Płeć.wartość, data = ankieta)
summary(anova_emocjonalne)

# ANOVA: Depersonalizacja
anova_depersonalizacja <- aov(depersonalizacja ~ Płeć.wartość, data = ankieta)
summary(anova_depersonalizacja)

# ANOVA: Satysfakcja z osiągnięć
anova_satysfakcja <- aov(satysfakcja_z_osiagniec ~ Płeć.wartość, data = ankieta)
summary(anova_satysfakcja)


# Wyczerpanie emocjonalne
ggplot(ankieta, aes(x = Płeć.wartość, y = wyczerpanie_emocjonalne)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Wyczerpanie emocjonalne a płeć", x = "Płeć", y = "Wyczerpanie emocjonalne") +
  theme_minimal()

# Depersonalizacja
ggplot(ankieta, aes(x = Płeć.wartość, y = depersonalizacja)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Depersonalizacja a płeć", x = "Płeć", y = "Depersonalizacja") +
  theme_minimal()

# Satysfakcja z osiągnięć
ggplot(ankieta, aes(x = Płeć.wartość, y = satysfakcja_z_osiagniec)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Satysfakcja z osiągnięć a płeć", x = "Płeć", y = "Satysfakcja z osiągnięć") +
  theme_minimal()
# ANOVA: Wyczerpanie emocjonalne
anova_emocjonalne_singiel <- aov(wyczerpanie_emocjonalne ~ Czy.jesteś.singlem.singielką., data = ankieta)
summary(anova_emocjonalne_singiel)

# ANOVA: Depersonalizacja
anova_depersonalizacja_singiel <- aov(depersonalizacja ~ Czy.jesteś.singlem.singielką., data = ankieta)
summary(anova_depersonalizacja_singiel)

# ANOVA: Satysfakcja z osiągnięć
anova_satysfakcja_singiel <- aov(satysfakcja_z_osiagniec ~ Czy.jesteś.singlem.singielką., data = ankieta)
summary(anova_satysfakcja_singiel)
# ANOVA: Wyczerpanie emocjonalne
anova_emocjonalne_sen <- aov(wyczerpanie_emocjonalne ~ Jak.oceniasz.jakość.swojego.snu., data = ankieta)
summary(anova_emocjonalne_sen)

# ANOVA: Depersonalizacja
anova_depersonalizacja_sen <- aov(depersonalizacja ~ Jak.oceniasz.jakość.swojego.snu., data = ankieta)
summary(anova_depersonalizacja_sen)

# ANOVA: Satysfakcja z osiągnięć
anova_satysfakcja_sen <- aov(satysfakcja_z_osiagniec ~ Jak.oceniasz.jakość.swojego.snu., data = ankieta)
summary(anova_satysfakcja_sen)
#anova, t-studenta, chikwadrat, piramida korelacji