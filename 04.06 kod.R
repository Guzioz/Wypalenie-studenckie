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

# 📦 Załaduj pakiet
library(corrplot)

# 🔍 Zmienne z ankiety
pytania <- c(
  "Jak.często.jesteś.aktywny.fizycznie.",
  "Jak.oceniasz.jakość.swojego.snu.",
  "Czy.rozwijasz.swoje.pasje.poza.naukowo.",
  "Jak.dużo.czasu.poświęcasz.na.naukę.tygodniowo.",
  "Jak.oceniasz.trudność.twojego.kierunku."
)

wypalenie <- c(
  "wyczerpanie_emocjonalne",
  "depersonalizacja",
  "satysfakcja_z_osiagniec"
)

# 📊 Macierz korelacji: pytania vs wypalenie
macierz_korelacji <- cor(ankieta[, pytania], ankieta[, wypalenie], use = "complete.obs")

# 🎨 Wykres korelacji
corrplot(macierz_korelacji, is.corr = FALSE, method = "color",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.8)

# Test chi-kwadrat dla każdej zmiennej względem Wyczerpanie.studenta

# 1. Na jakiej uczelni studiujesz?
cat("===== Na jakiej uczelni studiujesz? =====\n")
tab1 <- table(ankieta$Na.jakiej.uczelni.studiujesz., ankieta$Wyczerpanie.studenta)
print(chisq.test(tab1))

# 2. Jaki kierunek studiujesz?
cat("\n===== Jaki kierunek studiujesz? =====\n")
tab2 <- table(ankieta$Jaki.kierunek.studiujesz., ankieta$Wyczerpanie.studenta)
print(chisq.test(tab2))

# 3. Rodzaj studiów
cat("\n===== Rodzaj studiów =====\n")
tab3 <- table(ankieta$Rodzaj.studiów, ankieta$Wyczerpanie.studenta)
print(chisq.test(tab3))

# 4. Płeć
cat("\n===== Płeć =====\n")
tab4 <- table(ankieta$Płeć, ankieta$Wyczerpanie.studenta)
print(chisq.test(tab4))

# 5. Czy pracujesz?
cat("\n===== Czy pracujesz? =====\n")
tab5 <- table(ankieta$Czy.pracujesz., ankieta$Wyczerpanie.studenta)
print(chisq.test(tab5))

# 6. Miejsce zamieszkania
cat("\n===== Miejsce zamieszkania =====\n")
tab6 <- table(ankieta$Miejsce.zamieszkania., ankieta$Wyczerpanie.studenta)
print(chisq.test(tab6))

# 7. Czy jesteś singlem/singielką?
cat("\n===== Czy jesteś singlem/singielką? =====\n")
tab7 <- table(ankieta$Czy.jesteś.singlem.singielką., ankieta$Wyczerpanie.studenta)
print(chisq.test(tab7))

library(ggplot2)
library(dplyr)

# Tworzymy tabelę częstości
tab7 <- table(ankieta$Czy.jesteś.singlem.singielką., ankieta$Wyczerpanie.studenta)

# Zamieniamy tabelę na data frame
df_tab7 <- as.data.frame(tab7)
colnames(df_tab7) <- c("Status_Singla", "Wyczerpanie", "Liczba")

# Obliczamy procenty w obrębie każdej grupy Status_Singla
df_tab7 <- df_tab7 %>%
  group_by(Status_Singla) %>%
  mutate(Procent = Liczba / sum(Liczba) * 100)

# Rysujemy wykres słupkowy z procentami
ggplot(df_tab7, aes(x = Status_Singla, y = Procent, fill = Wyczerpanie)) +
  geom_bar(stat = "identity", position = "fill") +  # position = "fill" normalizuje do 100% słupka
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # etykiety w procentach
  labs(title = "Procentowy rozkład poziomu wyczerpania wg statusu singla",
       x = "Czy jesteś singlem/singielką?",
       y = "Procent osób",
       fill = "Poziom wyczerpania") +
  theme_minimal()

# Liczymy ile osób przekracza każdy próg (pomijamy NA)
ile_wyczerpanie <- sum(ankieta$wyczerpanie_emocjonalne > prog_wyczerpanie, na.rm = TRUE)
ile_depersonalizacja <- sum(ankieta$depersonalizacja > prog_depersonalizacja, na.rm = TRUE)
ile_satysfakcja <- sum(ankieta$satysfakcja_z_osiagniec > prog_satysfakcja, na.rm = TRUE)

# Liczymy ile osób NIE przekracza progu (pomijamy NA)
nie_wyczerpanie <- sum(ankieta$wyczerpanie_emocjonalne <= prog_wyczerpanie, na.rm = TRUE)
nie_depersonalizacja <- sum(ankieta$depersonalizacja <= prog_depersonalizacja, na.rm = TRUE)
nie_satysfakcja <- sum(ankieta$satysfakcja_z_osiagniec <= prog_satysfakcja, na.rm = TRUE)

# Tworzymy tabelę wynikową
tabela_progi <- data.frame(
  Zmienna = c("wyczerpanie_emocjonalne", "depersonalizacja", "satysfakcja_z_osiagniec"),
  Liczba_przekroczen = c(ile_wyczerpanie, ile_depersonalizacja, ile_satysfakcja),
  Liczba_nie_przekroczen = c(nie_wyczerpanie, nie_depersonalizacja, nie_satysfakcja)
)

print(tabela_progi)


# Funkcja do rysowania histogramu z linią progu
rysuj_histogram <- function(data, zmienna, prog, tytul) {
  ggplot(data, aes_string(x = zmienna)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_vline(xintercept = prog, color = "red", linetype = "dashed", size = 1) +
    labs(title = tytul,
         x = zmienna,
         y = "Liczba osób") +
    theme_minimal()
}

# Rysujemy histogramy
p1 <- rysuj_histogram(ankieta, "wyczerpanie_emocjonalne", prog_wyczerpanie, "Wyczerpanie emocjonalne")
p2 <- rysuj_histogram(ankieta, "depersonalizacja", prog_depersonalizacja, "Depersonalizacja")
p3 <- rysuj_histogram(ankieta, "satysfakcja_z_osiagniec", prog_satysfakcja, "Satysfakcja z osiągnięć")

# Wyświetlamy wykresy (jeśli używasz RStudio, wyświetli je kolejno)
print(p1)
print(p2)
print(p3)
      

library(nnet)

# Upewnij się, że Wyczerpanie.studenta to faktor
ankieta$Wyczerpanie.studenta <- factor(ankieta$Wyczerpanie.studenta, 
                                       levels = c("niskie", "umiarkowane", "wysokie"))

# Budujemy model regresji logistycznej wieloklasowej
model <- multinom(Wyczerpanie.studenta ~ wyczerpanie_emocjonalne + depersonalizacja + satysfakcja_z_osiagniec, data = ankieta)

# Wyświetlamy podsumowanie modelu
summary(model)

# Aby ocenić istotność, można obliczyć wartości p (przybliżone)
z <- summary(model)$coefficients / summary(model)$standard.errors
p <- 2 * (1 - pnorm(abs(z)))
print(p)

# Zamiana kategorii na liczby (np. niskie=1, umiarkowane=2, wysokie=3)
ankieta$Wyczerpanie_num <- as.numeric(factor(ankieta$Wyczerpanie.studenta, levels = c("niskie", "umiarkowane", "wysokie")))

cor(ankieta$Wyczerpanie_num, ankieta$wyczerpanie_emocjonalne, method = "spearman", use = "complete.obs")
cor(ankieta$Wyczerpanie_num, ankieta$depersonalizacja, method = "spearman", use = "complete.obs")
cor(ankieta$Wyczerpanie_num, ankieta$satysfakcja_z_osiagniec, method = "spearman", use = "complete.obs")

library(ggplot2)
library(dplyr)
library(tidyr)

# Usuwamy brakujące wartości potrzebnych kolumn
df <- ankieta %>%
  select(Wyczerpanie.studenta, wyczerpanie_emocjonalne, depersonalizacja, satysfakcja_z_osiagniec) %>%
  filter(!is.na(Wyczerpanie.studenta))

# Obliczamy średnie dla każdej grupy i zmiennej
df_summary <- df %>%
  group_by(Wyczerpanie.studenta) %>%
  summarise(
    srednie_wyczerpanie = mean(wyczerpanie_emocjonalne, na.rm = TRUE),
    srednia_depersonalizacja = mean(depersonalizacja, na.rm = TRUE),
    srednia_satysfakcja = mean(satysfakcja_z_osiagniec, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -Wyczerpanie.studenta,
               names_to = "Zmienna",
               values_to = "Srednia")

# Zamiana nazw na czytelniejsze
df_summary$Zmienna <- recode(df_summary$Zmienna,
                             srednie_wyczerpanie = "Wyczerpanie emocjonalne",
                             srednia_depersonalizacja = "Depersonalizacja",
                             srednia_satysfakcja = "Satysfakcja z osiągnięć")

# Rysujemy wykres
ggplot(df_summary, aes(x = Wyczerpanie.studenta, y = Srednia, fill = Zmienna)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Średnie wartości zmiennych wg poziomu wypalenia studenta",
       x = "Poziom wypalenia studenta",
       y = "Średnia wartość",
       fill = "Zmienna") +
  theme_minimal()

library(dplyr)
library(ggplot2)

# Zakładam, że progi prog_wyczerpanie, prog_depersonalizacja, prog_satysfakcja są już wyliczone

# 1. Dodajemy kolumny logiczne przekroczenia progów
ankieta <- ankieta %>%
  mutate(
    przekroczenie_wyczerpanie = wyczerpanie_emocjonalne > prog_wyczerpanie,
    przekroczenie_depersonalizacja = depersonalizacja > prog_depersonalizacja,
    przekroczenie_satysfakcja = satysfakcja_z_osiagniec > prog_satysfakcja
  )

# 2. Tworzymy tabelę liczby osób wg kombinacji progów i poziomu wypalenia
tabela_kombinacji <- ankieta %>%
  filter(!is.na(Wyczerpanie.studenta)) %>%
  group_by(Wyczerpanie.studenta,
           przekroczenie_wyczerpanie,
           przekroczenie_depersonalizacja,
           przekroczenie_satysfakcja) %>%
  summarise(liczba = n(), .groups = "drop") %>%
  mutate(
    przekroczenie_wyczerpanie = ifelse(przekroczenie_wyczerpanie, "Tak", "Nie"),
    przekroczenie_depersonalizacja = ifelse(przekroczenie_depersonalizacja, "Tak", "Nie"),
    przekroczenie_satysfakcja = ifelse(przekroczenie_satysfakcja, "Tak", "Nie"),
    kombinacja = paste0(
      "Wyczerpanie: ", przekroczenie_wyczerpanie, ", ",
      "Depers.: ", przekroczenie_depersonalizacja, ", ",
      "Satysf.: ", przekroczenie_satysfakcja
    )
  )

print(tabela_kombinacji)
library(dplyr)
library(ggplot2)

# Przyjmuję, że tabela_kombinacji jest już przygotowana jak wcześniej:
# tabela_kombinacji z kolumnami: Wyczerpanie.studenta, kombinacja, liczba

# Filtrujemy, porządkujemy i rysujemy oddzielnie dla każdej grupy
stopnie <- unique(tabela_kombinacji$Wyczerpanie.studenta)

for(stopien in stopnie) {
  df_filtr <- tabela_kombinacji %>%
    filter(Wyczerpanie.studenta == stopien, liczba >= 5) %>%
    arrange(desc(liczba))
  
  p <- ggplot(df_filtr, aes(x = reorder(kombinacja, liczba), y = liczba, fill = liczba)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = paste("Liczba osób wg kombinacji przekroczeń progów\nPoziom wypalenia:", stopien),
      x = "Kombinacja przekroczeń progów",
      y = "Liczba osób"
    ) +
    theme_minimal() +
    guides(fill = "none") +
    theme(axis.text.y = element_text(size = 9))
  
  print(p)
}
