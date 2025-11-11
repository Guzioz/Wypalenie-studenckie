# --- Wczytywanie danych ---
library(readxl)
ankieta <- read_xlsx("Wypalenie-wsrod-osob-studiujacych-2025-05-09.xlsx")
ankieta <- data.frame(ankieta)

## --- Pakiety ---
required_packages <- c("dplyr", "ggplot2", "finalfit", "VIM", "validate",
                       "errorlocate", "tidyverse", "ggcorrplot", "forcats",
                       "ggthemes", "dlookr", "editrules", "hrbrthemes", "plotly",
                       "ISLR", "gapminder", "kableExtra", "ggstatsplot", "gtsummary",
                       "readr", "rmarkdown", "moments", "knitr", "writexl", "caret")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

# 1. Instalacja wszystkich brakujących pakietów
sapply(required_packages, install_if_missing)

# 2. Ładowanie pakietów do środowiska globalnego
# Używamy pętli for dla pewności, że każdy pakiet jest załadowany jawnie.
for (pkg in required_packages) {
  # suppressMessages i suppressWarnings ukrywają długie komunikaty powitalne
  suppressMessages(suppressWarnings(library(pkg, character.only = TRUE)))
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### ONE HOT ENCODING

# Usuwamy kolumny, które się powtarzają -czyli kończą się na "wartosc"
ankieta <- ankieta %>%
  select(-ends_with("wartość"))

# Wybór TYLKO kolumn kategorycznych, które mają być zakodowane
dane_kategoryczne <- ankieta %>%
  select(where(is.factor) | where(is.character))
# Oblicz liczbę unikalnych poziomów dla każdej kolumny
unikalne_poziomy <- sapply(dane_kategoryczne, n_distinct)

# Zidentyfikuj nazwy kolumn, gdzie liczba unikalnych wartości wynosi 1
kolumny_do_usuniecia <- names(unikalne_poziomy[unikalne_poziomy <= 1])

# Usuń te kolumny z oryginalnej ramki danych
dane_kategoryczne <- dane_kategoryczne %>%
  select(-all_of(kolumny_do_usuniecia))

# Wybór TYLKO kolumn numerycznych, które mają pozostać bez zmian
dane_numeryczne <- ankieta %>%
  select(where(is.numeric))

# A. UTWORZENIE OBIEKTU TRANSFORMUJĄCEGO
# Formuła: '~ .' oznacza "użyj wszystkich kolumn w danych_kategoryczne"
dmy_obj <- dummyVars(
  formula = ~ ., 
  data = dane_kategoryczne, 
  fullRank = FALSE # Pełne kodowanie One-Hot (bez usuwania jednej kategorii jako bazowej)
)

# B. WYKONANIE TRANSFORMACJI
# Transformacja DANYCH KATEGORYCZNYCH
one_hot_encoded_macierz <- predict(dmy_obj, newdata = dane_kategoryczne)

# Konwersja macierzy na ramkę danych
one_hot_encoded_df <- as.data.frame(one_hot_encoded_macierz)

# C. SCALENIE
# Połącz kolumny numeryczne (dane_numeryczne) z nowo zakodowanymi kolumnami (one_hot_encoded_df)
ankieta<- bind_cols(ankieta, one_hot_encoded_df)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
# Usuń kolumny kończące się na "imp" - które są niepotrzebne
ankieta <- ankieta %>%
  select(-ends_with("imp"))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
  ### OBLICZANIE WYPALENIA
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
prog_wyczerpanie <- mean(ankieta$wyczerpanie_emocjonalne, na.rm = TRUE) 

prog_depersonalizacja <- mean(ankieta$depersonalizacja, na.rm = TRUE) 

prog_satysfakcja <- mean(ankieta$satysfakcja_z_osiagniec, na.rm = TRUE)

summary(ankieta[, c("wyczerpanie_emocjonalne", "depersonalizacja", "satysfakcja_z_osiagniec")])

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
#-------------------------------------------------------------------------------------------------------------------------------------------------------
  ### WIZUALIZACJE

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

# ANOVA: Wyczerpanie emocjonalne*
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

# 4. Płeć*
cat("\n===== Płeć =====\n")
tab4 <- table(ankieta$Płeć, ankieta$Wyczerpanie.studenta)
print(chisq.test(tab4))

# 5. Czy pracujesz?
cat("\n===== Czy pracujesz? =====\n")
tab5 <- table(ankieta$Czy.pracujesz., ankieta$depersonalizacja)
print(chisq.test(tab5))

# 6. Miejsce zamieszkania
cat("\n===== Miejsce zamieszkania =====\n")
tab6 <- table(ankieta$Miejsce.zamieszkania., ankieta$Wyczerpanie.studenta)
print(chisq.test(tab6))

# 7. Czy jesteś singlem/singielką?
cat("\n===== Czy jesteś singlem/singielką? =====\n")
tab7 <- table(ankieta$Czy.jesteś.singlem.singielką., ankieta$Wyczerpanie.studenta)
print(chisq.test(tab7))


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


ggplot(df_summary, aes(x = Wyczerpanie.studenta, y = Srednia, fill = Zmienna)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(
    values = c("#a8ddb5", "green", "#006d2c")  
  ) +
  labs(
    title = "Average values of variables by burnout level",
    x = "Burnout level",
    y = "Average value",
    fill = "Variable"
  ) +
  theme_minimal()



df_summary <- df_summary %>%
  mutate(
    Burnout = recode(Wyczerpanie.studenta,
                     "niskie" = "Low",
                     "umiarkowane" = "Medium",
                     "wysokie" = "High"),
    Variable = recode(Zmienna,
                      "Satysfakcja z osiągnięć" = "Satisfaction with Achievements",
                      "Wyczerpanie emocjonalne" = "Emotional exhaustion",
                      "Depersonalizacja" = "Depersonalization")  # dodaj więcej, jeśli masz inne zmienne
  )

ggplot(df_summary, aes(x = Burnout, y = Srednia, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(
    values = c("#a8ddb5", "green", "#006d2c")  # three shades of green
  ) +
  labs(
    title = "Average values of variables by burnout level",
    x = "Burnout level",
    y = "Average value",
    fill = "Variable"
  ) +
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

for (pyt in pytania) {
  for (wyp in wypalenie) {
    # Budujemy formułę ANOVA, np. wyczerpanie_emocjonalne ~ Jak.często.jesteś.aktywny.fizycznie.
    formula_anova <- as.formula(paste(wyp, "~", pyt))
    
    # Sprawdzamy, czy w kolumnach nie ma za dużo NA
    dane <- ankieta[, c(wyp, pyt)]
    dane <- dane[complete.cases(dane), ]
    
    if (nrow(dane) > 10) {  # minimalna liczba obserwacji, żeby test miał sens
      anova_res <- aov(formula_anova, data = dane)
      cat("\nANOVA dla:", wyp, "względem", pyt, "\n")
      print(summary(anova_res))
    } else {
      cat("\nZa mało danych do analizy dla:", wyp, "i", pyt, "\n")
    }
  }
}

library(ggplot2)
library(dplyr)

# Przygotowujemy dane z procentami w grupach płci
df_procent <- ankieta %>%
  filter(!is.na(Płeć), !is.na(Wyczerpanie.studenta)) %>%
  group_by(Płeć, Wyczerpanie.studenta) %>%
  summarise(liczba = n(), .groups = "drop") %>%
  group_by(Płeć) %>%
  mutate(procent = liczba / sum(liczba) * 100)

# Rysujemy wykres słupkowy procentowy
ggplot(df_procent, aes(x = Płeć, y = procent, fill = Wyczerpanie.studenta)) +
  geom_bar(stat = "identity", position = "fill") +  
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Percentage distribution of burout level by gender",
    x = "Gender",
    y = "Percent of responders",
    fill = "Burnout level"
  ) +
  theme_minimal()

ggplot(df_procent, aes(x = Płeć, y = procent, fill = Wyczerpanie.studenta)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(
    values = c("#a8ddb5", "green", "#006d2c")  # trzy odcienie zieleni
  ) +
  labs(
    title = "Percentage distribution of student burnout level by gender",
    x = "Gender",
    y = "Percentage of respondents",
    fill = "Burnout level"
  ) +
  theme_minimal()


df_procent <- df_procent %>%
  mutate(
    Gender = recode(Płeć,
                    "Kobieta" = "Female",
                    "Mężczyzna" = "Male"),
    Burnout = recode(Wyczerpanie.studenta,
                     "niskie" = "Low",
                     "umiarkowane" = "Medium",
                     "wysokie" = "High")
  )

ggplot(df_procent, aes(x = Gender, y = procent, fill = Burnout)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(
    values = c("#a8ddb5", "green", "#006d2c")  # three shades of green
  ) +
  labs(
    title = "Percentage distribution of student burnout level by gender",
    x = "Gender",
    y = "Percentage of respondents",
    fill = "Burnout level"
  ) +
  theme_minimal()
write.csv(ankieta, file = "ankieta.csv", row.names = FALSE)

#gghistostats dla wyczerpanie_emocjonalne
library(ggstatsplot)
library(ggplot2)
gghistostats(
  data = ankieta, 
  x = wyczerpanie_emocjonalne,
  binwidth = 0.5,
  xlab = "Emotional Exhaustion",
  title = "Distribution of emotional exhaustion among students",
  caption = NULL,
  type = "parametric",
  bf.message = FALSE,
  bin.args = list(color = "black", fill = "darkgreen", alpha = 0.7),
)

#gghistostats dla depersonalizacja
gghistostats(
  data = ankieta, 
  x = depersonalizacja,
  binwidth = 0.5,
  xlab = "Depersonalization",
  title = "Distribution of depersonalization among students",
  caption = NULL,
  type = "parametric",
  bf.message = FALSE,
  bin.args = list(color = "black", fill = "darkgreen", alpha = 0.7),
)

#gghistostats dla satysfakcja_z_osiagniec
gghistostats(
  data = ankieta, 
  x = satysfakcja_z_osiagniec,
  binwidth = 0.5,
  xlab = "Satisfaction with Achievements",
  title = "Distribution of satisfaction with achievements among students",
  caption = NULL,
  type = "parametric",
  bf.message = FALSE,
  bin.args = list(color = "black", fill = "darkgreen", alpha = 0.7),
)

# catterplot dla satysfakcji z osiagniec i jak duzo czasu poswiecasz na nauke tygodniowo
ggplot(ankieta, aes(x = factor(Czy.rozwijasz.swoje.pasje.poza.naukowo.), 
                    y = satysfakcja_z_osiagniec)) +
  geom_boxplot(fill = "darkgreen", alpha = 0.6) +
  labs(
    x = "Czy rozwijasz swoje pasje poza naukowo?",
    y = "Satysfakcja z osiągnięć",
    title = "Związek pomiędzy rozwijaniem pasji a satysfakcją z osiągnięć"
  ) +
  theme_minimal()

ggplot(ankieta, aes(x = Jak.oceniasz.trudność.twojego.kierunku., y = wyczerpanie_emocjonalne)) +
  geom_point(color = "darkgreen") +
  labs(
    x = "How do you rate the difficulty of your field of study?",
    y = "Emotional Exhaustion",
    title = "The relationship between the difficulty of the field of study and emotional exhaustion"
  )+
  theme_minimal() 

ggplot(ankieta, aes(x = Jak.oceniasz.trudność.twojego.kierunku., y = depersonalizacja)) +
  geom_point(color = "darkgreen") +
  labs(
    x = "How do you rate the difficulty of your field of study?",
    y = "Depersonalization",
    title = "The relationship between the difficulty of the field of study and depersonalization"
  )+
  theme_minimal() 
#-----------------------------------------------------------------------------------------------------------------------------------------------------
### MODELOWANIE
#---------------------------------------------------------------------------------------------------------------------------------------------------------
# Wymagany pakiet do czyszczenia nazw
install.packages("janitor")
library(janitor) 

# --- Krok 0: Oczyszczenie Nazw Kolumn ---
# To usunie spacje, myślniki i inne znaki z nazw kolumn
ankieta <- ankieta %>%
  janitor::clean_names()
# Wybór TYLKO kolumn numerycznych, które mają pozostać bez zmian
  model_data <- ankieta %>%
      select(where(is.numeric) & !matches("wyczerpanie_emocjonalne|satysfakcja_z_osiagniec|depersonalizacja"), "wyczerpanie_studenta")  

# Wymagany pakiet do Random Forest
install.packages("randomForest")
library(randomForest)

# Zakładam, że zmienna zależna jest już faktorem (jeśli nie, zrób to):
model_data$wyczerpanie_studenta <- as.factor(model_data$wyczerpanie_studenta)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
### RANDOM FOREST
#--------------------------------------------------------------------------------------------------------------------------------------------------------------
# Budowa pełnego modelu Random Forest
full_rf_model <- randomForest(
  wyczerpanie_studenta ~ .,
  data = model_data,
  ntree = 500, # Liczba drzew
  importance = TRUE
)
# Wyodrębnienie ważności zmiennych
importance_df <- as.data.frame(importance(full_rf_model, type = 2)) # type=2 to MeanDecreaseGini
importance_df$Variable <- rownames(importance_df)
print(importance_df)
# Sortowanie i wybór top N zmiennych
top_n_importance <- importance_df %>%
  arrange(desc(MeanDecreaseGini)) %>%
  slice_head(n = 10) # Przykładowo, wybieram 10 najbardziej istotnych

# Wypisz 10 najważniejszych predyktorów
print(top_n_importance)

# Wyodrębnienie nazw najlepszych predyktorów
best_predictors <- top_n_importance$Variable

# Utworzenie nowej formuły modelu tylko z najlepszymi zmiennymi
# paste0() i as.formula() pomagają w dynamicznym tworzeniu formuły
new_formula <- as.formula(paste("wyczerpanie_studenta ~", paste(best_predictors, collapse = " + ")))

# Budowa uproszczonego modelu Random Forest
simplified_rf_model <- randomForest(
  new_formula,
  data = model_data,
  ntree = 500
)

# Porównaj błąd obu modeli (Out-of-Bag Error)
print("Pełny model:")
print(full_rf_model)
print("Uproszczony model (Top 10):")
print(simplified_rf_model)

#--------------------------------------------------------------------------------------------------------------------------------------------
### MODEL REGRESJI LOGISTYCZNEJ WIELOMIANOWEJ
#--------------------------------------------------------------------------------------------------------------------------------------------

# Wymagany pakiet
# install.packages("nnet") # Odkomentuj, jeśli nie masz zainstalowanego
install.packages("nnet")
library(nnet)
# Budowa pełnego modelu wielomianowego
# Wyczerpanie.studenta ~ . oznacza, że wszystkie inne kolumny są predyktorami
full_multinom_model <- multinom(
  wyczerpanie_studenta ~ ., 
  data = model_data,
  MaxNWts = 2000 # Zwiększ, jeśli dostaniesz ostrzeżenie o zbyt małej liczbie wag
)

# Podsumowanie modelu (często wyświetla się tylko część)
summary(full_multinom_model)

# Wymagany pakiet do selekcji krokowej (zazwyczaj MASS jest wczytane)
library(MASS) 

# Selekcja wsteczna (Backward Elimination)
# direction="backward" usuwa predyktory, które najmniej obniżają AIC
simplified_multinom_model <- step(
  full_multinom_model, 
  direction = "backward",
  trace = FALSE # Ukrywa iteracyjne komunikaty
)

# Wyświetlenie formuły najlepszego modelu
print("Ostateczna formuła po selekcji wstecznej:")
print(simplified_multinom_model$call$formula)

# Podsumowanie uproszczonego modelu
summary(simplified_multinom_model)

print(paste("AIC modelu pełnego:", full_multinom_model$AIC))
print(paste("AIC modelu uproszczonego:", simplified_multinom_model$AIC))

# Predykcja na danych treningowych
predykcje <- predict(simplified_multinom_model, newdata = model_data)

# Macierz pomyłek
multinom_confusion_matrix <- table(
  "Oczekiwane" = model_data$wyczerpanie_studenta, 
  "Przewidziane" = predykcje
)

print("Macierz pomyłek dla modelu wielomianowego po selekcji wstecznej:")
print(multinom_confusion_matrix)

# Obliczanie ogólnej dokładności (accuracy)
dokladnosc <- sum(diag(multinom_confusion_matrix)) / sum(multinom_confusion_matrix)
print(paste("Dokładność modelu (na danych treningowych):", round(dokladnosc, 4)))