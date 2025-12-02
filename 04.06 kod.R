# --- Wczytywanie danych ---
library(readxl)
ankieta <- read_xlsx("Wypalenie-wsrod-osob-studiujacych-2025-11-22.xlsx")
ankieta <- data.frame(ankieta)

# 1. Lista bibliotek do instalacji
required_packages <- c("dplyr", "ggplot2", "finalfit", "VIM", "validate",
                       "errorlocate", "tidyverse", "ggcorrplot", "forcats",
                       "ggthemes", "dlookr", "editrules", "hrbrthemes", "plotly",
                       "ISLR", "gapminder", "kableExtra", "ggstatsplot", "gtsummary",
                       "readr", "rmarkdown", "moments", "knitr", "writexl", "caret")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) install.packages(new_packages, repos = "https://cloud.r-project.org/")

# Komunikat końcowy
message("Gotowe. Wszystkie pakiety są zainstalowane.")
# 4. Załaduj wszystkie pakiety
lapply(required_packages, library, character.only = TRUE)
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
anova_emocjonalne <- aov(wyczerpanie_emocjonalne ~ Płeć, data = ankieta)
summary(anova_emocjonalne)

# ANOVA: Depersonalizacja
anova_depersonalizacja <- aov(depersonalizacja ~ Płeć, data = ankieta)
summary(anova_depersonalizacja)

# ANOVA: Satysfakcja z osiągnięć
anova_satysfakcja <- aov(satysfakcja_z_osiagniec ~ Płeć, data = ankieta)
summary(anova_satysfakcja)


# Wyczerpanie emocjonalne
ggplot(ankieta, aes(x = Płeć, y = wyczerpanie_emocjonalne)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Wyczerpanie emocjonalne a płeć", x = "Płeć", y = "Wyczerpanie emocjonalne") +
  theme_minimal()

# Depersonalizacja
ggplot(ankieta, aes(x = Płeć, y = depersonalizacja)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Depersonalizacja a płeć", x = "Płeć", y = "Depersonalizacja") +
  theme_minimal()

# Satysfakcja z osiągnięć
ggplot(ankieta, aes(x = Płeć, y = satysfakcja_z_osiagniec)) +
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
install.packages("ggstatsplot")
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
# install.packages("nnet")
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








# ----Inaczej zbudwany model regresji logistycznej wielomianowej z podziałem na dane treningowe i testowe------------------------------------------

# Wymagane pakiety
# install.packages("nnet")
# install.packages("caret") # Potrzebny do ładnego podziału danych
library(nnet)
library(MASS) # Do funkcji step()
library(caret) # Do funkcji createDataPartition()

# --- KROK 0: Przygotowanie danych (Podział na treningowe i testowe) ---
# Zakładam, że Twoja główna ramka danych nazywa się 'model_data'.

set.seed(123) # Ustawiamy ziarno losowości, żeby wyniki były powtarzalne

# Tworzymy indeksy do podziału (np. 80% na trening, 20% na test)
# Funkcja createDataPartition dba o to, żeby proporcje kategorii w zmiennej wynikowej były zachowane.
trainIndex <- createDataPartition(model_data$wyczerpanie_studenta, p = .8,
                                  list = FALSE,
                                  times = 1)

# Tworzymy fizycznie dwa zbiory danych
train_data <- model_data[ trainIndex,] # Dane do nauki (80%)
test_data  <- model_data[-trainIndex,] # Dane do sprawdzenia (20%)

message(paste("Liczba obserwacji treningowych:", nrow(train_data)))
message(paste("Liczba obserwacji testowych:", nrow(test_data)))
message("--------------------------------------------------")


# --- KROK 1: Budowa modelu na DANYCH TRENINGOWYCH ---

# Budowa pełnego modelu wielomianowego (używamy train_data!)
full_multinom_model <- multinom(
  wyczerpanie_studenta ~ .,
  data = train_data, # ZMIANA: model uczy się tylko na treningowych
  MaxNWts = 5000, # Zwiększyłem profilaktycznie
  trace = FALSE # Ukrywa komunikaty o iteracjach
)

# Selekcja wsteczna (Backward Elimination) na modelu treningowym
simplified_multinom_model <- step(
  full_multinom_model,
  direction = "backward",
  trace = FALSE # Ukrywa iteracyjne komunikaty
)

# Wyświetlenie formuły najlepszego modelu
print("Ostateczna formuła po selekcji wstecznej:")
print(simplified_multinom_model$call$formula)


# --- KROK 2: Ocena na danych TRENINGOWYCH ---

message("\n--- WYNIKI NA ZBIORZE TRENINGOWYM (NAUKA) ---")
# Predykcja na danych treningowych
train_predykcje <- predict(simplified_multinom_model, newdata = train_data)

# Macierz pomyłek treningowa
train_confusion_matrix <- table(
  "Oczekiwane (Train)" = train_data$wyczerpanie_studenta,
  "Przewidziane (Train)" = train_predykcje
)
print(train_confusion_matrix)

# Dokładność treningowa
train_dokladnosc <- sum(diag(train_confusion_matrix)) / sum(train_confusion_matrix)
print(paste("Dokładność (Train):", round(train_dokladnosc, 4),
            "czyli", round(train_dokladnosc*100, 2), "%"))


# --- KROK 3: Ocena na danych TESTOWYCH (TO JEST NOWE) ---

message("\n--- WYNIKI NA ZBIORZE TESTOWYM (EGZAMIN) ---")
message("To jest najważniejszy wynik - jak model radzi sobie z nowymi danymi.")

# Predykcja na danych TESTOWYCH
# Używamy modelu nauczonego na train, ale każemy mu przewidywać dla test_data
test_predykcje <- predict(simplified_multinom_model, newdata = test_data)

# Macierz pomyłek TESTOWA
test_confusion_matrix <- table(
  "Oczekiwane (Test)" = test_data$wyczerpanie_studenta,
  "Przewidziane (Test)" = test_predykcje
)
print(test_confusion_matrix)

# Dokładność TESTOWA (Accuracy)
test_dokladnosc <- sum(diag(test_confusion_matrix)) / sum(test_confusion_matrix)
print(paste("DOKŁADNOŚĆ MODELU NA DANYCH TESTOWYCH:", round(test_dokladnosc, 4)))

# Procentowy wynik
wynik_procentowy <- round(test_dokladnosc * 100, 2)
message(paste(">>> Model działa poprawnie w", wynik_procentowy, "% na nowych danych. <<<"))




#---------------------------------------------------------------------------------------------
# --- ANALIZA STATYSTYCZNA NAJWAŻNIEJSZYCH ZMIENNYCH ---
#---------------------------------------------------------------------------------------------

# Upewnij się, że zmienna grupująca jest faktorem
model_data$wyczerpanie_studenta <- as.factor(model_data$wyczerpanie_studenta)

# Tworzymy pustą ramkę danych na wyniki
stat_results <- data.frame(
  Zmienna = character(),
  Typ_Testu = character(),
  P_value = numeric(),
  Istotne_statystycznie = character(),
  stringsAsFactors = FALSE
)

message("Rozpoczynam analizę statystyczną dla top 10 zmiennych...")

# Pętla po każdej zmiennej z listy najlepszych predyktorów
for (var_name in best_predictors) {
  
  # Pobieramy dane dla aktualnej zmiennej
  current_var_data <- model_data[[var_name]]
  
  # Sprawdzamy typ zmiennej i dobieramy test
  if (is.numeric(current_var_data)) {
    # --- TEST ANOVA dla zmiennych numerycznych ---
    test_type <- "ANOVA"
    
    # Formuła: Zmienna_numeryczna ~ Grupa_kategoryczna
    formula_aov <- as.formula(paste(var_name, "~ wyczerpanie_studenta"))
    
    # Wykonanie testu
    aov_result <- aov(formula_aov, data = model_data)
    
    # Wyciągnięcie p-value (jest w pierwszym wierszu, 5 kolumnie summary)
    p_val <- summary(aov_result)[[1]][["Pr(>F)"]][1]
    
  } else if (is.factor(current_var_data) || is.character(current_var_data)) {
    # --- TEST CHI-KWADRAT dla zmiennych kategorycznych ---
    test_type <- "Chi-kwadrat"
    
    # Tworzymy tabelę krzyżową
    contingency_table <- table(current_var_data, model_data$wyczerpanie_studenta)
    
    # Wykonanie testu (suppressWarnings na wypadek małych liczebności w komórkach)
    chisq_result <- suppressWarnings(chisq.test(contingency_table))
    
    # Wyciągnięcie p-value
    p_val <- chisq_result$p.value
    
  } else {
    # Na wypadek innego typu danych
    test_type <- "Nieznany typ"
    p_val <- NA
  }
  
  # Interpretacja istotności (przyjmujemy standardowy poziom alpha = 0.05)
  significance <- ifelse(!is.na(p_val) & p_val < 0.05, "TAK", "NIE")
  
  # Dodanie wyniku do tabeli zbiorczej
  stat_results[nrow(stat_results) + 1, ] <- list(
    var_name,
    test_type,
    p_val,
    significance
  )
}

# Formatowanie p-value, żeby było czytelne (np. < 0.0001 zamiast notacji naukowej e-16)
stat_results$P_value_formatted <- scales::pvalue(stat_results$P_value, accuracy = 0.0001)

# Wyświetlenie końcowej tabeli wyników
message("\n--- WYNIKI ANALIZY STATYSTYCZNEJ (ANOVA / Chi-kwadrat) ---")
print(stat_results[, c("Zmienna", "Typ_Testu", "P_value_formatted", "Istotne_statystycznie")])

message("\nInterpretacja: 'TAK' oznacza, że wartości tej zmiennej różnią się istotnie między grupami wyczerpania (p < 0.05).")
#--------------------------------------------------------------------------------------------------------------------------------------------
# WIZUALIZACJE
#--------------------------------------------------------------------------------------------------------------------------------------------

# --- 1. SETUP I PAKIETY ---
library(ggplot2)
library(dplyr)
# install.packages("ggstatsplot") # Jeśli nie masz
library(ggstatsplot)

# --- 2. PRZYGOTOWANIE DANYCH (TŁUMACZENIE NA ANGIELSKI) ---

# KROK KLUCZOWY: Zamiana poziomów zmiennej celu na angielski i ustalenie kolejności
# Zakładam, że Twoje dane to nadal 'model_data'
model_data_eng <- model_data

# Zamiana na angielski (Low, Moderate, High) i ustawienie jako ordered factor
model_data_eng$wyczerpanie_studenta <- factor(
  model_data_eng$wyczerpanie_studenta,
  # UWAGA: Upewnij się, że te polskie nazwy po lewej zgadzają się z Twoimi danymi!
  levels = c("niskie", "umiarkowane", "wysokie"),
  labels = c("Low", "Moderate", "High"),
  ordered = TRUE
)

# --- 3. SŁOWNIK TŁUMACZEŃ ZMIENNYCH ---
translation_dict <- c(
  "jak_bardzo_czujesz_sie_przytloczony_nadmiarem_obowiazkow" = "Feeling Overwhelmed by Duties",
  "czy_odczuwasz_przewlekly_stres" = "Chronic Stress Perception",
  "jak_czesto_czujesz_sie_zmeczony_a_fizycznie" = "Frequency of Physical Fatigue",
  "jak_czesto_czujesz_sie_emocjonalnie_wyczerpany" = "Frequency of Emotional Exhaustion",
  "jak_czesto_uwazasz_ze_kwestionujesz_swoje_decyzje" = "Frequency of Questioning Own Decisions",
  "czy_uwazasz_ze_masz_tendencje_do_przepracowywania_sie" = "Tendency to Overwork",
  "czy_czujesz_ze_masz_wsparcie_w_swoich_znajomych_ze_studiow" = "Support from Uni Friends",
  "jak_czesto_odkladasz_zadania_na_pozniej" = "Frequency of Procrastination",
  "czy_uwazasz_ze_praca_w_grupie_sprawia_ci_trudnosc" = "Difficulty Working in Groups",
  "czy_uwazasz_ze_masz_wsparcie_u_rodziny_i_lub_swoich_znajomych_spoza_studiow" = "Support from Family/Outside Friends"
)

# Lista zmiennych do pętli
vars_to_plot <- names(translation_dict)

# --- 4. DEFINICJA ZIELONEJ PALETY KOLORÓW ---
green_palette <- c("Low" = "#A1D99B",      # Jasna zieleń
                   "Moderate" = "#41AB5D", # Średnia zieleń
                   "High" = "#006837")     # Ciemna, intensywna zieleń


# --- 5. PĘTLA GENERUJĄCA WYKRESY (POPRAWIONA) ---

plot_list_eng <- list()
message("Generating English violin plots with ggstatsplot (Green theme)...")

for (var_pol_name in vars_to_plot) {
  
  # Pobranie angielskiego tytułu ze słownika
  english_title <- translation_dict[[var_pol_name]]
  
  # Generowanie wykresu
  p <- ggbetweenstats(
    data = model_data_eng,
    x = wyczerpanie_studenta,
    y = !!sym(var_pol_name),
    type = "parametric",
    plot.type = "violin",
    
    # --- Ustawienia Angielskie ---
    xlab = "Student Exhaustion Level",
    ylab = english_title,
    title = paste("Distribution of:", english_title),
    subtitle = "Comparison across exhaustion groups (ANOVA + Violin Plot)",
    
    # --- Estetyka ---
    ggtheme = ggplot2::theme_minimal(),
    results.subtitle = FALSE,
    pairwise.display = "significant",
    p.adjust.method = "bonferroni",
    
    # --- Kolory i kształty (POPRAWIONE) ---
    point.args = list(alpha = 0.2, size = 1.8, position = position_jitterdodge(dodge.width = 0.6)),
    violin.args = list(width = 0.5, alpha = 0.7),
    
    # --- TU BYŁ BŁĄD - POPRAWKA: ---
    # Zmieniamy outlier.shape = NA na outlier.color = "transparent"
    boxplot.args = list(width = 0.15, alpha = 0.8, outlier.color = "transparent")
    
  ) +
    # MANUALNE NADPISANIE KOLORÓW NA ZIELONE
    scale_color_manual(values = green_palette) +
    scale_fill_manual(values = green_palette) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      axis.title = element_text(face = "bold")
    )
  
  plot_list_eng[[english_title]] <- p
}

message("Gotowe! Wyświetlam zielone wykresy w oknie 'Plots'.")

# Wyświetlenie wykresów
for (plot_name in names(plot_list_eng)) {
  print(plot_list_eng[[plot_name]])
  # Sys.sleep(1) # Opcjonalne opóźnienie
}

# --- 5. PĘTLA GENERUJĄCA BOXPLOTY ---

plot_list_box <- list()
message("Generating English BOXPLOTS with ggstatsplot (Green theme)...")

for (var_pol_name in vars_to_plot) { 
  
  english_title <- translation_dict[[var_pol_name]]
  
  # Generowanie wykresu
  p <- ggbetweenstats(
    data = model_data_eng,
    x = wyczerpanie_studenta,
    y = !!sym(var_pol_name),
    type = "parametric",
    
    # --- ZMIANA KLUCZOWA: TYP WYKRESU NA "BOX" ---
    plot.type = "box",
    
    # --- Ustawienia Angielskie ---
    xlab = "Student Burnout Level",
    ylab = english_title,
    title = paste("Distribution of:", english_title),
    subtitle = "Comparison across burnout groups (ANOVA + Box Plot)",
    
    # --- Estetyka ---
    ggtheme = ggplot2::theme_minimal(),
    results.subtitle = TRUE,
    pairwise.display = "significant",
    p.adjust.method = "bonferroni",
    
    # --- Kolory i kształty dla BOXPLOTA ---
    # Punkty (jitter) trochę wyraźniejsze
    point.args = list(alpha = 0.4, size = 2, position = position_jitterdodge(dodge.width = 0.6)),
    # Ustawienia samego pudełka (trochę przezroczyste)
    # Ustawiamy outlier.color na transparent, bo i tak mamy punkty jitter
    boxplot.args = list(alpha = 0.7, width = 0.5, outlier.color = "transparent")
    
  ) +
    # MANUALNE NADPISANIE KOLORÓW NA ZIELONE
    scale_color_manual(values = green_palette) +
    scale_fill_manual(values = green_palette) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      axis.title = element_text(face = "bold")
    )
  
  plot_list_box[[english_title]] <- p
}

message("Gotowe! Wyświetlam zielone boxploty w oknie 'Plots'.")

# Wyświetlenie wykresów
for (plot_name in names(plot_list_box)) {
  print(plot_list_box[[plot_name]])
  # Sys.sleep(1) # Opcjonalne opóźnienie
}


library(dplyr)
library(tidyr)

# --- 2. PRZYGOTOWANIE DANYCH ---
# Zakładam, że masz w pamięci 'model_data' oraz listę 'best_predictors'
# i słownik 'translation_dict' z poprzednich kroków.

# a) Wybieramy tylko potrzebne kolumny
profile_data <- model_data %>%
  dplyr::select(wyczerpanie_studenta, all_of(best_predictors))

# b) Przygotowanie zmiennej grupującej (angielski + porządek)
profile_data$wyczerpanie_studenta <- factor(
  profile_data$wyczerpanie_studenta,
  levels = c("niskie", "umiarkowane", "wysokie"),
  labels = c("Low", "Moderate", "High"),
  ordered = TRUE
)

# c) STANDARYZACJA (Z-score) zmiennych numerycznych
# To kluczowe, żeby porównać zmienne o różnych skalach na jednej osi.
# Odejmujemy średnią i dzielimy przez odchylenie standardowe.
profile_data_scaled <- profile_data %>%
  mutate(across(where(is.numeric), scale))

# d) Obliczanie średnich dla każdej grupy
plot_data_summary <- profile_data_scaled %>%
  group_by(wyczerpanie_studenta) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  # e) Transformacja do formatu "długiego" (tidy data) dla ggplot2
  pivot_longer(
    cols = -wyczerpanie_studenta,
    names_to = "variable_polish",
    values_to = "mean_z_score"
  )

# f) Dodanie angielskich nazw ze słownika
plot_data_summary$variable_eng <- translation_dict[plot_data_summary$variable_polish]


# --- 3. DEFINICJA ZIELONEJ PALETY (spójna z poprzednimi) ---
green_palette <- c("Low" = "#A1D99B", "Moderate" = "#41AB5D", "High" = "#006837")

# --- 4. RYSOWANIE WYKRESU PROFILOWEGO ---
ggplot(plot_data_summary, aes(x = mean_z_score, y = reorder(variable_eng, mean_z_score), color = wyczerpanie_studenta)) +
  # Dodajemy linię pionową na zerze (średnia populacji)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Dodajemy linie łączące kropki dla jednej zmiennej (pokazują "rozstęp" między grupami)
  geom_line(aes(group = variable_eng), color = "gray80", size = 0.8) +
  # Dodajemy główne punkty (średnie)
  geom_point(size = 4) +
  
  # --- Estetyka i Opisy ---
  scale_color_manual(values = green_palette, name = "Exhaustion Level") +
  labs(
    title = "Multivariate Profile of Student Burnout Groups",
    subtitle = "Comparison of standardized mean scores across top predictors",
    x = "Standardized Mean Score (Z-score)\n(Negative = Below Average | 0 = Average | Positive = Above Average)",
    y = NULL # Usuwamy tytuł osi Y, bo etykiety wystarczą
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30"),
    axis.text.y = element_text(size = 10, color = "black"), # Wyraźne nazwy zmiennych
    legend.position = "top", # Legenda na górze
    panel.grid.major.y = element_line(color = "gray90") # Delikatne linie poziome
  )


#---------------------------
#Wykres przepływów alluvial - Płeć a Wyczerpanie Studenta
#---------------------------

# --- 1. PAKIETY ---
library(ggplot2)
library(dplyr)
# install.packages("ggalluvial") # Upewnij się, że masz ten pakiet
library(ggalluvial)

# --- 2. PRZYGOTOWANIE DANYCH ---
# Zakładam, że 'model_data' jest w pamięci.

alluvial_data_fixed <- model_data %>%
  # KROK 1: Rekonstrukcja jednej kolumny Płci z kolumn zero-jedynkowych
  mutate(
    Gender_Reconstructed = case_when(
      plec_kobieta == 1 ~ "Women",
      plec_mezczyzna == 1 ~ "Men",
      # Wszystko inne (czyli plec_inna) zamieniamy na NA (brak danych)
      TRUE ~ NA_character_
    )
  ) %>%
  # KROK 2: Filtrowanie - usuwamy przypadki 'inne' (czyli te, które stały się NA)
  filter(!is.na(Gender_Reconstructed)) %>%
  # KROK 3: Przygotowanie zmiennych do wykresu (tłumaczenie i porządek)
  mutate(
    # Uporządkowanie wyczerpania
    wyczerpanie_studenta = factor(
      wyczerpanie_studenta,
      levels = c("niskie", "umiarkowane", "wysokie"),
      labels = c("Low", "Moderate", "High"),
      ordered = TRUE
    ),
    # Ustawienie nowej kolumny płci jako faktora
    Gender = as.factor(Gender_Reconstructed)
  ) %>%
  # KROK 4: Agregacja danych (liczenie wystąpień) dla ggalluvial
  count(Gender, wyczerpanie_studenta)


# --- 3. DEFINICJA KOLORÓW (Zielona paleta) ---
green_palette_flow <- c("Low" = "#A1D99B",
                        "Moderate" = "#41AB5D",
                        "High" = "#006837")


# --- 4. RYSOWANIE WYKRESU ALLUVIAL ---
ggplot(alluvial_data_fixed,
       aes(axis1 = Gender,               # Oś 1: Zrekonstruowana Płeć
           axis2 = wyczerpanie_studenta, # Oś 2: Wyczerpanie
           y = n)) +                     # Grubość: Liczba osób
  
  # Przepływy
  geom_alluvium(aes(fill = wyczerpanie_studenta),
                width = 1/12, alpha = 0.7, color = "white", size = 0.5) +
  
  # Bloki kategorii
  geom_stratum(width = 1/6, fill = "gray90", color = "gray30") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4, fontface = "bold") +
  
  # Skale i estetyka
  scale_x_discrete(limits = c("Gender", "Burnout Level"), expand = c(.05, .05)) +
  scale_fill_manual(values = green_palette_flow, name = "Exhaustion Level") +
  labs(
    title = "Gender Differences in Student Burnout Flow",
    subtitle = "Flow of Men and Women towards different exhaustion levels",
    y = " "
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    axis.text.x = element_text(face = "bold", size = 12, vjust = -1),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )
#---------------------------
#Wykres skumulowany 100% - Płeć a Wyczerpan 
#---------------------------

library(scales) # Do ładnego formatowania procentów

# --- 2. PRZYGOTOWANIE DANYCH ---
# Zakładam, że 'model_data' jest w pamięci.

plot_data_gender_prop <- model_data %>%
  # Rekonstrukcja jednej kolumny Płci
  mutate(
    Gender_Reconstructed = case_when(
      plec_kobieta == 1 ~ "Women",
      plec_mezczyzna == 1 ~ "Men",
      TRUE ~ NA_character_
    )
  ) %>%
  # Filtrowanie (usuwamy 'inne')
  filter(!is.na(Gender_Reconstructed)) %>%
  # Porządkowanie zmiennych
  mutate(
    wyczerpanie_studenta = factor(
      wyczerpanie_studenta,
      levels = c("wysokie", "umiarkowane", "niskie"),
      labels = c("High", "Moderate", "Low"),
      ordered = TRUE
    ),
    Gender = as.factor(Gender_Reconstructed)
  )

# --- 3. DEFINICJA KOLORÓW ---
green_palette_reversed <- c("High" = "#006837",
                            "Moderate" = "#41AB5D",
                            "Low" = "#A1D99B")

# --- 4. RYSOWANIE WYKRESU SKUMULOWANEGO 100% (POPRAWIONY) ---

ggplot(plot_data_gender_prop, aes(x = Gender, fill = wyczerpanie_studenta)) +
  # POPRAWKA 1: Zmiana 'size' na 'linewidth' dla obramowania słupka
  geom_bar(position = "fill", width = 0.7, color = "white", linewidth = 0.5) +
  
  # Dodawanie etykiet procentowych (tu 'size' zostaje, bo to wielkość tekstu)
  geom_text(
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count), after_stat(x), sum)[as.character(after_stat(x))], accuracy = 0.1)),
    stat = "count",
    position = position_fill(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  # POPRAWKA 2a: Usunięcie 'guides(...)' ze środka scale_fill_manual
  scale_fill_manual(values = green_palette_reversed, name = "Exhaustion Level") +
  
  # POPRAWKA 2b: Dodanie 'guides()' jako osobnej warstwy
  guides(fill = guide_legend(reverse = TRUE)) +
  
  labs(
    title = "Proportion of Burnout Levels by Gender",
    subtitle = "Comparing the relative distribution of exhaustion within men and women populations",
    x = NULL,
    y = "Proportion (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    axis.text.x = element_text(face = "bold", size = 13),
    axis.text.y = element_text(color = "gray50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "top"
  )