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

# --- Wykresy ---
# Histogram
ggplot(ankieta, aes(x = wypalenie_studenckie)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram wypalenia studenckiego", x = "Wypalenie studenckie", y = "Liczba studentów") +
  theme_minimal()
# Wykres pudełkowy
ggplot(ankieta, aes(x = Płeć.wartość, y = wypalenie_studenckie)) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Wykres pudełkowy wypalenia studenckiego w zależności od płci", 
       x = "Płeć", y = "Wypalenie studenckie") +
  theme_minimal()

# --- Statystyki opisowe ---
# Średnia i odchylenie standardowe
srednia_wypalenie <- mean(ankieta$wypalenie_studenckie, na.rm = TRUE)
odchylenie_wypalenie <- sd(ankieta$wypalenie_studenckie, na.rm = TRUE)
srednia_wypalenie
odchylenie_wypalenie
#anova
anova_model <- aov(wypalenie_studenckie ~ Płeć.wartość, data = ankieta)
anova_table <- as.data.frame(summary(anova_model)[[1]])
rownames(anova_table)[1] <- "Płeć"
colnames(anova_table) <- c("Źródło", "Sumy kw.", "Stopnie sw.", "F", "p-wartość")

kable(anova_table, caption = "Tabela ANOVA: ") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "bordered", "responsive"), 
                full_width = FALSE, font_size = 14)




#-----Nowe kody, czy coś chcemy zostawić czy jak robimy-----
# Lista zmiennych jakościowych – już po make.names()
zmienne_jakosciowe <- c("Płeć.wartość",
                        "Na.jakiej.uczelni.studiujesz..wartość",
                        "Rodzaj.studiów.wartość",
                        "Jaki.kierunek.studiujesz..wartość",
                        "Czy.pracujesz..wartość",
                        "Miejsce.zamieszkania..wartość",
                        "Czy.jesteś.singlem.singielką..wartość",
                        "Na.którym.roku.studiów.jesteś..wartość")

# Test ANOVA dla każdej zmiennej jakościowej
for (zmienna in zmienne_jakosciowe) {
  cat("\n🔎 Zmienna:", zmienna, "\n")
  form <- as.formula(paste("wypalenie_studenckie ~", zmienna))
  print(summary(aov(form, data = data)))
}

# Korelacja Pearsona: wiek a wypalenie
# Zamień "35 i więcej" na 35 w kolumnie wieku
data$Ile.masz.lat..wartość <- gsub("35 i więcej", "35", data$Ile.masz.lat..wartość)
data$wiek_numeric <- as.numeric(data$Ile.masz.lat..wartość)
cor.test(data$wiek_numeric, data$wypalenie_studenckie, use = "complete.obs", method = "pearson")


# Wybieramy kolumny 21 do 38 oraz kolumnę wypalenie_studenckie
sub_data <- data[, c("wypalenie_studenckie", colnames(data)[21:38])]

# Konwertujemy wszystkie kolumny na numeryczne (na wypadek, gdyby były faktorami lub tekstem)
sub_data <- data.frame(lapply(sub_data, function(x) as.numeric(as.character(x))))

# Sprawdzamy korelację między wypalenie_studenckie a każdą z kolumn 21:38
korelacje <- sapply(sub_data[, -1], function(x) cor(sub_data$wypalenie_studenckie, x, use = "complete.obs"))

# Tworzymy czytelną ramkę danych z wynikami
wynik_df <- data.frame(
  zmienna = colnames(sub_data)[-1],
  korelacja_z_wypaleniem = korelacje
)

# Wyświetlenie wyników
print(wynik_df)
library(ggplot2)
library(reshape2)

# Wybierz dane
sub_data <- data[, c("wypalenie_studenckie", colnames(data)[21:38])]

# Konwertuj wszystko na liczby
sub_data <- data.frame(lapply(sub_data, function(x) as.numeric(as.character(x))))

# Oblicz korelacje
korelacje <- sapply(sub_data[, -1], function(x) cor(sub_data$wypalenie_studenckie, x, use = "complete.obs"))

# Stwórz ramkę do wykresu
df_korelacje <- data.frame(
  zmienna = names(korelacje),
  korelacja = korelacje
)

# Wykres heatmapy
ggplot(df_korelacje, aes(x = "", y = zmienna, fill = korelacja)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "Korelacja") +
  geom_text(aes(label = round(korelacja, 2)), color = "black") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Korelacje z wypaleniem studenckim")

max(data$wypalenie_studenckie, na.rm = TRUE)
min(data$wypalenie_studenckie, na.rm = TRUE)
library(dplyr)

data <- data %>%
  mutate(
    ryzyko_wypalenia = case_when(
      wypalenie_studenckie >= 61 ~ "wysokie",
      wypalenie_studenckie >= 47 ~ "średnie",
      wypalenie_studenckie >= 0  ~ "niskie",
      TRUE ~ NA_character_  # na wypadek braków danych
    )
  )
library(ggplot2)

ggplot(data, aes(x = ryzyko_wypalenia,
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