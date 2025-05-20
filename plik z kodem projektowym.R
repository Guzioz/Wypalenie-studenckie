# --- Setup ---
knitr::opts_chunk$set(echo = TRUE, fig.show="hold", fig.width=7, fig.height=5, dev="png")

# Wczytanie danych(co jest wyczerpaniem, co bierzemy pod uwagę, które to czynniki)
library(readxl)
data <- read_excel("Wypalenie-wsrod-osob-studiujacych-2025-05-09.xlsx")
data <- data.frame(data)

# --- Pakiety ---
required_packages <- c("dplyr", "ggplot2", "finalfit", "VIM", "validate", 
                       "errorlocate", "tidyverse", "ggcorrplot", "forcats",
                       "ggthemes", "dlookr", "editrules", "hrbrthemes", "plotly",
                       "ISLR", "gapminder", "kableExtra", "ggstatsplot", "gtsummary",
                       "readr", "rmarkdown", "moments", "knitr")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
sapply(required_packages, install_if_missing)

# Załadowanie pakietów
lapply(required_packages, library, character.only = TRUE)

# --- Walidacja danych ---
RULE <- validator(
  Hours_Studied >= 0, Hours_Studied <= 30,
  Attendance >= 0, Attendance <= 100,
  Parental_Involvement %in% c("Low", "Medium", "High"),
  Access_to_Resources %in% c("Low", "Medium", "High"),
  Extracurricular_Activities %in% c("No", "Yes"),
  Sleep_Hours >= 0,
  Previous_Scores >= 0, Previous_Scores <= 100,
  Motivation_Level %in% c("Low", "Medium", "High"),
  Internet_Access %in% c("No", "Yes"),
  Tutoring_Sessions >= 0,
  Family_Income %in% c("Low", "Medium", "High"),
  Teacher_Quality %in% c("Low", "Medium", "High"),
  School_Type %in% c("Public", "Private"),
  Peer_Influence %in% c("Negative", "Neutral", "Positive"),
  Physical_Activity >= 0, Physical_Activity <= 25,
  Learning_Disabilities %in% c("No", "Yes"),
  Parental_Education_Level %in% c("High School", "College", "Postgraduate"),
  Distance_from_Home %in% c("Near", "Moderate", "Far"),
  Gender %in% c("Male", "Female"),
  Exam_Score >= 0, Exam_Score <= 100
)

# --- Obsługa błędnych danych i imputacja (jak ktoś nie jest ze studiów i puste kolumny)---
out <- confront(data, RULE)
data <- data %>% mutate_if(is.character, as.factor)
data_no_error <- replace_errors(data, RULE)
czyste_dane <- hotdeck(data_no_error)

# --- Missing data plot after cleaning ---
czyste_dane %>%
  dplyr::select(Hours_Studied, Attendance, Parental_Involvement, Access_to_Resources, 
                Extracurricular_Activities, Sleep_Hours, Previous_Scores, 
                Motivation_Level, Internet_Access, Tutoring_Sessions, Family_Income, 
                Teacher_Quality, School_Type, Peer_Influence, Physical_Activity, 
                Learning_Disabilities, Parental_Education_Level, Distance_from_Home, 
                Gender) %>%
  finalfit::missing_plot()

# --- Korelacja Spearmana ---
dane_temp <- czyste_dane
num_vars <- names(dane_temp)[sapply(dane_temp, is.numeric)]
cat_vars <- names(dane_temp)[sapply(dane_temp, is.character) | sapply(dane_temp, is.factor)]
dane_temp[cat_vars] <- lapply(dane_temp[cat_vars], function(x) as.numeric(as.factor(x)))
all_numeric_vars <- c(num_vars, cat_vars)

cor_results <- sapply(setdiff(all_numeric_vars, "Exam_Score"), function(var) {
  cor.test(dane_temp[[var]], dane_temp$Exam_Score, method = "spearman")$estimate
})
cor_results_sorted <- sort(abs(cor_results), decreasing = TRUE)

cor_results_df <- data.frame(
  Variable = names(cor_results_sorted),
  Spearman_Rho = cor_results_sorted
)

kable(cor_results_df, caption = "Wartości korelacji Spearmana względem wyniku egzaminu") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# --- Podziały na grupy ---
czyste_dane <- czyste_dane %>%
  mutate(
    grupa1 = case_when(
      Hours_Studied < 16 ~ "poniżej 16",
      Hours_Studied <= 23 ~ "16-23",
      Hours_Studied > 23 ~ "powyżej 23"
    ),
    grupa2 = case_when(
      Exam_Score < 60 ~ "2.0",
      Exam_Score <= 65 ~ "3.0",
      Exam_Score <= 70 ~ "3.5",
      Exam_Score <= 80 ~ "4.0",
      Exam_Score <= 90 ~ "4.5",
      Exam_Score > 90 ~ "5.0",
      TRUE ~ "Brak danych"
    ),
    grupa3 = case_when(
      Previous_Scores < 60 ~ "2.0",
      Previous_Scores <= 65 ~ "3.0",
      Previous_Scores <= 70 ~ "3.5",
      Previous_Scores <= 80 ~ "4.0",
      Previous_Scores <= 90 ~ "4.5",
      Previous_Scores > 90 ~ "5.0",
      TRUE ~ "Brak danych"
    )
  ) %>%
  mutate(across(c(grupa1, grupa2, grupa3), as.factor))

# --- Histogram ---
ggplot(czyste_dane, aes(x = Exam_Score)) +
  geom_histogram(bins = 20, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Rozkład wyników egzaminacyjnych", x = "Wynik egzaminu", y = "Liczba uczniów") +
  theme_minimal()

# --- Statystyki opisowe ---
exam_score_stats <- summary(czyste_dane$Exam_Score)
std_dev <- sd(czyste_dane$Exam_Score)
skewness <- skewness(czyste_dane$Exam_Score)
kurtosis <- kurtosis(czyste_dane$Exam_Score)

stats_table <- data.frame(
  Statystyka = c("Min", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max", 
                 "Standard Deviation", "Skewness", "Kurtosis"),
  Wartość = c(exam_score_stats[1:6], std_dev, skewness, kurtosis)
)

kable(stats_table, caption = "Statystyki opisowe dla wyników egzaminacyjnych", digits = 1, align = "c")

# --- Model regresji liniowej ---
model <- lm(Exam_Score ~ Hours_Studied + Attendance + Parental_Involvement + 
              Access_to_Resources + Extracurricular_Activities + Sleep_Hours + 
              Previous_Scores + Motivation_Level + Internet_Access + Tutoring_Sessions + 
              Family_Income + Teacher_Quality + School_Type + Peer_Influence + 
              Physical_Activity + Learning_Disabilities + Parental_Education_Level + 
              Distance_from_Home + Gender, data = czyste_dane)

summary_model <- summary(model)
coefficients <- summary_model$coefficients
sorted_coefficients <- coefficients[order(abs(coefficients[, "t value"]), decreasing = TRUE), ]
top_10_df <- as.data.frame(head(sorted_coefficients, 10))
top_10_df$Variable <- rownames(top_10_df)

kable(top_10_df, caption = "10 najważniejszych zmiennych w modelu regresji") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# --- Wymuszenie poziomów dla zmiennych porządkowych ---
czyste_dane$Access_to_Resources <- factor(czyste_dane$Access_to_Resources, levels = c("Low", "Medium", "High"))
czyste_dane$Parental_Involvement <- factor(czyste_dane$Parental_Involvement, levels = c("Low", "Medium", "High"))
czyste_dane$Family_Income <- factor(czyste_dane$Family_Income, levels = c("Low", "Medium", "High"))

# --- Boxplot: godziny nauki vs wynik ---
ggplot(czyste_dane, aes(x = grupa1, y = Exam_Score, fill = grupa1)) +
  geom_boxplot() +
  labs(title = "Wpływ liczby godzin nauki na wynik egzaminu", x = "Liczba godzin nauki", y = "Wynik egzaminu") +
  theme_minimal()

# --- Scatterplot: godziny nauki vs wynik ---
ggplot(czyste_dane, aes(x = Hours_Studied, y = Exam_Score)) +
  geom_point(alpha = 0.6, color = "orange") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Wpływ godzin nauki na wynik egzaminu", x = "Godziny nauki", y = "Wynik egzaminu") +
  theme_minimal()

# --- Statystyki opisowe + ANOVA ---
statystyki_opisowe <- czyste_dane %>%
  group_by(grupa1) %>%
  summarise(
    Średnia = mean(Exam_Score, na.rm = TRUE),
    Q1 = quantile(Exam_Score, 0.25, na.rm = TRUE),
    Mediana = median(Exam_Score, na.rm = TRUE),
    Q3 = quantile(Exam_Score, 0.75, na.rm = TRUE),
    Odchylenie_std = sd(Exam_Score, na.rm = TRUE),
    Min = min(Exam_Score, na.rm = TRUE),
    Max = max(Exam_Score, na.rm = TRUE),
    N = n()
  )

anova_model <- aov(Exam_Score ~ grupa1, data = czyste_dane)
anova_table <- as.data.frame(summary(anova_model)[[1]])
rownames(anova_table)[1] <- "Godziny nauki"
colnames(anova_table) <- c("Źródło", "Sumy kw.", "Stopnie sw.", "F", "p-wartość")

kable(statystyki_opisowe, caption = "Tabela statystyk opisowych dla grup godzin nauki") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "bordered", "responsive"), 
                full_width = FALSE, font_size = 14)

kable(anova_table, caption = "Tabela ANOVA: Wpływ godzin nauki na wynik egzaminu") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "bordered", "responsive"), 
                full_width = FALSE, font_size = 14)

                                                                        