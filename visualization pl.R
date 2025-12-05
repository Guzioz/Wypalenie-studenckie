#--------------------------------------------------------------------------------------------------------------------------------------------
# WIZUALIZACJE (Wersja Polska)
#--------------------------------------------------------------------------------------------------------------------------------------------

# --- 1. SETUP I PAKIETY ---
library(ggplot2)
library(dplyr)
# install.packages("ggstatsplot") # Jeśli nie masz
library(ggstatsplot)

# --- 2. PRZYGOTOWANIE DANYCH (Wersja Polska) ---

# Tworzymy kopię danych do wykresów
model_data_pl <- model_data

# Ustawienie poziomów wypalenia na polskie i w odpowiedniej kolejności
model_data_pl$wyczerpanie_studenta <- factor(
  model_data_pl$wyczerpanie_studenta,
  levels = c("niskie", "umiarkowane", "wysokie"), # Tak jak masz w danych
  labels = c("Niskie", "Umiarkowane", "Wysokie"), # Ładne etykiety na wykres
  ordered = TRUE
)

# --- 3. SŁOWNIK "ŁADNYCH NAZW" ZMIENNYCH ---
# Mapujemy nazwy kolumn na czytelne tytuły w języku polskim
translation_dict <- c(
  "jak_bardzo_czujesz_sie_przytloczony_nadmiarem_obowiazkow" = "Poczucie przytłoczenia obowiązkami",
  "czy_odczuwasz_przewlekly_stres" = "Odczuwanie przewlekłego stresu",
  "jak_czesto_czujesz_sie_zmeczony_a_fizycznie" = "Częstość zmęczenia fizycznego",
  "jak_czesto_czujesz_sie_emocjonalnie_wyczerpany" = "Częstość wyczerpania emocjonalnego",
  "jak_czesto_uwazasz_ze_kwestionujesz_swoje_decyzje" = "Częstość kwestionowania decyzji",
  "czy_uwazasz_ze_masz_tendencje_do_przepracowywania_sie" = "Tendencja do przepracowywania się",
  "czy_czujesz_ze_masz_wsparcie_w_swoich_znajomych_ze_studiow" = "Wsparcie znajomych ze studiów",
  "jak_czesto_odkladasz_zadania_na_pozniej" = "Tendencja do prokrastynacji",
  "czy_uwazasz_ze_praca_w_grupie_sprawia_ci_trudnosc" = "Trudność w pracy grupowej",
  "czy_uwazasz_ze_masz_wsparcie_u_rodziny_i_lub_swoich_znajomych_spoza_studiow" = "Wsparcie rodziny/znajomych spoza studiów"
)

# Lista zmiennych do pętli (klucze słownika)
vars_to_plot <- names(translation_dict)

# --- 4. DEFINICJA ZIELONEJ PALETY KOLORÓW ---
green_palette <- c("Niskie" = "#A1D99B",      # Jasna zieleń
                   "Umiarkowane" = "#41AB5D", # Średnia zieleń
                   "Wysokie" = "#006837")     # Ciemna, intensywna zieleń


# --- 5. PĘTLA GENERUJĄCA WYKRESY SKRZYPCOWE (VIOLIN PLOTS) ---

plot_list_violin <- list()
message("Generowanie polskich wykresów skrzypcowych (Violin plots)...")

for (var_code_name in vars_to_plot) {
  
  # Pobranie polskiego tytułu ze słownika
  polish_title <- translation_dict[[var_code_name]]
  
  # Generowanie wykresu
  p <- ggbetweenstats(
    data = model_data_pl,
    x = wyczerpanie_studenta,
    y = !!sym(var_code_name),
    type = "parametric",
    plot.type = "violin",
    
    # --- Ustawienia Polskie ---
    xlab = "Poziom wypalenia studenta",
    ylab = polish_title,
    title = paste("Rozkład zmiennej:", polish_title),
    subtitle = "Porównanie między grupami wypalenia (ANOVA + Wykres skrzypcowy)",
    
    # --- Estetyka ---
    ggtheme = ggplot2::theme_minimal(),
    results.subtitle = FALSE, # Ukrywamy statystyki w podtytule dla czystości (opcjonalnie TRUE)
    pairwise.display = "significant",
    p.adjust.method = "bonferroni",
    
    # --- Kolory i kształty ---
    point.args = list(alpha = 0.2, size = 1.8, position = position_jitterdodge(dodge.width = 0.6)),
    violin.args = list(width = 0.5, alpha = 0.7),
    boxplot.args = list(width = 0.15, alpha = 0.8, outlier.color = "transparent")
    
  ) +
    # MANUALNE NADPISANIE KOLORÓW NA ZIELONE
    scale_color_manual(values = green_palette) +
    scale_fill_manual(values = green_palette) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      axis.title = element_text(face = "bold")
    )
  
  plot_list_violin[[polish_title]] <- p
}

message("Gotowe! Wyświetlam wykresy skrzypcowe.")

# Wyświetlenie wykresów
for (plot_name in names(plot_list_violin)) {
  print(plot_list_violin[[plot_name]])
}

# --- 6. PĘTLA GENERUJĄCA WYKRESY PUDEŁKOWE (BOXPLOTS) ---

plot_list_box <- list()
message("Generowanie polskich wykresów pudełkowych (Boxplots)...")

for (var_code_name in vars_to_plot) { 
  
  polish_title <- translation_dict[[var_code_name]]
  
  # Generowanie wykresu
  p <- ggbetweenstats(
    data = model_data_pl,
    x = wyczerpanie_studenta,
    y = !!sym(var_code_name),
    type = "parametric",
    plot.type = "box",
    
    # --- Ustawienia Polskie ---
    xlab = "Poziom wypalenia studenta",
    ylab = polish_title,
    title = paste("Rozkład zmiennej:", polish_title),
    subtitle = "Porównanie między grupami wypalenia (ANOVA + Wykres pudełkowy)",
    
    # --- Estetyka ---
    ggtheme = ggplot2::theme_minimal(),
    results.subtitle = TRUE, # Tutaj pokazujemy wyniki testu statystycznego
    pairwise.display = "significant",
    p.adjust.method = "bonferroni",
    
    # --- Kolory i kształty ---
    point.args = list(alpha = 0.4, size = 2, position = position_jitterdodge(dodge.width = 0.6)),
    boxplot.args = list(alpha = 0.7, width = 0.5, outlier.color = "transparent")
    
  ) +
    scale_color_manual(values = green_palette) +
    scale_fill_manual(values = green_palette) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      axis.title = element_text(face = "bold")
    )
  
  plot_list_box[[polish_title]] <- p
}

message("Gotowe! Wyświetlam wykresy pudełkowe.")

for (plot_name in names(plot_list_box)) {
  print(plot_list_box[[plot_name]])
}


library(dplyr)
library(tidyr)

# --- PRZYGOTOWANIE DANYCH DO PROFILU ---
# Używamy model_data_pl (które już ma polskie poziomy: Niskie, Umiarkowane, Wysokie)

# a) Wybieramy tylko potrzebne kolumny
profile_data <- model_data_pl %>%
  dplyr::select(wyczerpanie_studenta, all_of(best_predictors))

# b) STANDARYZACJA (Z-score)
profile_data_scaled <- profile_data %>%
  mutate(across(where(is.numeric), scale))

# c) Obliczanie średnich dla każdej grupy i transformacja
plot_data_summary <- profile_data_scaled %>%
  group_by(wyczerpanie_studenta) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(
    cols = -wyczerpanie_studenta,
    names_to = "variable_code",
    values_to = "mean_z_score"
  )


#---------------------------
# Wykres przepływów alluvial - Płeć a Wyczerpanie Studenta (PL)
#---------------------------

library(ggplot2)
library(dplyr)
library(ggalluvial)

# --- PRZYGOTOWANIE DANYCH ---
alluvial_data_fixed <- model_data %>%
  # KROK 1: Rekonstrukcja Płci na PL
  mutate(
    Gender_Reconstructed = case_when(
      plec_kobieta == 1 ~ "Kobiety",
      plec_mezczyzna == 1 ~ "Mężczyźni",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Gender_Reconstructed)) %>%
  mutate(
    # Uporządkowanie wypalenia po Polsku
    wyczerpanie_studenta = factor(
      wyczerpanie_studenta,
      levels = c("niskie", "umiarkowane", "wysokie"),
      labels = c("Niskie", "Umiarkowane", "Wysokie"),
      ordered = TRUE
    ),
    Gender = as.factor(Gender_Reconstructed)
  ) %>%
  count(Gender, wyczerpanie_studenta)

# --- DEFINICJA KOLORÓW ---
green_palette_flow <- c("Niskie" = "#A1D99B",
                        "Umiarkowane" = "#41AB5D",
                        "Wysokie" = "#006837")

# --- RYSOWANIE WYKRESU ALLUVIAL ---
ggplot(alluvial_data_fixed,
       aes(axis1 = Gender,                # Oś 1
           axis2 = wyczerpanie_studenta,  # Oś 2
           y = n)) +                      
  
  geom_alluvium(aes(fill = wyczerpanie_studenta),
                width = 1/12, alpha = 0.7, color = "white", size = 0.5) +
  
  geom_stratum(width = 1/6, fill = "gray90", color = "gray30") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4, fontface = "bold") +
  
  scale_x_discrete(limits = c("Płeć", "Poziom wypalenia"), expand = c(.05, .05)) +
  scale_fill_manual(values = green_palette_flow, name = "Poziom wypalenia") +
  labs(
    title = "Przepływ poziomu wypalenia w zależności od płci",
    subtitle = "Wizualizacja relacji między płcią a poziomem wypalenia (Alluvial Plot)",
    y = "Liczba studentów"
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

# d) Dodanie polskich nazw ze słownika
plot_data_summary$variable_pl <- translation_dict[plot_data_summary$variable_code]

# --- RYSOWANIE WYKRESU PROFILOWEGO ---
ggplot(plot_data_summary, aes(x = mean_z_score, y = reorder(variable_pl, mean_z_score), color = wyczerpanie_studenta)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(aes(group = variable_pl), color = "gray80", size = 0.8) +
  geom_point(size = 4) +
  
  # --- Estetyka i Opisy PL ---
  scale_color_manual(values = green_palette, name = "Poziom wypalenia") +
  labs(
    title = "Profil czynników związanych z wypaleniem",
    subtitle = "Porównanie standaryzowanych średnich (Z-score) dla głównych predyktorów",
    x = "Standaryzowana średnia (Z-score)\n(Ujemne = Poniżej średniej | 0 = Średnia | Dodatnie = Powyżej średniej)",
    y = NULL 
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30"),
    axis.text.y = element_text(size = 10, color = "black"), 
    legend.position = "top", 
    panel.grid.major.y = element_line(color = "gray90") 
  )



#---------------------------
# Wykres skumulowany 100% - Płeć a Wyczerpanie (PL)
#---------------------------

library(scales)

# --- PRZYGOTOWANIE DANYCH ---
plot_data_gender_prop <- model_data %>%
  mutate(
    Gender_Reconstructed = case_when(
      plec_kobieta == 1 ~ "Kobiety",
      plec_mezczyzna == 1 ~ "Mężczyźni",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Gender_Reconstructed)) %>%
  mutate(
    # Odwracamy kolejność levels dla ładniejszego słupka (Wysokie na górze lub dole)
    wyczerpanie_studenta = factor(
      wyczerpanie_studenta,
      levels = c("wysokie", "umiarkowane", "niskie"),
      labels = c("Wysokie", "Umiarkowane", "Niskie"), 
      ordered = TRUE
    ),
    Gender = as.factor(Gender_Reconstructed)
  )

# --- DEFINICJA KOLORÓW (Odwrócona dla logiki wykresu) ---
green_palette_reversed <- c("Wysokie" = "#006837",
                            "Umiarkowane" = "#41AB5D",
                            "Niskie" = "#A1D99B")

# --- RYSOWANIE WYKRESU ---
ggplot(plot_data_gender_prop, aes(x = Gender, fill = wyczerpanie_studenta)) +
  geom_bar(position = "fill", width = 0.7, color = "white", linewidth = 0.5) +
  
  geom_text(
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count), after_stat(x), sum)[as.character(after_stat(x))], accuracy = 0.1)),
    stat = "count",
    position = position_fill(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = green_palette_reversed, name = "Poziom wypalenia") +
  
  # Odwracamy legendę, żeby pasowała do słupków
  guides(fill = guide_legend(reverse = TRUE)) +
  
  labs(
    title = "Proporcje poziomów wypalenia wg płci",
    subtitle = "Porównanie relatywnego rozkładu wyczerpania w populacji kobiet i mężczyzn",
    x = NULL,
    y = "Proporcja (%)"
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