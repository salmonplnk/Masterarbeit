# Bibliotheken
library(dplyr)
library(ggplot2)
library(readr)
library(randomForest)
library(stringr)

rm(list = ls())
# Daten einlesen
data <- read_csv("C:/Users/vince/OneDrive/Desktop/Masterarbeit 2024/Mortality_rates_raw.csv",
                 skip = 4,
                 col_names = c('Geschlecht', 'Todesursache', 'Jahr', 'Monat', 'Altersgruppe', 'Impfstatus',
                               'Anzahl_Todesfaelle', 'Personenjahre', 'Mortalitaetsrate', 
                               'Unzuverlaessigkeits_Flag', 'Unteres_Konfidenzintervall', 'Oberes_Konfidenzintervall'),
                 col_types = cols(.default = col_character(), Jahr = col_double()))

# Fälle "<3" als NA setzen und numerisch konvertieren
data$Anzahl_Todesfaelle <- ifelse(data$Anzahl_Todesfaelle == "<3", NA, data$Anzahl_Todesfaelle)
data$Anzahl_Todesfaelle <- as.numeric(data$Anzahl_Todesfaelle)
data$Personenjahre <- as.numeric(gsub("[^0-9.]", "", data$Personenjahre))

# Nur valide Einträge behalten
data <- data %>% filter(!is.na(Personenjahre) & Personenjahre > 0)

# Dummy Impfstatus
data$Impfstatus_Dummy <- factor(ifelse(grepl("Unvaccinated", data$Impfstatus), 0, 1))

# Quartale & chronologische Ordnung erstellen
data <- data %>%
  mutate(Quartal_Jahr = factor(case_when(
    Monat %in% c("January", "February", "March") ~ paste0("Q1_", Jahr),
    Monat %in% c("April", "May", "June") ~ paste0("Q2_", Jahr),
    Monat %in% c("July", "August", "September") ~ paste0("Q3_", Jahr),
    TRUE ~ paste0("Q4_", Jahr)
  )))

quartal_levels <- data %>%
  mutate(Datum = as.Date(paste(Jahr, Monat, "01"), "%Y %B %d")) %>%
  arrange(Datum) %>%
  distinct(Quartal_Jahr)

data$Quartal_Jahr <- factor(data$Quartal_Jahr, levels = quartal_levels$Quartal_Jahr)

# Random-Forest-Training mit Fällen zwischen 3 und 9
rf_train <- data %>% filter(!is.na(Anzahl_Todesfaelle), Anzahl_Todesfaelle >= 3, Anzahl_Todesfaelle <= 9)

# Random-Forest-Modell trainieren (Regression)
rf_model <- randomForest(
  Anzahl_Todesfaelle ~ Altersgruppe + Geschlecht + Impfstatus + Personenjahre,
  data = rf_train,
  ntree = 500
)

# Fälle mit "<3" (NA) vorhersagen
missing_data <- data %>% filter(is.na(Anzahl_Todesfaelle))
predicted_values <- predict(rf_model, newdata = missing_data)

# Vorhersage gerundet auf eine Dezimalstelle ersetzen
data$Anzahl_Todesfaelle[is.na(data$Anzahl_Todesfaelle)] <- round(predicted_values, 1)

# Poisson-Regressionsanalyse nach Altersgruppe & Quartal
run_poisson <- function(df, gruppe) {
  if (length(unique(df$Impfstatus_Dummy)) < 2) {
    return(NULL)
  }
  
  glm(Anzahl_Todesfaelle ~ Impfstatus_Dummy, 
      family = poisson, offset = log(Personenjahre), data = df)
}

# Ergebnisse speichern
ergebnisse <- list()
for (altersgruppe in unique(data$Altersgruppe)) {
  for (quartal in levels(data$Quartal_Jahr)) {
    subset_df <- data %>% filter(Altersgruppe == altersgruppe, Quartal_Jahr == quartal)
    if (nrow(subset_df) > 1) {
      modell <- run_poisson(subset_df, paste(altersgruppe, quartal))
      if (!is.null(modell)) {
        ergebnisse[[paste(altersgruppe, quartal)]] <- modell
      }
    }
  }
}

# Modell-Summaries als HTML speichern
sink("model_summaries.html")
cat("<html><body>\n")
for (name in names(ergebnisse)) {
  cat(paste0("<h2>", name, "</h2><pre>", capture.output(summary(ergebnisse[[name]])), "</pre><hr>"))
}
cat("</body></html>\n")
sink()

# Grafiken pro Altersgruppe erstellen
plot_df <- data %>%
  group_by(Altersgruppe, Quartal_Jahr) %>%
  summarise(Todesfaelle = sum(Anzahl_Todesfaelle)) %>%
  ungroup()

for (gruppe in unique(plot_df$Altersgruppe)) {
  temp_plot <- plot_df %>% filter(Altersgruppe == gruppe)
  
  ggplot(temp_plot, aes(x = Quartal_Jahr, y = Todesfaelle)) +
    geom_col(fill = "steelblue") +
    labs(title = paste("Todesfälle Altersgruppe", gruppe),
         x = "Quartal", y = "Anzahl Todesfälle") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(paste0("Todesfaelle_", gruppe, ".png"), width = 10, height = 6, bg = "white")
}

# Gesamteffekt der Impfung (Kontrafaktische Analyse)
gesamtmodell <- glm(Anzahl_Todesfaelle ~ Impfstatus_Dummy, 
                    family = poisson, offset = log(Personenjahre), data = data)

geimpft_df <- data %>% filter(Impfstatus_Dummy == 1)
geimpft_df$Impfstatus_Dummy <- 0
geimpft_df$vorhergesagt <- predict(gesamtmodell, newdata = geimpft_df, type = "response")

verhinderte_faelle <- sum(geimpft_df$vorhergesagt - geimpft_df$Anzahl_Todesfaelle)

cat("Geschätzte Gesamtzahl der durch Impfung verhinderten Todesfälle:",
    round(verhinderte_faelle, 0), "\n")
