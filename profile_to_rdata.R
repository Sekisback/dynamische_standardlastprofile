# slp_converter.R
# Konvertiert mehrere SLP-Excel-Dateien in ein gemeinsames .RData

library(readxl)
library(dplyr)
library(stringr)

# --- Pfade zu den Profil-Dateien ---
profil_pfade <- list(
  G25 = "/media/archive/RStudio/EEG/Dynamische_Profile/in/G25_Profil.xlsx", # Beispiel
  H25 = "/media/archive/RStudio/EEG/Dynamische_Profile/in/H25_Profil.xlsx", # Beispiel
  L25 = "/media/archive/RStudio/EEG/Dynamische_Profile/in/L25_Profil.xlsx", # Beispiel
  P25 = "/media/archive/RStudio/EEG/Dynamische_Profile/in/P25_Profil.xlsx", # Beispiel
  S25 = "/media/archive/RStudio/EEG/Dynamische_Profile/in/S25_Profil.xlsx" # Beispiel
)

# --- Deutsche Monatsnamen erzeugen ---
monate <- format(ISOdate(2000, 1:12, 1), "%B")
typen <- c("SA", "FT", "WT")
spalten_namen <- expand.grid(Monat = monate, Typ = typen) |>
  arrange(Monat) |>
  mutate(Name = paste0(Monat, "_", Typ)) |>
  pull(Name)

# --- Hilfsfunktion: Profil einlesen ---
einlesen_profil <- function(pfad) {
  raw <- read_excel(pfad, skip = 2, col_names = FALSE)
  colnames(raw) <- c("Uhrzeit", spalten_namen)
  raw |>
    mutate(across(-Uhrzeit, ~as.numeric(str_replace(., ",", ".")))) |>
    mutate(Uhrzeit = str_trim(as.character(Uhrzeit)))
}

# --- Alle Profile einlesen ---
profile <- lapply(profil_pfade, einlesen_profil)

# --- RData speichern ---
speicher_pfad <- file.path("/media/archive/RStudio/EEG/Dynamische_Profile/SLP_Profiles.RData")
save(list = names(profile), file = speicher_pfad, envir = list2env(profile))

cat("Profile wurden erfolgreich gespeichert unter:\n", speicher_pfad, "\n")
