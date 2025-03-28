# Profile ohne Dynamisierung G25 und L25

# Wenn gesetzt, dann werden nur Feiertage des gewählten Bundeslandes angezeigt. 
# Mögliche Werte:
# NATIONAL, BW, BY, BE, BB, HB, HH, HE, MV, NI, NW, RP, SL, SN, ST, SH, TH
# https://feiertage-api.de/api/?jahr=2016&nur_land=NW

# --- Pakete laden ---
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(httr)
library(jsonlite)

# --- Pfad zur Excel-Datei ---
pfad <- "/media/archive/RStudio/EEG/Dynamische_Profile/H25_Profil.xlsx"
# --- Jahr global festlegen ---
jahr <- 2026
land <- "NATIONAL"

# --- Feiertage für Deutschland über API laden ---
feiertage_url <- paste0("https://feiertage-api.de/api/?jahr=", jahr, "&nur_land=", land)
response <- GET(feiertage_url)
feiertage_data <- content(response, "text", encoding = "UTF-8")
feiertage <- fromJSON(feiertage_data)

# Feiertage aus der API extrahieren
feiertage_df <- tibble(
  Datum = as.Date(sapply(feiertage, function(x) x$datum)),
  Feiertagsname = names(feiertage)
)


# --- Sprache auf Deutsch setzen ---
Sys.setlocale("LC_TIME", "de_DE.UTF-8")  # oder "German"

# --- Deutsche Monatsnamen erzeugen ---
monate <- format(ISOdate(2000, 1:12, 1), "%B")

# --- Datei einlesen ---
raw <- read_excel(pfad, skip = 2, col_names = FALSE)

# --- Spaltennamen definieren ---
zeit <- raw[[1]]
typen <- c("SA", "FT", "WT")
spalten_namen <- expand.grid(Monat = monate, Typ = typen) |>
  arrange(Monat) |>
  mutate(Name = paste0(Monat, "_", Typ)) |>
  pull(Name)
colnames(raw) <- c("Uhrzeit", spalten_namen)
profil <- raw

# --- Profil in long-Format bringen ---
profil_long <- profil |>
  mutate(Uhrzeit = str_trim(as.character(Uhrzeit))) |>
  pivot_longer(
    cols = -Uhrzeit,
    names_to = "Profil_Spalte",
    values_to = "x0"
  )

# --- Alle Tage im Jahr erzeugen ---
tage_2026 <- tibble(
  Datum = seq(as.Date(paste0(jahr, "-01-01")), as.Date(paste0(jahr, "-12-31")), by = "day")
) |>
  mutate(
    Monat = month(Datum, label = TRUE, abbr = FALSE),
    Wochentag = wday(Datum, label = TRUE, abbr = FALSE),
    Feiertag = Datum %in% feiertage_df$Datum,
    Typ = case_when(
      Feiertag ~ "FT",
      Wochentag == "Samstag" ~ "SA",
      Wochentag == "Sonntag" ~ "FT",
      TRUE ~ "WT"
    ),
    t = yday(Datum),
    Profil_Spalte = paste0(Monat, "_", Typ)
  )

# --- Viertelstundenliste erzeugen ---
viertelstunden <- profil$Uhrzeit |> as.character() |> str_trim()

# --- Datum × Uhrzeit kombinieren und dynamisieren ---
tabelle <- expand.grid(
  Datum = tage_2026$Datum,
  Uhrzeit = viertelstunden
) |>
  as_tibble() |>
  mutate(Uhrzeit = as.character(Uhrzeit)) |>
  left_join(tage_2026 |> select(Datum, Profil_Spalte, t), by = "Datum") |>
  rowwise() |>
  mutate(
    x0 = {
      spalte <- Profil_Spalte
      zeile <- which(str_trim(as.character(profil$Uhrzeit)) == Uhrzeit)
      if (length(zeile) == 1 && spalte %in% names(profil)) {
        as.numeric(str_replace(profil[[spalte]][zeile], ",", "."))
      } else {
        NA_real_
      }
    },
    Faktor = (-3.92e-10 * t^4) +
      (3.2e-7   * t^3) -
      (7.02e-5  * t^2) +
      (2.1e-3   * t) + 1.24,
    x_dyn = round(x0 * Faktor, 5)
  ) |>
  ungroup() |>
  arrange(Datum, Uhrzeit) |>
  rename(Tag = t) |>
  select(Datum, Uhrzeit, Profil_Spalte, Tag, x0, x_dyn)

# --- Export als CSV ---
write.csv2(tabelle,
           file = "/media/archive/RStudio/EEG/Dynamische_Profile/H25_Profil_dynamisiert_SH.csv",
           row.names = FALSE)

# --- Export als CSV ---
write.csv2(feiertage_df,,
           file = "/media/archive/RStudio/EEG/Dynamische_Profile/Feiertage_2026_NATIONAL.csv",
           row.names = FALSE)
