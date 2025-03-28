# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
#
# DYNAMISCHE STANDARDLASTPROFILE                                            ----
#
# Author : Sascha Kornberger
# Datum  : 28.03.2025
# Version: 1.0.0
#
# History:
# 1.0.0  Funktion: Initiale Freigabe
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

# BENOETIGTE PAKETE
# Falls pacman noch nicht installiert ist, installieren
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", repos = "https://cran.r-project.org")
}
# Pakete laden oder installiert
pacman::p_load(
  shiny, dplyr, tidyr, lubridate, stringr, httr, jsonlite, DT
)

# --- UI ---
ui <- fluidPage(
  
  # Fortschrittsbalken-JavaScript
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateProgressBar', function(message) {
        $('#progress-bar').css('width', message.percent + '%');
        $('#progress-bar').text(message.text);
      });
    ")),
    # CSS für .well
    tags$style(HTML("
    .well {
      min-height: 800px;
      }
    "))
  ),
  
  # Titel
  div(
    style = "
      background-color: #007BFF; 
      color: white; 
      margin-top: 10px !important; 
      margin-bottom: 10px !important;
      padding-top: 5px !important;
      padding-bottom: 5px !important;
      text-align: center; 
      border-radius: 5px;",
    h3("DYNAMISCHE STANDARDLASTPROFILE", style = "margin: 5px 0; font-weight: bold !important;")
  ),
  
  # Sidebar und Main Panel
  sidebarLayout(
    sidebarPanel(
      # Eingabefelder
      selectInput("profilwahl", "Profil wählen:", 
                  choices = c(
                    "(H25) - Haushaltsprofil" = "H25",
                    "(G25) - Gewerbeprofil" = "G25",
                    "(L25) - Landwirtschaftsprofil" = "L25",
                    "(P25) - Kombinationsprofil PV" = "P25",
                    "(S25) - Kombinationsprofil PV-Speicher" = "S25"
                    ),
                  selected = "H25"
                  ),
      numericInput("jahr", "Jahr wählen:", value = 2026, min = 2025, max = 2100),
      selectInput("land", "Bundesland (für Feiertage):",
                  choices = c(
                    "Bundesweit anerkannte Feiertage" = "NATIONAL",
                    "Baden-Württemberg" = "BW",
                    "Bayern" = "BY",
                    "Berlin" = "BE",
                    "Brandenburg" = "BB",
                    "Bremen" = "BE",
                    "Hamburg" = "HH",
                    "Hessen" = "HE",
                    "Mecklenburg-Vorpommern" = "MV",
                    "Niedersachsen" = "NI",
                    "Nordrhein-Westfalen" = "NW",
                    "Rheinland-Pfalz" = "RP",
                    "Saarland" = "SL",
                    "Sachsen" = "SN",
                    "Sachsen-Anhalt" = "ST",
                    "Schleswig-Holstein" = "SH",
                    "Thüringen" = "TH"
                    ),
                  selected = "NATIONAL",
                  selectize = FALSE
                  ),
      # Button zum Starten
      div(style = "margin-top: 500px; text-align: center;",
          actionButton(
            "starte",
            "PROFIL ERSTELLEN",
            width = "100%",
            style = "background-color: #007BFF; color: white; 
               border: 2px solid black; border-radius: 8px; 
               padding: 10px 20px; font-size: 16px;"
          )
      )
    ),
    
    # Main Panel
    mainPanel(
      # Ausgabe der Feiertage
      uiOutput("feiertage_titel"),
      DT::dataTableOutput("feiertage_tabelle"),
      # Ausgabe des Status
      HTML("<div style='margin-top: 20px;'></div>"),
      verbatimTextOutput("status")
    )
  )
)

# Server Funktion
server <- function(input, output, session) {
  
  # Fortschrittsbalken 
  observeEvent(input$starte, {
    
    # Zeige Modal mit Platz für Fortschritt
    showModal(modalDialog(
      title = "Bitte warten",
      tags$div(id = "progress-text", "Erstelle Profil ..."),
      tags$div(id = "progress-bar-container",
               style = "width: 100%; background-color: #e9ecef; height: 25px; border-radius: 5px; margin-top: 10px;",
               tags$div(id = "progress-bar",
                        style = "width: 0%; height: 100%; background-color: #007bff; color: white; text-align: center; line-height: 25px; border-radius: 5px;",
                        "0%"
               )
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Funktion zum Aktualisieren des Fortschrittsbalkens
    updateProgress <- function(percent, text = "") {
      percent <- round(percent)
      session$sendCustomMessage("updateProgressBar", list(
        percent = percent,
        text = paste0(percent, "% – ", text)
      ))
    }
    
    # Statusausgabe
    output$status <- renderPrint({
      cat("Verarbeitung gestartet...")
    })
    
    # Feiertage und Profile laden
    shiny::isolate({
      updateProgress(5, "Lade Feiertage...")
      
      # Feiertage laden
      jahr <- input$jahr
      land <- input$land
      profilwahl <- input$profilwahl
      
      url <- paste0("https://feiertage-api.de/api/?jahr=", jahr, "&nur_land=", land)
      response <- GET(url)
      feiertage_data <- content(response, "text", encoding = "UTF-8")
      feiertage <- fromJSON(feiertage_data)
      feiertage_df <- tibble(
        Datum = as.Date(sapply(feiertage, function(x) x$datum)),
        Feiertagsname = names(feiertage)
      )
      
      # Ausgabe der Bundesländer im Titel
      output$feiertage_titel <- renderText({
        land_namen <- c(
          "NATIONAL" = "Deutschland bundesweit",
          "BW" = "Baden-Württemberg",
          "BY" = "Bayern",
          "BE" = "Berlin",
          "BB" = "Brandenburg",
          "HB" = "Bremen",
          "HH" = "Hamburg",
          "HE" = "Hessen",
          "MV" = "Mecklenburg-Vorpommern",
          "NI" = "Niedersachsen",
          "NW" = "Nordrhein-Westfalen",
          "RP" = "Rheinland-Pfalz",
          "SL" = "Saarland",
          "SN" = "Sachsen",
          "ST" = "Sachsen-Anhalt",
          "SH" = "Schleswig-Holstein",
          "TH" = "Thüringen"
        )
        HTML(paste("<h4><b>Feiertage", jahr, "für:", land_namen[input$land], "</b></h4>"))
      })
      
      # Ausgabe der Feiertage als Tabelle 
      output$feiertage_tabelle <- DT::renderDataTable({
        DT::datatable(
          feiertage_df,
          options = list(
            pageLength = 20,         # Anzahl der Zeilen pro Seite
            paging = FALSE,          # Seitenzahlen ausblenden
            ordering = FALSE,        # Sortierung deaktivieren
            searching = FALSE,       # Suchleiste ausblenden
            info = FALSE,            # Info-Text ausblenden
            lengthChange = FALSE     # Dropdown für Zeilenanzahl ausblenden
          ),
          rownames = FALSE      # kein Index
        )
      })
      
      # Statusausgabe
      updateProgress(20, "Lade Profil...")
      load("/media/archive/RStudio/EEG/Dynamische_Profile/SLP_Profiles.RData")
      profil <- get(profilwahl)
      viertelstunden <- profil$Uhrzeit |> as.character() |> str_trim()
      
      # Deutsche Monatsnamen erzeugen
      Sys.setlocale("LC_TIME", "de_DE.UTF-8")
      monate <- format(ISOdate(2000, 1:12, 1), "%B")
      
      # Statusausgabe
      updateProgress(30, "Berechne Kalendertage...")
      tage <- tibble(
        Datum = seq(as.Date(paste0(jahr, "-01-01")),
                    as.Date(paste0(jahr, "-12-31")), by = "day")
      ) |> mutate(
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
      
      # Statusausgabe
      updateProgress(50, "Erzeuge Kombinationen...")
      tabelle <- expand.grid(Datum = tage$Datum, Uhrzeit = viertelstunden) |>
        as_tibble() |>
        mutate(Uhrzeit = as.character(Uhrzeit)) |>
        left_join(tage |> select(Datum, Profil_Spalte, t), by = "Datum")
      
      # Statusausgabe
      updateProgress(70, "Berechne Werte...")
      # Dynamisierungsfaktor
      tabelle <- tabelle |>
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
          Faktor = (-3.92e-10 * t^4) + (3.2e-7 * t^3) -
            (7.02e-5 * t^2) + (2.1e-3 * t) + 1.24,
          x_dyn = round(x0 * Faktor, 4)
        ) |>
        ungroup() |>
        arrange(Datum, Uhrzeit) |>
        rename(Tag = t) 
      
      # ohne x_dyn für G25 und L25
      if (profilwahl == "G25" || profilwahl == "L25") {
        tabelle <- tabelle |>
          select(Datum, Uhrzeit, Profil_Spalte, Tag, x0)
      } else {
        tabelle <- tabelle |>
          select(Datum, Uhrzeit, Profil_Spalte, Tag, x0, x_dyn)
      }
      
      # Statusausgabe
      updateProgress(90, "Schreibe CSV-Datei...")
      # Pfad für CSV-Datei
      pfad <- file.path("/media/archive/RStudio/EEG/Dynamische_Profile/out", paste0(profilwahl, "_", jahr, "_", land, ".csv"))
      # Export als CSV
      write.csv2(tabelle, file = pfad, row.names = FALSE)
      
      updateProgress(100, "Fertig!")
      Sys.sleep(0.5)
      removeModal()
      
      output$status <- renderPrint({
        cat("Profil gespeichert unter:", pfad)
      })
    })
  })
}

# --- App starten ---
shinyApp(ui, server)
