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

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
#                                      UI                                   ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
ui <- fluidPage(
  
  # JavaScript-Handler für den Fortschrittsbalken
  tags$script(HTML("
    Shiny.addCustomMessageHandler('updateProgressBar', function(message) {
      // Setzt die Breite des Balkens je nach Prozentwert
      $('#progress-bar').css('width', message.percent + '%');
      // Zeigt den aktuellen Text im Fortschrittsbalken an
      $('#progress-bar').text(message.text);
    });
  ")),
  
  # CSS-Anpassung: Setzt die Mindesthöhe des .well-Containers auf 800px
  tags$style(HTML("
    .well {
      min-height: 800px;
    }
  ")),
  
  # Titelbereich als farbige Box
  div(
    style = "
    background-color: #007BFF;       
    color: white;                    
    margin-top: 10px !important;     
    margin-bottom: 10px !important; 
    padding-top: 5px !important;    
    padding-bottom: 5px !important;  
    text-align: center;              
    border-radius: 5px;             
    ",
    
    # Die eigentliche Titelzeile
    h3(
      "DYNAMISCHE STANDARDLASTPROFILE",
      style = "margin: 5px 0; font-weight: bold !important;"  # Weniger Abstand, fett
    )
  ),
  
  # Sidebar und Main Panel
  sidebarLayout(
    sidebarPanel(
      # Dropdown zur Auswahl des Profils
      selectInput("profilwahl", "Profil wählen:",
        choices = c(
          "(H25) - Haushaltsprofil" = "H25",
          "(G25) - Gewerbeprofil" = "G25",
          "(L25) - Landwirtschaftsprofil" = "L25",
          "(P25) - Kombinationsprofil PV" = "P25",
          "(S25) - Kombinationsprofil PV-Speicher" = "S25"
        ),
        # Vorauswahl
        selected = "H25",
        # Dropdown-Liste deaktivieren
        selectize = FALSE
      ),
      
      # Eingabe des Jahres
      numericInput("jahr", "Jahr wählen:", value = 2026, min = 2025, max = 2100),
      
      # Auswahl des Bundeslands für Feiertage
      selectInput("land", "Bundesland (für Feiertage):",
        choices = c(
          "Bundesweit anerkannte Feiertage" = "NATIONAL",
          "Baden-Württemberg" = "BW",
          "Bayern" = "BY",
          "Berlin" = "BE",
          "Brandenburg" = "BB",
          "Bremen" = "HB",
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
        # Vorauswahl
        selected = "NATIONAL",
        # Dropdown-Liste deaktivieren
        selectize = FALSE
      ),
      
      # Action Button mit Abstand nach unten
      div(
        # Abstand nach oben + zentrierte Ausrichtung
        style = "margin-top: 500px; text-align: center;",
        actionButton(
          "starte",
          "PROFIL ERSTELLEN",
          # volle Breite
          width = "100%",
          # Blauer Hintergrund, weißer fetter Text, schwarzer Rand, abgerundete Ecken 
          style = "font-weight: bold !important;
                   background-color: #007BFF; color: white; 
                   border: 2px solid black; border-radius: 8px; 
                   padding: 10px 20px; font-size: 16px;"
        )
      )
    ),
    
    # Main Panel
    mainPanel(
      # Überschrift für die Feiertage (dynamisch per uiOutput)
      uiOutput("feiertage_titel"),
      
      # Tabelle mit den Feiertagen
      DT::dataTableOutput("feiertage_tabelle"),
      
      # Etwas Abstand vor dem Status-Text
      HTML("<div style='margin-top: 20px;'></div>"),
      
      # Textausgabe für Status/Erfolgsmeldungen
      verbatimTextOutput("status")
    )
  )
)


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
#                                  SERVER                                   ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
server <- function(input, output, session) {
  
  # Fortschrittsbalken anzeigen, wenn Button gedrückt wird
  observeEvent(input$starte, {
    
    # Zeige ein Modal (Popup-Fenster) mit Fortschrittsbalken
    showModal(modalDialog(
      title = "Bitte warten",
      # Textanzeige über dem Balken (z.B. „Erstelle Profil …“)
      tags$div(id = "progress-text", "Erstelle Profil ..."),
      # Container für den Fortschrittsbalken (grauer Hintergrund)
      tags$div(
        id = "progress-bar-container",
        style = "width: 100%; background-color: #e9ecef; height: 25px; border-radius: 5px; margin-top: 10px;",
        
        # Fortschrittsbalken selbst (blau, animiert sich später per JS)
        tags$div(
          id = "progress-bar",
          style = "width: 0%; height: 100%; background-color: #007bff; color: white; text-align: center; line-height: 25px; border-radius: 5px;",
          # Initiale Textanzeige im Balken
          "0%"
        )
      ),
      # Ohne Button zum Schließen
      footer = NULL,
      # Modal kann nicht durch Klicken außerhalb geschlossen werden
      easyClose = FALSE
    ))
    
    # Funktion zum Aktualisieren des Fortschrittsbalkens im Modal
    updateProgress <- function(percent, text = "") {
      
      # Runde den Prozentwert auf ganze Zahlen
      percent <- round(percent)
      
      # Sende Nachricht an den Client (Browser), um den Fortschrittsbalken zu aktualisieren
      session$sendCustomMessage(
        # Name des Custom Message Handlers in JavaScript
        "updateProgressBar", 
        list(
          # Prozentwert für die Breite des Balkens
          percent = percent,  
          # Text, der im Balken angezeigt wird
          text = paste0(percent, "% – ", text)  
        )
      )
    }
    
    # Statusausgabe im Hauptfenster
    output$status <- renderPrint({
      # Initiale Statusmeldung
      cat("Verarbeitung gestartet...\n")
    })
    
    # Feiertage und Profil laden
    shiny::isolate({
      
      # Zeige Fortschritt bei 5 %
      updateProgress(5, "Lade Feiertage...")
      
      # Parameter aus UI holen
      jahr <- input$jahr
      land <- input$land
      profilwahl <- input$profilwahl
      
      # Feiertage aus API abrufen
      url <- paste0("https://feiertage-api.de/api/?jahr=", jahr, "&nur_land=", land)
      response <- GET(url)
      feiertage_data <- content(response, "text", encoding = "UTF-8")
      feiertage <- fromJSON(feiertage_data)
      
      # Feiertage in ein DataFrame umwandeln
      feiertage_df <- tibble(
        Datum = as.Date(sapply(feiertage, function(x) x$datum)),
        Feiertagsname = names(feiertage)
      )
      
      # Ausgabe der Bundesländer im Titel
      output$feiertage_titel <- renderText({
        # Bundeslandnamen zuordnen
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

        # Titel erzeugen
        HTML(paste("<h4><b>Feiertage", input$jahr, "für:", land_namen[input$land], "</b></h4>"))
      })
      
      # Ausgabe der Feiertage als Tabelle 
      output$feiertage_tabelle <- DT::renderDataTable({
        DT::datatable(
          feiertage_df,
          options = list(
            # nur Tabelle, ohne Extras
            dom = 't',              
            # Anzahl der Zeilen pro Seite
            pageLength = 20,
            # Seitenzahlen ausblenden
            paging = FALSE, 
            # Sortierung deaktivieren
            ordering = FALSE, 
            # Suchleiste ausblenden
            searching = FALSE,   
            # Info-Text ausblenden
            info = FALSE, 
            # Dropdown für Zeilenanzahl ausblenden
            lengthChange = FALSE     
          ),
          # keind Indexspalte
          rownames = FALSE      
        )
      })
      
      # Zeige Fortschritt: Lade das gewählte Profil
      updateProgress(20, "Lade Profil...")
      
      # Lade die gespeicherte RData-Datei mit allen Profil-DataFrames
      # Diese Datei enthält alle Profile (H25, G25, L25, S25, P25) als separate Objekte
      load("/media/archive/RStudio/EEG/Dynamische_Profile/SLP_Profiles.RData")
      
      # Wähle das aktuell gewünschte Profil anhand des Dropdown-Werts
      profil <- get(profilwahl)
      
      # Extrahiere und bereinige die Viertelstunden-Zeitstempel aus dem Profil
      viertelstunden <- profil$Uhrzeit |> 
        as.character() |> 
        # entfernt Leerzeichen am Anfang/Ende
        str_trim()  
      
      # Setze die Sprache für Datumsformate auf Deutsch
      Sys.setlocale("LC_TIME", "de_DE.UTF-8")
      # Erzeuge einen Vektor mit den vollständigen deutschen Monatsnamen als Text
      monate <- format(ISOdate(2000, 1:12, 1), "%B")
      
      # Statusausgabe für Fortschrittsbalken
      updateProgress(30, "Berechne Kalendertage...")
      
      # Erzeuge eine tibble mit allen Tagen im Jahr
      tage <- tibble(
        # Sequenz vom 01.01. bis 31.12. des gewählten Jahres
        Datum = seq(as.Date(paste0(jahr, "-01-01")),
                    as.Date(paste0(jahr, "-12-31")), by = "day")
      ) |> 
        mutate(
          # Monatsname (lang, nicht abgekürzt) z.B. "Januar"
          Monat = month(Datum, label = TRUE, abbr = FALSE),
          
          # Wochentag (z.B. Montag, Dienstag …)
          Wochentag = wday(Datum, label = TRUE, abbr = FALSE),
          
          # Ist der Tag ein Feiertag?
          Feiertag = Datum %in% feiertage_df$Datum,
          
          # Typ des Tages (FT = Feiertag oder Sonntag, SA = Samstag, WT = Werktag)
          Typ = case_when(
            Feiertag ~ "FT",
            Wochentag == "Samstag" ~ "SA",
            Wochentag == "Sonntag" ~ "FT",
            TRUE ~ "WT"
          ),
          
          # Tagesnummer im Jahr (1–365/366)
          t = yday(Datum),
          
          # Zusammensetzung der Spaltennamen im Profil (z.B. "Januar_WT")
          Profil_Spalte = paste0(Monat, "_", Typ)
        )
      
      
      # --- Statusausgabe für Fortschrittsbalken ---
      updateProgress(50, "Erzeuge Kombinationen...")
      
      # --- Erzeuge vollständige Kombination aus Datum und Uhrzeit ---
      tabelle <- expand.grid(
        # Alle Kalendertage im Jahr
        Datum = tage$Datum,
        # Alle 96 Viertelstunden-Zeitpunkte
        Uhrzeit = viertelstunden    
      ) |>
        as_tibble() |>
        
        # Uhrzeit als Zeichenkette formatieren
        mutate(Uhrzeit = as.character(Uhrzeit)) |>
        
        # Verknüpfe für jedes Datum die zugehörige Profilspalte (z.B. "März_WT") und Tagesnummer
        left_join(
          tage |> select(Datum, Profil_Spalte, t),
          by = "Datum"
        )
      
      # Statusausgabe für Fortschrittsbalken
      updateProgress(70, "Berechne Werte...")
      
      # Berechne dynamisierte Werte für jede Kombination
      tabelle <- tabelle |>
        # Zeilenweise Verarbeitung, da jede Zeile individuell behandelt wird
        rowwise() |>  
        mutate(
          # Originalwert (x0) aus dem Profil heraussuchen
          x0 = {
            # z.B. "März_WT"
            spalte <- Profil_Spalte 
            # Uhrzeit-Zeile finden
            zeile <- which(str_trim(as.character(profil$Uhrzeit)) == Uhrzeit)  
            if (length(zeile) == 1 && spalte %in% names(profil)) {
              # Komma durch Punkt ersetzen und in numerischen Wert umwandeln
              as.numeric(str_replace(profil[[spalte]][zeile], ",", "."))  
            } else {
              # Falls Uhrzeit oder Spalte nicht vorhanden: NA
              NA_real_  
            }
          },
          
          # Dynamisierungsfaktor berechnen nach Formel (abhängig vom Tag des Jahres)
          Faktor = (-3.92e-10 * t^4) + (3.2e-7 * t^3) - (7.02e-5 * t^2) + (2.1e-3 * t) + 1.24,
          
          # Dynamisierter Viertelstundenwert (x_dyn) = x0 * Faktor, auf 4 Nachkommastellen gerundet
          x_dyn = round(x0 * Faktor, 4)
        ) |>
        # rowwise-Operation beenden
        ungroup() |> 
        # Saubere Sortierung nach Datum und Uhrzeit
        arrange(Datum, Uhrzeit) |>
        # Spalte "t" in "Tag" umbenennen
        rename(Tag = t)  
      
      # Wenn das Profil G25 oder L25 ist, keine Dynamisierung
      if (profilwahl == "G25" || profilwahl == "L25") {
        # Nur Basiswerte ohne Dynamisierung ausgeben
        tabelle <- tabelle |> 
          select(Datum, Uhrzeit, Profil_Spalte, Tag, x0)
      } else {
        # Dynamisierte Werte ebenfalls mit ausgeben
        tabelle <- tabelle |> 
          select(Datum, Uhrzeit, Profil_Spalte, Tag, x0, x_dyn)
      }
      
      # Statusausgabe für den Fortschrittsbalken
      updateProgress(90, "Schreibe CSV-Datei...")
      
      # Pfad für den CSV-Export definieren
      pfad <- file.path(
        "/media/archive/RStudio/EEG/Dynamische_Profile/out",
        paste0(profilwahl, "_", jahr, "_", land, ".csv")
      )
      # Exportiere die erzeugte Tabelle als CSV im Windows-kompatiblen Format
      write.csv2(tabelle, file = pfad, row.names = FALSE)
      
      # Fortschrittsbalken auf 100 % setzen und kurze Pause zur Anzeige
      updateProgress(100, "Fertig!")
      Sys.sleep(0.5)
      # Modal-Fenster schließen
      removeModal()
      
      # Ausgabe des Speicherorts im Hauptbereich anzeigen
      output$status <- renderPrint({
        cat("Profil gespeichert unter:", pfad)
      })
      
    })
  })
  
  # Wenn die Sitzung endet, beende die App  
  session$onSessionEnded(function() {    
    stopApp()
  })
}

# Starte die App im Browser; Shiny wählt automatisch einen freien Port
runApp(
  list(ui = ui, server = server),
  launch.browser = TRUE
)
