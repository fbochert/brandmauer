###################### VORBEREITUNGEN ###############################

library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
library(shiny)

###################### SHINY APP ####################################

data_map <- read_sf("full_2.shp") %>%
  st_transform(., crs = '+proj=longlat +datum=WGS84') %>%
  mutate(
    DV = round(DV * 100, 2),
    DV_1 = round(DV_1 * 100, 2)) %>%
  rename(
    "AfD-Anträge" = "AfD-Ant",
    "Kooperationen" = "Koprtnn",
    "starke Kooperationen" = "vKoop",
    "% Kooperation" = "DV",
    "% starke Kooperation" = "DV_1"
  ) %>%
  pivot_longer(cols = c("AfD-Anträge", "Kooperationen", "starke Kooperationen", 
                        "% Kooperation", "% starke Kooperation"),
               names_to = "Kategorie",
               values_to = "Wert")

ui <- fluidPage(
  titlePanel('Wie stabil ist die Brandmauer in Deutschland?'),
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      selectInput("selected_var", 
                  "Welche Kategorie soll dargestellt werden?", 
                  choices = unique(data_map$Kategorie),
                  selected = "Kooperationen"
      )
    ),
    mainPanel = mainPanel(
      leafletOutput('leaflet_output', height = 800),
      p("Diese interaktive Grafik ist Teil des WZB Discussion Papers"),
      em("Brandmauer – is still alive! Empirische Ergebnisse zur Unterstützung der AfD in den ostdeutschen Kommunen durch etablierte Parteien (2019-2024)"),
      p("von Wolfgang Schroeder, Daniel Ziblatt, und Florian Bochert")),
    
    position = "right"))

server <- function(input, output, session) {
  
  data_map_reactive <- reactive({
    data_map[data_map$Kategorie == input$selected_var, ]
  })
  
  output$leaflet_output <- renderLeaflet({
    
    data_map <- data_map_reactive()
    
    palo <- colorNumeric(palette = c("green", "red"),
                         domain = data_map$Wert)
    
    labelz <- sprintf("<strong>%s</strong><br/>Wert: %g",
                      data_map$GEN, data_map$Wert) %>%
      lapply(htmltools::HTML)
    
    leaflet(data_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 9.49, lat = 51.31, zoom = 5.5) %>%
      addPolygons(color = ~palo(Wert),
                  weight = 2,
                  opacity = 1,
                  fillOpacity = 0.9,
                  label = labelz) %>%
      addLegend(position = "bottomright",
                pal = palo,
                values = data_map$Wert,
                title = NULL,
                opacity = 1)
    
  })
}

shinyApp(ui, server)

