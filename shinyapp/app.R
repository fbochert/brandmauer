###################### VORBEREITUNGEN ###############################

library(tidyverse)
library(sf)
library(ggiraph)
library(htmlwidgets)
library(shiny)

###################### SHINY APP ####################################

load(file = "full.Rdata") 

test <- full %>%
  mutate(
    DV = round(DV * 100, 2),
    DV_1 = round(DV_1 * 100, 2)) %>%
  rename(
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
                  choices = unique(test$Kategorie),
                  selected = "Kooperationen"
                  )
      ),
  mainPanel = mainPanel(
    girafeOutput('girafe_output', height = 800),
    p("Diese interaktive Grafik ist Teil des WZB Discussion Papers"),
    em("Brandmauer – is still alive! Empirische Ergebnisse zur Unterstützung der AfD in den ostdeutschen Kommunen durch etablierte Parteien (2019-2024)"),
    p("von Wolfgang Schroeder, Daniel Ziblatt, und Florian Bochert")),
  
  position = "right"))

server <- function(input, output, session) {
  
  df_of_interest <- reactive({test |> filter(Kategorie == input$selected_var)})
  
  output$girafe_output <- renderGirafe({
    
    mymap <- df_of_interest() |>
      ggplot(aes(geometry = geometry)) + 
      geom_sf(aes(fill = Wert)) +
      geom_sf_interactive(fill = NA,
                          aes(tooltip = glue::glue('{GEN}<br>Wert: {Wert}'))) +
      theme_minimal() +
      scale_fill_gradient(
        name = "Wert",
        low = "green",
        high = "red",
        na.value = "grey50") +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid  = element_blank())
    
    girafe(
      ggobj = mymap,
      options = list(
        opts_selection(
          type = "single"
        )
      )
    )
    
  })
}

shinyApp(ui, server)
