library(shiny)
library(shinythemes)
library(tidyverse)
library(googlesheets)
library(rmarkdown)
source('./library.R')

torneo_actual <- 'Apertura 2017'

# Data processing
raw <- gs_title('Clasificación FutbolBA') %>% 
  gs_read('partidos') %>% 
  filter(!is.na(Resultado) & Jugador != 'Invitado') %>%
  mutate(
    Fecha = as.Date(Fecha, "%d/%m/%Y")
  )

dat <- raw %>% 
  filter(Torneo == torneo_actual)

jugadores <- get_jugadores()

goals_by_player <- get_goals_by_player(dat)
results_by_player <- get_results_by_player(dat)
clasificacion <- get_clasificacion(results_by_player, goals_by_player)

partidos <- dat %>% 
  filter(!is.na(Jugador)) %>% 
  arrange(desc(Fecha)) %>% 
  mutate(
    Fecha = as.character(Fecha)
  )

######## ------ 

# Define UI for app
ui <- fluidPage(
  navbarPage(
    theme = shinytheme("simplex"),
    title = "FutbolBA",
    tabPanel("Clasificación",
             DT::dataTableOutput('tabla_ranking'),
             br(),
             shiny::includeMarkdown("clasificacion.md")
    ),
    tabPanel("Equipómetro",
             column(4,
                    h4("Selecciona:"),
                    selectInput("blancos", 
                                label = h5('Blancos'), 
                                choices = jugadores,
                                selected = 1,
                                multiple = TRUE),
                    selectInput("negros",
                                label = h5('Negros'),
                                choices = jugadores,
                                selected = 1,
                                multiple = TRUE),
                    numericInput("n_partidos",
                                label = h5('Partidos por jugador a considerar:'),
                                value = 5,
                                min = 1,
                                max = 999)
             ),
             column(4,
                    h4("Estadísticas por equipo:"),
                    tableOutput("equipometro")
             )
    ),
    tabPanel("Partidos",
             tableOutput('tabla_partidos')
    )
  )
)


# Define server logic for app
server <- function(input, output) {
  # choose columns to display
  output$tabla_ranking <- DT::renderDataTable({
    DT::datatable(clasificacion[, names(clasificacion), drop = FALSE],
                  rownames = FALSE,
                  options = list(paging = FALSE, searching = FALSE,
                                 lengthMenu = list(-1),
                                 width = 'auto'))
  })
  output$tabla_partidos <-  renderTable(partidos)
  
  # equipometro
  output$equipometro <- renderTable(get_equipometro(raw, input$blancos, input$n_partidos, 'Blancos') %>%
                                      bind_rows(get_equipometro(raw, input$negros, input$n_partidos, 'Negros')))
}

# Run the application 
shinyApp(ui = ui, server = server)

