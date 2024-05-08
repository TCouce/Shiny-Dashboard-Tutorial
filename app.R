#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Libraries ====================================================================
library(tidyverse)
library(shiny)
library(shinydashboard)
library(palmerpenguins)
library(DT)
library(htmltools)
library(plotly)
library(bslib)
library(thematic)

# Data =========================================================================

data("penguins")
df <- penguins
# UI =penguins# UI =============================================================

# Define UI for application that draws a histogram
ui <- dashboardPage(skin ="yellow",
  dashboardHeader(title = "My dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gráficos", tabName = "Gráficos", icon = icon("dashboard")),
      menuItem("Tabla", tabName = "Tabla", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("Gráficos",
              fluidRow(
                column(width = 4,
                       box(title = tags$strong("Configuración de los datos"),
                           solidHeader = T,status = "warning",
                         selectInput("penguinvar",
                                     "Selecciona la variable de Interés: ",
                                     choices = c("Longitud del Pico" = "bill_length_mm",
                                                 "Profundidad del Pico" = "bill_depth_mm",
                                                 "Masa Corporal" = "body_mass_g")),
                         radioButtons("penguinspecies",
                                      "Selecciona la especie de interés: ",
                                      choices = c("Todas" = "All",
                                                  "Adelie" = "Adelie",
                                                  "Gentoo" = "Gentoo",
                                                  "Chinstrap" = "Chinstrap")),
                         width = 12),
                       box(title = tags$strong("Configuración del Histograma"),
                           solidHeader = T,
                           status = "warning",
                           icon = "line-chart",
                         sliderInput("nbins",
                                     "Selecciona número de barras: ",
                                     min = 10,
                                     max = 50,
                                     value = 20),
                         sliderInput("binwidth",
                                     "Selecciona amplitud de las barras: ",
                                     min = 0.1,
                                     max = 1,
                                     value = 0.3),                         
                         width = 12)
                ),
                column(width = 8,
                       box(title = tags$strong("Densidades"),
                           solidHeader = T,
                           status = "warning",
                           plotOutput("p1"), width = 12),
                       box(title = tags$strong("Histograma"),
                           solidHeader = T,
                           status = "warning",
                           plotOutput("p2"), width = 12)
                       
                )
              )
      ),
      tabItem("Tabla",
              fluidPage(
                h1("Pingüinos"),
                dataTableOutput("TablaPinguino")
              )
      )
    )
  ),
)

# SERVER =======================================================================

# Define server logic required to draw a histogram
server <- function(input, output) {
  thematic::thematic_shiny()
# OutPut Grafico Densidad
  output$p1 <- renderPlot({
    filtered_data <- df %>%
      drop_na()
    
    if (input$penguinspecies != "All") {
      filtered_data <- filtered_data %>%
        filter(species == input$penguinspecies)
    }
    
    filtered_data %>%
      ggplot(aes(x = .data[[input$penguinvar]])) +
      geom_density() +
      facet_wrap(~island) +
      labs(x = input$penguinvar,
           y = "Densidad") +
      theme_grey()
  })
# OutPut Grafico Histograma
  output$p2 <- renderPlot({
    filtered_data <- df %>%
      drop_na()
    
    if (input$penguinspecies != "All") {
      filtered_data <- filtered_data %>%
        filter(species == input$penguinspecies)
    }  
    filtered_data %>%
      ggplot(aes(x = .data[[input$penguinvar]])) +
      geom_histogram(bins = input$nbins,binwidth = input$binwidth) +
      facet_wrap(var(input$penguinspecies)) +
      labs(x = input$penguinvar,
           y = "Densidad") +
      theme_grey()
  })  
# OutPut Tabla Densidad  
  output$TablaPinguino <- renderDataTable(df)
}

# Run the application ==========================================================
shinyApp(ui = ui, server = server)


