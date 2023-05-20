library(shiny)

# Definir a interface do usuário (UI)
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: #AED3E3; /* Cor de fundo azul claro */
        }
        
        .well {
          background-color: #ffffff; /* Cor de fundo branco para as caixas */
          border: 1px solid #dddddd; /* Borda das caixas */
          border-radius: 5px; /* Cantos arredondados das caixas */
          padding: 10px; /* Espaçamento interno das caixas */
        }
        
        .title-panel .title {
          background-color: #004B57; /* Cor de fundo vermelha para o titlePanel */
          color: #ffffff; /* Cor do texto do titlePanel (branco) */
          padding: 10px; /* Espaçamento interno do titlePanel */
        }
      ")
    )
  ),
  
  titlePanel(
    div(class = "title-panel", h1(class = "title", "Maximizador de Lucro"))
  ),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("semana", "Número da Semana:", value = 1, min = 1, max = 20)
    ),
    mainPanel(
      h4("Resultados:"),
      wellPanel(
        fluidRow(
          column(6, verbatimTextOutput("output1")),
          column(6, verbatimTextOutput("output2")),
          column(6, verbatimTextOutput("output3")),
          column(6, verbatimTextOutput("output4")),
          column(6, verbatimTextOutput("output5")),
          column(6, verbatimTextOutput("output6")),
          column(6, verbatimTextOutput("output7")),
          column(6, verbatimTextOutput("output8")),
          column(6, verbatimTextOutput("output9")),
          column(6, verbatimTextOutput("output10")),
          column(6, verbatimTextOutput("output11")),
          column(6, verbatimTextOutput("output12")),
          column(6, verbatimTextOutput("output13"))
        )
      )
    )
  )
)


# Definir o servidor (Server)
server <- function(input, output) {
  semana <- reactive({
    input$semana
  })
  
  output$output1 <- renderText({
    paste("Saída 1 para a semana", semana())
  })
  
  output$output2 <- renderText({
    paste("Saída 2 para a semana", semana())
  })
  
  output$output3 <- renderText({
    paste("Saída 3 para a semana", semana())
  })
  
  output$output4 <- renderText({
    paste("Saída 4 para a semana", semana())
  })
  
  output$output5 <- renderText({
    paste("Saída 5 para a semana", semana())
  })
  
  output$output6 <- renderText({
    paste("Saída 6 para a semana", semana())
  })
  
  
  output$output7 <- renderText({
    paste("Saída 7 para a semana", semana())
  })
  
  output$output8 <- renderText({
    paste("Saída 8 para a semana", semana())
  })
  
  output$output9 <- renderText({
    paste("Saída 9 para a semana", semana())
  })
  
  output$output10 <- renderText({
    paste("Saída 10 para a semana", semana())
  })
  
  output$output11 <- renderText({
    paste("Saída 11 para a semana", semana())
  })
  
  output$output12 <- renderText({
    paste("Saída 12 para a semana", semana())
  })
  
  output$output13 <- renderText({
    paste("Saída 13 para a semana", semana())
  })
}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)
