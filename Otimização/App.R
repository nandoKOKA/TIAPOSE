library(shiny)


##Carregar resultado
carregarResultado <- function() {
 # source("Vendas_previstas_st_bud_app.R")
  #resultado
}


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
      numericInput("semana", "Número da Semana a Prever (1-20):", value = 0, min = 0, max = 20),
      tags$hr(),
      h5("Datas a Prever:"),
      verbatimTextOutput("datasOutput")
    ),
    mainPanel(
      fluidRow(
        column(6, 
               wellPanel(
                 h5("Vendas Previstas Steella:"),
                 verbatimTextOutput("output1")
               )
        ),
        column(6, 
               wellPanel(
                 h5("Plano Armazém:"),
                 verbatimTextOutput("output2")
               )
        ),
        column(6, 
               wellPanel(
                 h5("Vendas  Previstas Bud:"),
                 verbatimTextOutput("output3")
               )
        ),
        column(6, 
               wellPanel(
                 h5("Plano Veículo 1:"),
                 verbatimTextOutput("output4")
               )
        ),
        column(6, 
               wellPanel(
                 h5("Plano Veículo 1:"),
                 verbatimTextOutput("output5")
               )
        ),
        column(6, 
               wellPanel(
                 h5("Bebidas Empacotas e Distribuidas Steella:"),
                 verbatimTextOutput("output6")
               )
        ),
        column(6, 
               wellPanel(
                 h5("Plano Veicúlo 2"),
                 verbatimTextOutput("output7")
               )
        ),
        column(6, 
               wellPanel(
                 h5("Bebidas Empacotas e Distribuidas Bud:"),
                 verbatimTextOutput("output8")
               )
        ),
        column(6, 
               wellPanel(
                 h5("Plano Veículo 3"),
                 verbatimTextOutput("output9")
               )
        ),
        column(6, 
               wellPanel(
                 h5("LUCRO FINAL:"),
                 verbatimTextOutput("output10")
               )
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
    paste("Vendas Previtas Steella")
  })
  
  output$output2 <- renderText({
    paste("Plano Armazém")
  })
  
  output$output3 <- renderText({
    paste("Vendas Previstas Bud")
  })
  
  output$output4 <- renderText({
    paste("Conteúdo 4")
    #resultado <- carregarResultado()
    #paste(resultado)
  })
  
  output$output5 <- renderText({
    paste("Conteúdo 5")
  })
  
  output$output6 <- renderText({
    paste("Conteúdo 6")
  })
  
  output$output7 <- renderText({
    paste("Conteúdo 7")
  })
  
  output$output8 <- renderText({
    paste("Conteúdo 8")
  })
  
  output$output9 <- renderText({
    paste("Conteúdo 9")
  })
  
  output$output10 <- renderText({
    paste("Conteúdo 10")
  })
  
  output$datasOutput <- renderText({
    paste("Sem Datas Previstas")
  })
  



  observe({
    # Ação a ser realizada quando o valor da semana é alterado
    semana_selecionada <- input$semana
    cerveja <- "steella"
    # Realize as ações desejadas com o valor da semana aqui
    # Exemplo: print(semana)
    if (semana_selecionada > 0) {
      source("Vendas_previstas_st_bud_app.R")
      resultado <- semana_selecionada_ML_RW(semana_selecionada, cerveja)
      resultados <- list(
        predicted_dates = resultado$predicted_dates,
        predicted_sales = resultado$predicted_sales
      )
      
      ## guardar datas previstas // 
      #  Datas previstas vem do ficheiro  ||Vendas_previstas_st_bud.R||
      predicted_dates <- resultados$predicted_dates
      output$datasOutput <- renderText({
        paste(predicted_dates, collapse = ", ")
      })
      
      #guardar vendas previstas STEELLA
      predicted_sales_steella <- resultados$predicted_sales
      
      output$output1 <- renderText({
        paste(predicted_sales_steella, collapse = ", ")
      })
      
      # Alterar cerceja
      cerveja <- "bud"
      # Chamar novamente a função
      resultado_bud <- semana_selecionada_ML_RW(semana_selecionada, cerveja)
      resultados_bud <- list(
        predicted_dates = resultado_bud$predicted_dates,
        predicted_sales = resultado_bud$predicted_sales
      )
      
      # Guardar vendas previstas BUD
      predicted_sales_bud <- resultados_bud$predicted_sales
      
      # Atualizar a caixa bud
      output$output3 <- renderText({
        paste(predicted_sales_bud, collapse = ", ")
      })
    } 
  })
  

##end server
}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)
