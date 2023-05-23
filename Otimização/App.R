library(shiny)


##Carregar resultado
carregarResultado <- function() {
 # source("Vendas_previstas_st_bud_app.R")
  #resultado
}


# Definir a interface do usuário (UI)
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css"),
    tags$style(
      HTML("
        body {
          background-color: #AED3E3; /* Cor de fundo azul claro */
          font-family: NewsGott, sans-serif;
          
        }
        
        
        .well {
          background-color: #ffffff; /* Cor de fundo branco para as caixas */
          border: 1px solid #dddddd; /* Borda das caixas */
          border-radius: 5px; /* Cantos arredondados das caixas */
          padding: 10px; /* Espaçamento interno das caixas */
        }
        
        .title-panel .title {
          background-color: #ffffff; /* Cor de fundo vermelha para o titlePanel */
          color: #000000; /* Cor do texto do titlePanel (branco) */
          padding: 10px; /* Espaçamento interno do titlePanel */
          font-family: NewsGott, sans-serif;
          border: 1px solid #dddddd;
          border-radius: 5px; /* Cantos arredondados das caixas */
          font-size:30px;
          font-weight:bold;
        }
        
        .scrollable-output {
        width: 100%; /* Defina a largura desejada para cada output */
        overflow-x: auto; /* Adicione uma barra de rolagem horizontal */
        border: 1px solid #dddddd; /* Opcional: adicione uma borda para separar os outputs */
        border-radius: 5px; /* Opcional: cantos arredondados da caixa do output */
        padding: 10px; /* Opcional: adicione espaçamento interno */

        }
        
        
      .small-output {
          width: 200px; /* Defina a largura desejada para as caixas de saída */
      }
        
        .bold-text {
        font-weight: bold;
        }
        
        /* Defina a fonte desejada para as saídas */
        .shiny-output-output pre {
       
        font-family: Arial, sans-serif;
        }
        
        .painel {
        
        background-color: #ffffff; 
          border: 1px solid #dddddd; 
          border-radius: 5px; 
          padding: 5px; 
          
          
        }
      ")
    )
  ),
  
  titlePanel(
    div(class = "title-panel", h1(class = "title", "Maximizador de Lucro"))
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("semana", "Número da Semana a Prever (20 (+ antiga) - 1 (+ recente)):", choices= 0:20),
      tags$hr(),
      h5("Datas a Prever:"),
      verbatimTextOutput("datasOutput"),
      
      h5("Vendas Previstas Steella:"),
      verbatimTextOutput("output1"),
      
      h5("Vendas  Previstas Bud:"),
      verbatimTextOutput("output3")
    ),
    mainPanel(
      div(class = "painel",
        fluidRow(
          column(6, 
                 
                   h5("Plano Armazém:"),
                   verbatimTextOutput("output2")
          ),
          column(6, 
                 
                   h5("Custos Totais:"),
                   verbatimTextOutput("output4")
                 
          ),
          column(6, 
                 
                   h5("Plano Veículo 1:"),
                   verbatimTextOutput("output5")
                 
          ),
          column(6, 
                 
                   h5("Bebidas Empacotas e Distribuidas Steella:"),
                   verbatimTextOutput("output6")
                 
          ),
          column(6, 
                 
                   h5("Plano Veicúlo 2"),
                   verbatimTextOutput("output7")
                 
          ),
          column(6, 
                  
                   h5("Bebidas Empacotas e Distribuidas Bud:"),
                   verbatimTextOutput("output8")
                 
          ),
          column(6, 
                 
                   h5("Plano Veículo 3"),
                   verbatimTextOutput("output9")
                 
          ),
          column(6, 
                  
                  h5(tags$strong("LUCRO FINAL:")),
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
    as.integer(input$semana)
  })
  
  output$output1 <- renderText({
    paste("Vendas Previstas Steella:")
  })
  
  
  output$output2 <- renderText({
    paste("Plano Armazém")
  })
  
  output$output3 <- renderText({
    paste("Vendas Previstas Bud")
  })
  
  output$output4 <- renderText({
    paste("Despesas")
    #resultado <- carregarResultado()
    #paste(resultado)
  })
  
  output$output5 <- renderText({
    paste("Plano Veículos Tipo 1")
  })
  
  output$output6 <- renderText({
    paste("PAC_STEELLA")
  })
  
  output$output7 <- renderText({
    paste("Plano Veículos Tipo 2")
  })
  
  output$output8 <- renderText({
    paste("PAC_BUD")
  })
  
  output$output9 <- renderText({
    paste("Plano Veículos Tipo 3")
  })
  
  output$output10 <- renderText({
    paste("Lucro final")
  })
  
  output$datasOutput <- renderText({
    paste("Sem Datas Previstas")
  })
  

  semana_anterior <- reactiveVal(0)

  observe({
    
    
    # Ação a ser realizada quando o valor da semana é alterado
    semana_selecionada <- as.integer(input$semana)
    
    
    if (semana_selecionada == 0 && semana_anterior() != 0) {
      # Define os valores das caixas de texto como vazios
      output$output1 <- renderText({
        paste("Vendas Previstas Steella:")
      })
      
      
      output$output2 <- renderText({
        paste("Plano Armazém")
      })
      
      output$output3 <- renderText({
        paste("Vendas Previstas Bud")
      })
      
      output$output4 <- renderText({
        paste("Despesas")
        #resultado <- carregarResultado()
        #paste(resultado)
      })
      
      output$output5 <- renderText({
        paste("Plano Veículos Tipo 1")
      })
      
      output$output6 <- renderText({
        paste("PAC_STEELLA")
      })
      
      output$output7 <- renderText({
        paste("Plano Veículos Tipo 2")
      })
      
      output$output8 <- renderText({
        paste("PAC_BUD")
      })
      
      output$output9 <- renderText({
        paste("Plano Veículos Tipo 3")
      })
      
      output$output10 <- renderText({
        paste("Lucro final")
      })
      
      output$datasOutput <- renderText({
        paste("Sem Datas Previstas")
      })
    }
    
    # Atualiza o valor da semana anterior
    semana_anterior(semana_selecionada)
    
    cerveja <- "steella"
    
   # melhor método steella
    metodo_ml <-"mlpe"
    
    # Realize as ações desejadas com o valor da semana aqui
    # Exemplo: print(semana)
    if (semana_selecionada > 0) {
      source("Vendas_previstas_st_bud_app.R")
      resultado <- semana_selecionada_ML_RW(semana_selecionada, cerveja, metodo_ml)
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
      
      print(predicted_sales_steella)
      
      output$output1 <- renderText({
        paste(round(predicted_sales_steella), collapse = ", ")
      })
      
      # Alterar cerceja
      cerveja <- "bud"
      
      #melhor metodo bud 
      metodo_ml <-"xgboost" 
      
      # Chamar novamente a função
      resultado_bud <- semana_selecionada_ML_RW(semana_selecionada, cerveja, metodo_ml)
      resultados_bud <- list(
        predicted_dates = resultado_bud$predicted_dates,
        predicted_sales = resultado_bud$predicted_sales
      )
      
      # Guardar vendas previstas BUD
      predicted_sales_bud <- resultados_bud$predicted_sales
    
        
      # Atualizar a caixa bud
      output$output3 <- renderText({
        paste(round(predicted_sales_bud), collapse = ", ")
      })
      
      
      #### Otimizazao
      
      resultado_otm <- Otimiza(predicted_sales_steella, predicted_sales_bud)
      
      ## guardar valores vindos em lista
      ## arm - 1
      ## v1 - 2
      ## v2 - 3
      ## ...
      
      ## guardar e ir buscar o arm
      resultado_arm <- resultado_otm[[1]]
      resultado_v1 <- resultado_otm[[2]]
      resultado_v2 <- resultado_otm[[3]]
      resultado_v3 <- resultado_otm[[4]]
      resultado_pac_st <- resultado_otm[[5]]
      resultado_pac_bud <- resultado_otm[[6]]
      resultado_lucro_final <- resultado_otm[[7]]
      despesas <- resultado_otm[[8]]
      
      # Atualizar a caixa armazem
      output$output2 <- renderText({
        paste(resultado_arm, collapse = ", ")
      })
      
      
      # Atualizar a caixa v1
      output$output5 <- renderText({
        paste(resultado_v1, collapse = ", ")
      })
      
      # Atualizar a caixa v2
      output$output7 <- renderText({
        paste(resultado_v2, collapse = ", ")
      })
      
      # Atualizar a caixa v3
      output$output9 <- renderText({
        paste(resultado_v3, collapse = ", ")
      })
      
      # Atualizar a caixa pac STEELLA
      output$output6 <- renderText({
        paste(resultado_pac_st, collapse = ", ")
      })
      
      # Atualizar a caixa pac BUD
      output$output8 <- renderText({
        paste(resultado_pac_bud, collapse = ", ")
      })
      
      # Atualizar a caixa pac STEELLA
      output$output10 <- renderText({
        paste(round(resultado_lucro_final), collapse = ", ")
      })
      
      # Atualizar a caixa custos
      output$output4 <- renderText({
        paste(despesas, collapse = ", ")
      })
      
    } 
  })
  

##end server
}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)
