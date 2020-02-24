# Pacotes ----

library(shiny)
library(shinydashboard)
library(shinythemes)
library(simulador.fundeb)
library(shinyWidgets)
library(tidyverse)

# Ui ----
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Como usar", tabName = "tutorial"),
      menuItem("Dashboard",
               tabName = "dashboard",
               icon = icon("dashboard")),
      menuItem("Todos", tabName = "widgets", icon = icon("th"))
    )),
  dashboardBody(
    tabItems(
      # First tab content ====
      # Second tab content ====
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  radioButtons(
                    inputId = "distribuicao_social",
                    choiceValues = c(TRUE, FALSE),
                    selected = FALSE,
                    choiceNames = c("Verdadeiro", "Falso"),
                    label = "Utilizara a ponderação socioeconomica na distribuição do fundo?"
                  ),
                  
                  radioButtons(
                    inputId = "equalizacao_socio",
                    choiceValues = c(TRUE, FALSE),
                    selected = FALSE,
                    choiceNames = c("Verdadeiro", "Falso"),
                    label = "Utilizara a ponderação socioeconomica na equalização do fundo?"
                  ),
                  radioButtons(
                    inputId = "modelo",
                    choiceValues = c("fundeb", "vat", "hibrido"),
                    selected = "fundeb",
                    choiceNames = c("Modelo Fundeb", "Modelo VAT", "Modelo Híbrido"),
                    label = "Qual modelo será simulado?"
                  ),
                  radioButtons(
                    inputId = "considerar",
                    choiceValues = c("social", "financeiro", "ambos"),
                    selected = "ambos",
                    choiceNames = c("Critério Social", "Critério financeiro", "Ambos os critérios"),
                    label = "Que critérios serão considerados na ponderação socioeconômica"
                  )
                ),
                box(
                  fileInput(
                    "dados_alunos",
                    "Selecione arquivo CSV com dados de alunos",
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                  ),
                  fileInput(
                    "ponderador_alunos",
                    "Selecione arquivo CSV com dados de peso de etapa de alunos",
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                  ),
                  fileInput(
                    "dados_social",
                    "Selecione arquivo CSV com dados de informações sociais",
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                  ),
                  fileInput(
                    "dados_financeiro",
                    "Selecione arquivo CSV com dados de finanças",
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                  )),
                box(
                  textInput("auxilio_federal",
                                 "Auxílio Federal (valor anuais separados por vírgula, ponto sendo símbolo de decimal",
                  "0.1, 0.12"),
                textInput("auxilio_federal_vat",
                          "Auxílio Federal na segunda etapa do modelo híbrido (valor anuais separados por vírgula, ponto sendo símbolo de decimal",
                "0.05, 0.06"),
              textInput("crescimento_economico",
                        "Valor do crescimento econômico esperado no modelo a cada ano (valor anuais separados por vírgula, ponto sendo símbolo de decimal",
              "0.01, 0.02"),
      textInput("crescimento_demografico",
                "Valor do crescimento demográfico da população de alunos esperado no modelo a cada ano  (valor anuais separados por vírgula, ponto sendo símbolo de decimal",
      "-0.01, -0.02")),
              box(
                sliderInput(
                  "parametro_social",
                  "Define parametro social:",
                  min = 1,
                  max = 2,
                  value = c(1, 1.3)
                ),
                sliderInput(
                  "parametro_financeiro",
                  "Define parametro financeiro:",
                  min = 1,
                  max = 2,
                  value = c(1, 1.3)
                )),
                box(actionButton("botao", "simular")),
                box(dataTableOutput("summary_table"))
              )))))

# Server ----
server <- function(input, output) {
  data <- eventReactive(input$botao, {
    if(input$modelo == "fundeb"){
    df <-
      simular_modelo_fundeb_tempo(
        input$dados_alunos,
        input$ponderador_alunos,
        input$dados_socio,
        input$dados_financeiro,
        min_social = input$parametro_social[[1]],
        max_social = input$parametro_social[[2]],
        min_financas = input$parametro_financeiro[[1]],
        max_financas = input$parametro_financeiro[[2]]
        
      )} else if(input$modelo == "vat"){
        df <-
          simular_modelo_vat_tempo(
            input$dados_alunos,
            input$ponderador_alunos,
            input$dados_socio,
            input$dados_financeiro,
            min_social = input$parametro_social[[1]],
            max_social = input$parametro_social[[2]],
            min_financas = input$parametro_financeiro[[1]],
            max_financas = input$parametro_financeiro[[2]])
      }  else if(input$modelo == "hibrido"){
        df <-
          simular_modelo_hibrido_tempo(
            input$dados_alunos,
            input$ponderador_alunos,
            input$dados_socio,
            input$dados_financeiro,
            min_social = input$parametro_social[[1]],
            max_social = input$parametro_social[[2]],
            min_financas = input$parametro_financeiro[[1]],
            max_financas = input$parametro_financeiro[[2]])
      }
    df
  })
  
  output$summary_table <- renderDataTable({
    data()
  })
}

# APP ----
shinyApp(ui = ui, server = server)