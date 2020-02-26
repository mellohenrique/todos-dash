# Pacotes ----

library(shiny)
library(markdown)
library(shinydashboard)
library(simulador.fundeb)
library(tidyverse)
library(knitr)
library(prettydoc)
options(shiny.maxRequestSize=30*1024^2)
simplifica_text_input <- function(texto){stringr::str_split(texto, ",", simplify = TRUE) %>% as_vector %>% as.numeric()}

# Ui ----
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Como usar", tabName = "tutorial"),
      menuItem("Dashboard",
               tabName = "dashboard",
               icon = icon("dashboard")),
      menuItem("Todos", tabName = "todos", icon = icon("th"))
    )),
  dashboardBody(
    tabItems(
      # First tab content ====
      tabItem(tabName = "tutorial",
              fluidPage(box(width = 12,
                            uiOutput('markdown_tutorial')))),
      tabItem(tabName = "todos",
              fluidPage(box(width = 12,
                            uiOutput('markdown_todos')))),
      # Second tab content ====
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  radioButtons(
                    inputId = "distribuicao_social",
                    choiceValues = c(TRUE, FALSE),
                    selected = FALSE,
                    choiceNames = c("Sim", "Não"),
                    label = "Utilizara a ponderação socioeconomica na distribuição do fundo?"
                  ),
                  radioButtons(
                    inputId = "condicao_rede",
                    choiceValues = c(TRUE, FALSE),
                    selected = TRUE,
                    choiceNames = c("Sim", "Não"),
                    label = "Altera a ponderação da rede educacional se esta é difere entre redes estaduais e municipais?"
                  ),
                  radioButtons(
                    inputId = "equalizacao_socio",
                    choiceValues = c(TRUE, FALSE),
                    selected = FALSE,
                    choiceNames = c("Sim", "Não"),
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
                box(actionButton("botao", "simular"))),
      fluidRow(
                box(DT::dataTableOutput("summary_table"), width = 12)
              ),
      fluidRow(
        box(plotOutput("violino")),
        box(plotOutput("ponto_vaa_estado"))
      )))))

# Server ----
server <- function(input, output) {
  
  alunos <- reactive({
    inFile <- input$dados_alunos
    if (is.null(inFile))
      return(NULL)
    df <- read_csv2(inFile$datapath)
    return(df)
  })
  ponderador_alunos <- reactive({
    inFile <- input$ponderador_alunos
    if (is.null(inFile))
      return(NULL)
    df <- read_csv2(inFile$datapath)
    return(df)
  })
  socioeco <- reactive({
    inFile <- input$dados_social
    if (is.null(inFile))
      return(NULL)
    df <- read_csv2(inFile$datapath)
    return(df)
  })
  financeiro <- reactive({
    inFile <- input$dados_financeiro
    if (is.null(inFile))
      return(NULL)
    df <- read_csv2(inFile$datapath)
    return(df)
  })
  
  data <- eventReactive(input$botao, {
    
    if(input$modelo == "fundeb"){
    df <-
      simular_modelo_fundeb_tempo(
        alunos(),
        ponderador_alunos(),
        socioeco(),
        financeiro(),
        condicao_rede = input$condicao_rede,
        min_social = input$parametro_social[[1]],
        max_social = input$parametro_social[[2]],
        min_financas = input$parametro_financeiro[[1]],
        max_financas = input$parametro_financeiro[[2]],
        auxilio_federal = simplifica_text_input(input$auxilio_federal),
        crescimento_economico = simplifica_text_input(input$crescimento_economico),
        crescimento_demografico = simplifica_text_input(input$crescimento_demografico)
        
      )} else if(input$modelo == "vat"){
        df <-
          simular_modelo_vat_tempo(
            alunos(),
            ponderador_alunos(),
            socioeco(),
            financeiro(),
            condicao_rede = input$condicao_rede,
            min_social = input$parametro_social[[1]],
            max_social = input$parametro_social[[2]],
            min_financas = input$parametro_financeiro[[1]],
            max_financas = input$parametro_financeiro[[2]],
            auxilio_federal = simplifica_text_input(input$auxilio_federal),
        crescimento_economico = simplifica_text_input(input$crescimento_economico),
        crescimento_demografico = simplifica_text_input(input$crescimento_demografico))
      }  else if(input$modelo == "hibrido"){
        df <-
          simular_modelo_hibrido_tempo(
            alunos(),
            ponderador_alunos(),
            socioeco(),
            financeiro(),
            condicao_rede = input$condicao_rede,
            min_social = input$parametro_social[[1]],
            max_social = input$parametro_social[[2]],
            min_financas = input$parametro_financeiro[[1]],
            max_financas = input$parametro_financeiro[[2]],
        auxilio_federal = simplifica_text_input(input$auxilio_federal),
        auxilio_federal_vat = simplifica_text_input(input$auxilio_federal_vat),
        crescimento_economico = simplifica_text_input(input$crescimento_economico),
        crescimento_demografico = simplifica_text_input(input$crescimento_demografico))
      }
    df
  })
  
  output$summary_table <- DT::renderDataTable({
    data() 
  }, 
  options = list(scrollX = TRUE,
                 paging = TRUE,
                 searching = TRUE,
                 fixedColumns = TRUE,
                 autoWidth = TRUE,
                 ordering = TRUE,
                 dom = 'tB',
                 buttons = c('copy', 'csv', 'excel')))
  
  output$violino <- renderPlot(
    data() %>% 
      ggplot(aes(x = ano, y = vaa_final)) +
      geom_violin()
                                )
  output$ponto_vaa_estado <- renderPlot(
    data() %>% 
      ggplot(aes(x = codigo_estado, y = vaa_final)) +
               geom_jitter()
  )
  
  output$markdown_tutorial <- renderUI({
    HTML(markdown::markdownToHTML(knit('../rmd/tutorial.rmd', quiet = TRUE)))
  })
  
  output$markdown_todos <- renderUI({
    HTML(markdown::markdownToHTML(knit('../rmd/todos.rmd', quiet = TRUE)))
  })
  
}


# APP ----
shinyApp(ui = ui, server = server)