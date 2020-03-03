# Pacotes ----

library(shiny)
library(shinyWidgets)
library(markdown)
library(shinydashboard)
library(shinycssloaders)
library(simulador.fundeb)
library(tidyverse)
library(knitr)
library(plotly)

# Funções e opções ----
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

simplifica_text_input <-
  function(texto) {
    stringr::str_split(texto, ",", simplify = TRUE) %>% as_vector %>% as.numeric()
  }

# Ui ----
ui <- dashboardPage(
  dashboardHeader(title = "Simulador FUNDEB"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Como usar", tabName = "tutorial"),
      menuItem("Dashboard",
               tabName = "dashboard",
               icon = icon("dashboard")
               ),
      menuItem("Todos", tabName = "todos", icon = icon("th")))),
  dashboardBody(tabItems(
    # First tab content ====
    tabItem(tabName = "tutorial",
            fluidPage(
              box(width = 12,
                  uiOutput('markdown_tutorial')
                  ))),
    # Second tab content ====
    tabItem(tabName = "todos",
            fluidPage(box(
            width = 12,
            uiOutput('markdown_todos')
            ))),
    # Third tab content ====
    tabItem(
      tabName = "dashboard",
      fluidRow(
        box(collapsible = TRUE,
            fileInput(
              "dados_alunos",
              "Selecione arquivo CSV com dados de alunos",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            fileInput(
              "ponderador_alunos",
              "Selecione arquivo CSV com dados de peso de etapa de alunos",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
            ),
        box(collapsible = TRUE,
            fileInput(
              "dados_social",
              "Selecione arquivo CSV com dados de informações sociais",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            fileInput(
              "dados_financeiro",
              "Selecione arquivo CSV com dados de finanças",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
            )
        ),
      fluidRow(
        box(
          radioButtons(
            inputId = "distribuicao_social",
            choiceValues = c(TRUE, FALSE),
            selected = FALSE,
            choiceNames = c("Sim", "Não"),
            label = "Utiliza a ponderação socioeconomica na distribuição do fundo?"),
          radioButtons(
            inputId = "equalizacao_socio",
            choiceValues = c(TRUE, FALSE),
            selected = FALSE,
            choiceNames = c("Sim", "Não"),
            label = "Utiliza a ponderação socioeconomica na equalização do fundo?"),
          radioButtons(
            inputId = "condicao_rede",
            choiceValues = c(TRUE, FALSE),
            selected = TRUE,
            choiceNames = c("Sim", "Não"),
            label = "Altera a ponderação da rede educacional se esta é difere entre redes estaduais e municipais?"),
          radioButtons(
            inputId = "modelo",
            choiceValues = c("fundeb", "vat", "hibrido"),
            selected = "fundeb",
            choiceNames = c("Modelo Fundeb", "Modelo VAT", "Modelo Híbrido"),
            label = "Qual modelo será simulado?"),
          radioButtons(
            inputId = "considerar",
            choiceValues = c("social", "financas", "ambos"),
            selected = "ambos",
            choiceNames = c("Critério Social", "Critério financeiro", "Ambos os critérios"),
            label = "Que critérios serão considerados na ponderação socioeconômica")),
        box(
          textInput(
            "auxilio_federal",
            "Auxílio Federal (valores anuais separados por vírgula, ponto sendo símbolo de decimal",
            "0.1, 0.12"
            ),
          textInput(
            "auxilio_federal_vat",
            "Auxílio Federal na segunda etapa do modelo híbrido (valores anuais separados por vírgula, ponto sendo símbolo de decimal",
            "0.05, 0.06"
            ),
          textInput(
            "crescimento_economico",
            "Valor do crescimento econômico esperado no modelo a cada ano (valores anuais separados por vírgula, ponto sendo símbolo de decimal",
            "0, 0.02"
            ),
          textInput(
            "crescimento_demografico",
            "Valor do crescimento demográfico da população de alunos esperado no modelo a cada ano  (valores anuais separados por vírgula, ponto sendo símbolo de decimal",
            "0, -0.02"
            )
          )
        ),
      fluidRow(
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
            )
          )
        ),
      fluidRow(column(width = 4),
        box(width = 4,
        actionBttn("botao", "simular", style = "jelly", size = "lg", 
                   block = TRUE)
         )
      ),
      fluidRow(
        box(width = 12,
          plotlyOutput("violino") %>%  withSpinner()
          )),
      fluidRow(
        box(width = 12,
          plotlyOutput("ponto_vaa_estado") %>%  withSpinner()
          )
        ),
      fluidRow(
        infoBoxOutput("vaa_medio_ente"),
        infoBoxOutput("vaa_mediano_ente"),
        infoBoxOutput("vaa_minimo_ente")
      ),
      fluidRow(
        infoBoxOutput("vaa_maximo_ente"),
        infoBoxOutput("ente_max_vaa"),
        infoBoxOutput("ente_min_vaa")),
      fluidRow(),
      fluidRow(
        box(
          DT::dataTableOutput("simulacao"), width = 12
        )
      )
      )
    )
    )
  )
    
    

# Server ----
server <- function(input, output) {
  
  alunos <- reactive({
    inFile <- input$dados_alunos
    if (is.null(inFile))
      return(alunos_2015)
    df <- read_csv2(inFile$datapath)
    return(df)
  })
  ponderador_alunos <- reactive({
    inFile <- input$ponderador_alunos
    if (is.null(inFile))
      return(ponderador_alunos)
    df <- read_csv2(inFile$datapath)
    return(df)
  })
  socioeco <- reactive({
    inFile <- input$dados_social
    if (is.null(inFile))
      return(socioeco_2015)
    df <- read_csv2(inFile$datapath)
    return(df)
  })
  financeiro <- reactive({
    inFile <- input$dados_financeiro
    if (is.null(inFile))
      return(financas_2015)
    df <- read_csv2(inFile$datapath)
    return(df)
  })
  
  data <- eventReactive(input$botao, {
    if (input$modelo == "fundeb") {
      df <-
        simular_modelo_fundeb_tempo(
          alunos(),
          ponderador_alunos(),
          socioeco(),
          financeiro(),
          considerar = input$considerar,
          distribuicao_fundo_estadual_socio = as.logical(input$distribuicao_social),
          equalizacao_socio = input$equalizacao_socio,
          condicao_rede = input$condicao_rede,
          min_social = input$parametro_social[[1]],
          max_social = input$parametro_social[[2]],
          min_financas = input$parametro_financeiro[[1]],
          max_financas = input$parametro_financeiro[[2]],
          auxilio_federal = simplifica_text_input(input$auxilio_federal),
          crescimento_economico = simplifica_text_input(input$crescimento_economico),
          crescimento_demografico = simplifica_text_input(input$crescimento_demografico)
          
        )
    } else if (input$modelo == "vat") {
      df <-
        simular_modelo_vat_tempo(
          alunos(),
          ponderador_alunos(),
          socioeco(),
          financeiro(),
          considerar = input$considerar,
          distribuicao_fundo_estadual_socio = as.logical(input$distribuicao_social),
          equalizacao_socio = input$equalizacao_socio,
          condicao_rede = input$condicao_rede,
          min_social = input$parametro_social[[1]],
          max_social = input$parametro_social[[2]],
          min_financas = input$parametro_financeiro[[1]],
          max_financas = input$parametro_financeiro[[2]],
          auxilio_federal = simplifica_text_input(input$auxilio_federal),
          crescimento_economico = simplifica_text_input(input$crescimento_economico),
          crescimento_demografico = simplifica_text_input(input$crescimento_demografico)
        )
    }  else if (input$modelo == "hibrido") {
      df <-
        simular_modelo_hibrido_tempo(
          alunos(),
          ponderador_alunos(),
          socioeco(),
          financeiro(),
          considerar = input$considerar,
          condicao_rede = input$condicao_rede,
          distribuicao_fundo_estadual_socio = as.logical(input$distribuicao_social),
          equalizacao_socio = input$equalizacao_socio,
          min_social = input$parametro_social[[1]],
          max_social = input$parametro_social[[2]],
          min_financas = input$parametro_financeiro[[1]],
          max_financas = input$parametro_financeiro[[2]],
          auxilio_federal = simplifica_text_input(input$auxilio_federal),
          auxilio_federal_vat = simplifica_text_input(input$auxilio_federal_vat),
          crescimento_economico = simplifica_text_input(input$crescimento_economico),
          crescimento_demografico = simplifica_text_input(input$crescimento_demografico)
        )
    }
    df
  })
  
  output$simulacao <- DT::renderDataTable({
    data()
  },
  server = FALSE,
  extensions = 'Buttons',
  options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    fixedColumns = TRUE,
    autoWidth = TRUE,
    ordering = TRUE,
    dom = 'Bftsp',
    buttons = c('copy', 'csv', 'excel')
  )
  )
  
  
  output$violino <- renderPlotly(
    ggplotly(data() %>%
               ggplot(aes(x = ano, y = vaa_final, fill = ano)) +
               scale_fill_viridis_d()+
               theme_bw() +
               guides(fill=FALSE) + 
               labs(fill = "", x = "Ano", y = "Valor Aluno Ano") +
               geom_boxplot()))
  
  output$ponto_vaa_estado <- renderPlotly({ggplotly(
    data() %>%
      ggplot(aes(
        x = fct_reorder(estado, codigo_estado),
        y = vaa_final,
        fill = ano
      )) +
      geom_boxplot() + 
      scale_fill_viridis_d() + 
      theme_bw() + 
      labs(fill = "Ano", x = "Estado", y = "Valor Aluno Ano")
  ) %>%
      layout(boxmode = "group")})
  
  output$markdown_tutorial <- renderUI({
    HTML(markdown::markdownToHTML(knit('rmd/tutorial.rmd', quiet = TRUE)))
  })
  
  output$markdown_todos <- renderUI({
    HTML(markdown::markdownToHTML(knit('rmd/todos.rmd', quiet = TRUE)))
  })
  
  output$vaa_medio_ente <- renderInfoBox({
    infoBox(
      HTML("VAA médio<br/>por ente"), paste0("R$", data()$vaa_final %>% mean() %>% round(digits = 2)), icon = icon("list"),
      color = "purple"
    )})
  
  output$vaa_mediano_ente <- renderInfoBox({
    infoBox(
      HTML("VAA mediano<br/>por ente"), HTML(paste0("R$", data()$vaa_final %>% median() %>% round(digits = 2))), icon = icon("list"),
      color = "purple"
    )})
  
  output$vaa_minimo_ente <- renderInfoBox({
    infoBox(
      HTML("VAA mínimo<br/>de um ente"), HTML(paste0("R$", data()$vaa_final %>% min() %>% round(digits = 2))), icon = icon("list"),
      color = "purple"
    )})
  
  output$vaa_maximo_ente <- renderInfoBox({
    infoBox(
      HTML("VAA máximo<br/>de um ente"), HTML(paste0("R$", data()$vaa_final %>% max() %>% round(digits = 2))), icon = icon("list"),
      color = "purple"
    )})
  
  output$ente_max_vaa <- renderInfoBox({
    infoBox(
      HTML("Estado com<br/>maior VAA médio"), HTML(paste0(data() %>% group_by(estado) %>% summarise(media = mean(vaa_final)) %>% top_n(media, n = 1) %>% pull(estado))), icon = icon("list"),
      color = "purple"
    )})
  
  output$ente_min_vaa <- renderInfoBox({
    infoBox(
      HTML("Estado com<br/>menor VAA médio"), HTML(paste0(data() %>% group_by(estado) %>% summarise(media = mean(vaa_final)) %>% top_n(media, n = -1) %>% pull(estado))), icon = icon("list"),
      color = "purple"
    )})

}

# APP ----
shinyApp(ui = ui, server = server)