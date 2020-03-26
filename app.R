# Pacotes ----

library(shiny)
library(shinyWidgets)
library(markdown)
library(shinydashboard)
library(shinycssloaders)
library(simulador.fundeb)
library(dplyr)
library(knitr)
library(rbokeh)
library(ineq)

# Carrega funções e modulos ----

for(i in seq_along(dir("modules"))){
  source(paste0("modules/", dir("modules")[i]), encoding = "UTF-8")
}

# Funções e opções ----
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

simplifica_text_input <-
  function(texto) {
    stringr::str_split(texto, ",", simplify = TRUE) %>% as_vector %>% as.numeric()
  }


# Ui ----
ui <- dashboardPage(
  # Estrutura da interface de Usuário ====
  dashboardHeader(title = "Simulador FUNDEB"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Como usar", tabName = "tutorial"),
      menuItem("Dashboard",
               tabName = "dashboard",
               icon = icon("dashboard")
               ),
      menuItem("Comparação",
               tabName = "comparacao",
               icon = icon("dashboard")
      ),
      menuItem("Todos", tabName = "todos", icon = icon("th")))),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
    # Tutorial tab content ====
    tabItem(tabName = "tutorial",
            fluidPage(
              box(width = 12,
                  uiOutput('markdown_tutorial')
                  ))),
    # Todos tab content ====
    tabItem(tabName = "todos",
            fluidPage(box(
            width = 12,
            uiOutput('markdown_todos')
            ))),
    # Dashboard tab content ====
    tabItem(
      tabName = "dashboard",
      fluidRow(
        box(select_files_alunos("dashboard")),
        box(select_files("dashboard"))
        ),
      fluidRow(
        box(select_options("dashboard")),
        box(select_numeric_vector("dashboard"))),
      fluidRow(
        box(select_slider("dashboard"))),
      fluidRow(column(width = 4),
        box(width = 4,
        botao_modulo("dashboard")
      )),
      fluidRow(
        column(width = 4),
        box(width = 4,
            selectInput(inputId = "filtro_ano", label = "Selecione ano para medidas resumo", choices = NULL))),
      fluidRow(
        box(width = 12,
          plotOutput("vaa_total") %>%  withSpinner()
          )),
      fluidRow(
        infoBoxOutput("vaa_medio_ente"),
        infoBoxOutput("vaa_mediano_ente"),
        infoBoxOutput("vaa_minimo_ente"),
        infoBoxOutput("vaa_maximo_ente"),
        infoBoxOutput("ente_max_vaa"),
        infoBoxOutput("ente_min_vaa"),
        infoBoxOutput("inter_quartil"),
        infoBoxOutput("inter_decil"),
        infoBoxOutput("max_min"),
        infoBoxOutput("desvio_padrao"),
        infoBoxOutput("desvio_padrao_somatorio"),
        infoBoxOutput("gini")),
      fluidRow(
        box(width = 12,
          DT::dataTableOutput("simulacao")
        )
      ),
      fluidRow(
        box(width = 12, 
            DT::dataTableOutput("simulacao_resumo"))
      )
      ),
    # Aba da comparacao entre modelos ====
    tabItem(tabName = "comparacao",
            fluidRow(
              box(select_files_alunos("comparacao")),
              box(select_files("comparacao"))
            ),
            fluidRow(
              box(
                select_comparacao_input_1("comparacao")),
              box(
                select_comparacao_input_2("comparacao")
              )),
              fluidRow(column(width = 4),
                       box(width = 4,
                           botao_modulo("comparacao")
                       )),
              fluidRow(
                column(width = 4),
                box(width = 4,
                    selectInput(inputId = "filtro_ano_comparacao", label = "Selecione ano para medidas resumo", choices = NULL))),
              fluidRow(plotOutput("grafico_resumo_comparacao")),
              fluidRow(box(width = 12,
                           DT::dataTableOutput("dt_comparacao"))),
              fluidRow(
                column(width = 4),
                box(boxwidth =  12,
                       DT::dataTableOutput("comparacao_resumo"))))))
    
  )
    
    

# Server ----
server <- function(session, input, output) {
  
  # Carrega bases ====
  alunos <- callModule(alunos_modulo, "dashboard")
  ponderador_alunos <- callModule(ponderador_alunos_modulo, "dashboard")
  socioeco <- callModule(socioeco_modulo, "dashboard")
  financeiro <- callModule(financeiro_modulo, "dashboard")
  
  # Simula modelos ====
  data <- callModule(simula, "dashboard", alunos = alunos(), ponderador_alunos = ponderador_alunos(), socioeco = socioeco(), financeiro = financeiro())
  
  output$vaa_total <- renderPlot({
    data_resumo() %>% 
      group_by(estado) %>% 
      summarise(vaa_final = mean(vaa_final)) %>% 
      ggplot(aes(x = estado, y = vaa_final)) +
      geom_col() +
      labs(x = "Estado", y = "VAAT médio")
  })

  anos_usados <- reactive({
    unique(data()$ano)
  })
  
  observeEvent(anos_usados(), {
    updateSelectInput(session, inputId = "filtro_ano", choices = anos_usados())
  })
  
  data_resumo <- reactive({
    data() %>% 
      filter(ano == input$filtro_ano)
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
  
  output$simulacao_resumo <- DT::renderDataTable({
    map_dfr(anos_usados(), ~(data() %>% filter(ano == .x) %>% comparacao_resumo() %>% mutate(ano = .x)))
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

  # Infoboxes ====
  output$vaa_medio_ente <- renderInfoBox({
    infoBox(
      HTML("VAA médio<br/>por ente"), paste0("R$", data_resumo()$vaa_final %>% mean() %>% round(digits = 2)), icon = icon("chalkboard-teacher"),
      color = "purple"
    )})
  
  output$vaa_mediano_ente <- renderInfoBox({
    infoBox(
      HTML("VAA mediano<br/>por ente"), HTML(paste0("R$", data_resumo()$vaa_final %>% median() %>% round(digits = 2))), icon = icon("chalkboard-teacher"),
      color = "purple"
    )})
  
  output$vaa_minimo_ente <- renderInfoBox({
    infoBox(
      HTML("VAA mínimo<br/>de um ente"), HTML(paste0("R$", data_resumo()$vaa_final %>% min() %>% round(digits = 2))), icon = icon("chalkboard-teacher"),
      color = "red"
    )})
  
  output$vaa_maximo_ente <- renderInfoBox({
    infoBox(
      HTML("VAA máximo<br/>de um ente"), HTML(paste0("R$", data_resumo()$vaa_final %>% max() %>% round(digits = 2))), icon = icon("chalkboard-teacher"),
      color = "green"
    )})
  
  output$ente_max_vaa <- renderInfoBox({
    infoBox(
      HTML("Estado com<br/>maior VAA médio"), HTML(paste0(data_resumo() %>% group_by(estado) %>% summarise(media = mean(vaa_final)) %>% top_n(media, n = 1) %>% pull(estado))), icon = icon("university"),
      color = "green"
    )})
  
  output$ente_min_vaa <- renderInfoBox({
    infoBox(
      HTML("Estado com<br/>menor VAA médio"), HTML(paste0(data_resumo() %>% group_by(estado) %>% summarise(media = mean(vaa_final)) %>% top_n(media, n = -1) %>% pull(estado))), icon = icon("university"),
      color = "red"
    )})
  
  ## Medidas de desvio
  output$inter_quartil <- renderInfoBox({
    infoBox(
      HTML("Razão interquartil"), HTML(paste0(data_resumo() %>% summarise(resumo = (quantile(vaa_final, 0.75)/quantile(vaa_final, 0.25)) %>% round(2)) %>% pull(resumo))), icon = icon("chart-line"),
      color = "purple"
    )})
  output$inter_decil <- renderInfoBox({
    infoBox(
      HTML("Razão interdecil"), HTML(paste0(data_resumo() %>% summarise(resumo = (quantile(vaa_final, 0.9)/quantile(vaa_final, 0.1)) %>% round(2)) %>% pull(resumo))), icon = icon("chart-line"),
      color = "purple"
    )})
  output$max_min <- renderInfoBox({
    infoBox(
      HTML("Razão Máximo<br/>Valor e Mínimo"), HTML(paste0(data_resumo() %>% summarise(resumo = (max(vaa_final)/min(vaa_final)) %>% round(2)) %>% pull(resumo))), icon = icon("chart-line"),
      color = "purple"
    )})
  output$desvio_padrao <- renderInfoBox({
    infoBox(
      HTML("Desvio Padrão<br/>do VAA"), HTML(paste0(data_resumo() %>% summarise(resumo = sd(vaa_final) %>% round(2)) %>% pull(resumo))), icon = icon("chart-line"),
      color = "purple"
    )})
  output$desvio_padrao_somatorio <- renderInfoBox({
    infoBox(
      HTML("Somatório do Desvio<br/>Padrão do VAA<br/> dos estados"), HTML(paste0(data_resumo() %>% filter(estado != "DF") %>%  group_by(estado) %>%  summarise(resumo = sd(vaa_final) %>% round(2)) %>% pull(resumo) %>% sum())), icon = icon("chart-line"),
      color = "purple"
    )})
  output$gini <- renderInfoBox({
    infoBox(
      HTML("Índice de Gini"), HTML(paste0(ineq::Gini(data_resumo()$vaa_final) %>% round(2))), icon = icon("chart-line"),
      color = "purple"
    )})
  
  # Carrega bases ====
  alunos <- callModule(alunos_modulo, "comparacao")
  ponderador_alunos <- callModule(ponderador_alunos_modulo, "comparacao")
  socioeco <- callModule(socioeco_modulo, "comparacao")
  financeiro <- callModule(financeiro_modulo, "comparacao")
  
  # Comparação de modelos ====
  data_comparacao <- callModule(compara, "comparacao", alunos = alunos(), ponderador_alunos = ponderador_alunos(), socioeco = socioeco(), financeiro = financeiro())
  
  output$dt_comparacao <- DT::renderDataTable({
    data_comparacao()
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
  
  anos_usados_comparacao <- reactive({
    unique(data_comparacao()$ano)
  })
  
  observeEvent(anos_usados_comparacao(), {
    updateSelectInput(session, inputId = "filtro_ano_comparacao", choices = anos_usados_comparacao())
  })
  
  mapeamento_ano_modelo <- reactive({
    expand.grid(anos = anos_usados_comparacao(), modelo =  1:2)
  })
  
  tabela_resumo <- reactive({
    map2_dfr(mapeamento_ano_modelo()$anos, mapeamento_ano_modelo()$modelo, ~comparacao_resumo(data_comparacao() %>% filter(ano == .x, modelo == .y)) %>% 
               mutate(ano = .x, modelo = .y))
  })
  
  output$comparacao_resumo <- DT::renderDataTable({
    tabela_resumo()
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
  
  output$grafico_resumo_comparacao <- renderPlot({
    tabela_resumo() %>% 
      filter(ano == input$filtro_ano_comparacao) %>% 
      ggplot(aes(x = modelo, fill = as.factor(modelo), y = Valores)) +
               geom_col()+
               facet_wrap(~Medidas, scales = "free") +
      labs(fill = "Modelo", x = "Modelo")
  })
  
  
  # Rmarkdown usado ====
  
  output$markdown_tutorial <- renderUI({
    HTML(markdown::markdownToHTML(knit('rmd/tutorial.rmd', quiet = TRUE)))
  })
  
  output$markdown_todos <- renderUI({
    HTML(markdown::markdownToHTML(knit('rmd/todos.rmd', quiet = TRUE)))
  })
  
  
}

# APP ----
shinyApp(ui = ui, server = server)