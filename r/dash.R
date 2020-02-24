# Pacotes ----

library(shiny)
library(shinythemes)
library(simulador.fundeb)
library(shinyWidgets)

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
      tabItem(tabName = "dashboard",
              fluidRow(column(
                width = 12,
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
                  ),
                  fileInput(
                    "input_alunos",
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
                    "input_social",
                    "Selecione arquivo CSV com dados de informações sociais",
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                  ),
                  fileInput(
                    "input_financas",
                    "Selecione arquivo CSV com dados de finanças",
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                  ),
                  radioButtons(
                    inputId = "distribuicao_social",
                    choiceValues = c(TRUE, FALSE),
                    selected = FALSE,
                    choiceNames = c("Verdadeiro", "Falso"),
                    label = "Teste"
                  ),
                  
                  radioButtons(
                    inputId = "equalizacao_socio",
                    choiceValues = c(TRUE, FALSE),
                    selected = FALSE,
                    choiceNames = c("Verdadeiro", "Falso"),
                    label = "Teste"
                  ),
                  
                  radioButtons(
                    inputId = "modelo",
                    choiceValues = c("fundeb", "vat", "hibrido"),
                    selected = FALSE,
                    choiceNames = c("Modelo Fundeb", "Modelo VAT", "Modelo Híbrido"),
                    label = "Teste"
                  )
                )
              ),
                box(actionButton("botao", "simular")),
                box(dataTableOutput("summary_table"))
              )))))

# Server ----
server <- function(input, output) {
  data <- eventReactive(input$botao, {
    df <-
      simular_modelo_fundeb(
        alunos_teste,
        ponderador_alunos,
        socioeco_teste,
        financas_teste,
        min_social = input$parametro_social[[1]],
        max_social = input$parametro_social[[2]],
        min_financas = input$parametro_financeiro[[1]],
        max_financas = input$parametro_financeiro[[2]]
        
      )
    df
  })
  
  output$summary_table <- renderDataTable({
    data()
  })
}

# APP ----
shinyApp(ui = ui, server = server)