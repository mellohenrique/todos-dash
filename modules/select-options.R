# Modulo com parte de seleção de opções para a simulacao na UI
select_options <- function(id) {
  ns <- NS(id)
  
tagList(
  radioButtons(
    inputId = ns("equalizacao_socio"),
    choiceValues = c(TRUE, FALSE),
    selected = FALSE,
    choiceNames = c("Sim", "Não"),
    label = "Utiliza ponderadores de equidade na distribuição da complementação da União?"
  ),
  radioButtons(
    inputId = ns("fatores_intra_equidade"),
    choiceValues = c(TRUE, FALSE),
    selected = FALSE,
    choiceNames = c("Sim", "Não"),
    label = "Utiliza ponderadores de equidade na equalização intraestadual do FUNDEB?"
  ),
  radioButtons(
    inputId = ns("gov_estado_intraestadual"),
    choiceValues = c(FALSE, TRUE),
    selected = FALSE,
    choiceNames = c("Sim", "Não"),
    label = "Os ponderadores de equidade intraestadual alteram os valores recebidos pela rede estadual?"
  ),
  radioButtons(
    inputId = ns("condicao_rede"),
    choiceValues = c(FALSE, TRUE),
    selected = TRUE,
    choiceNames = c("Sim", "Não"),
    label = "Matrículas estaduais de Educação Infantil e matrículas municipais de Ensino Médio são contabilizadas na distribuição?"
  ),
  radioButtons(
    inputId = ns("modelo"),
    choiceValues = c("fundeb", "vaat", "hibrido"),
    selected = "fundeb",
    choiceNames = c(
      "Modelo FUNDEB atual (para fundos estaduais)",
      "Modelo VAAT",
      "Modelo Híbrido"
    ),
    label = "Qual modelo de complementação da União será simulado?"
  ),
  radioButtons(
    inputId = ns("considerar"),
    choiceValues = c("social", "financas", "ambos"),
    selected = "ambos",
    choiceNames = c(
      "Fator socioeconômico",
      "Fator de disponibilidade fiscal",
      "Ambos os critérios"
    ),
    label = "Que fatores de equidade serão considerados?"
  )
)
}
