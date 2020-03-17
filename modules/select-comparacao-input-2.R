select_comparacao_input_2 <- function(id) {
  ns <- NS(id)
  
  tagList(
    radioButtons(
      inputId = ns("distribuicao_social_2"),
      choiceValues = c(TRUE, FALSE),
      selected = FALSE,
      choiceNames = c("Sim", "Não"),
      label = "Utiliza ponderadores de equidade na distribuição da complementação da União?"
    ),
    radioButtons(
      inputId = ns("equalizacao_socio_2"),
      choiceValues = c(TRUE, FALSE),
      selected = FALSE,
      choiceNames = c("Sim", "Não"),
      label = "Utiliza ponderadores de equidade na equalização intraestadual do FUNDEB?"
    ),
    radioButtons(
      inputId = ns("condicao_rede_2"),
      choiceValues = c(TRUE, FALSE),
      selected = TRUE,
      choiceNames = c("Sim", "Não"),
      label = "Os ponderadores de equidade intraestadual alteram os valores recebidos pela rede estadual?"
    ),
    radioButtons(
      inputId = ns("modelo_2"),
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
      inputId = ns("considerar_2"),
      choiceValues = c("social", "financas", "ambos"),
      selected = "ambos",
      choiceNames = c(
        "Fator socioeconômico",
        "Fator de disponibilidade fiscal",
        "Ambos os critérios"
      ),
      label = "Que fatores de equidade serão considerados?"
    ),
    textInput(
      ns("complem_uniao_2"),
      "Complementação da União (valores anuais separados por vírgula, ponto sendo símbolo de decimal",
      "0.1, 0.12"
    ),
    textInput(
      ns("complem_uniao_vaat_2"),
      "Complementação da União na segunda etapa do modelo híbrido (valores anuais separados por vírgula, ponto sendo símbolo de decimal",
      "0.05, 0.06"
    ),
    textInput(
      ns("crescimento_economico_2"),
      "Valor do crescimento econômico esperado no modelo a cada ano (valores anuais separados por vírgula, ponto sendo símbolo de decimal",
      "0, 0.02"
    ),
    textInput(
      ns("crescimento_demografico_2"),
      "Valor do crescimento demográfico da população de alunos esperado no modelo a cada ano  (valores anuais separados por vírgula, ponto sendo símbolo de decimal",
      "0, -0.02"
    ),
    sliderInput(
      ns("parametro_social_2"),
      "Define intervalo do fator socioeconômico:",
      min = 1,
      max = 2,
      value = c(1, 1.3)
    ),
    
    sliderInput(
      ns("parametro_financeiro_2"),
      "Define intervalo do fator fiscal:",
      min = 1,
      max = 2,
      value = c(1, 1.3)
    )
  )
}