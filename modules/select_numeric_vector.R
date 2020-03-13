#' Variable selection for plot user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' 
select_numeric_vector <- function(id) {
  ns <- NS(id)
  
  tagList(
  textInput(
    ns("complem_uniao"),
    "Complementação da União (valores anuais separados por vírgula, ponto sendo símbolo de decimal",
    "0.1, 0.12"
  ),
  textInput(
    ns("complem_uniao_vaat"),
    "Complementação da União na segunda etapa do modelo híbrido (valores anuais separados por vírgula, ponto sendo símbolo de decimal",
    "0.05, 0.06"
  ),
  textInput(
    ns("crescimento_economico"),
    "Valor do crescimento econômico esperado no modelo a cada ano (valores anuais separados por vírgula, ponto sendo símbolo de decimal",
    "0, 0.02"
  ),
  textInput(
    ns("crescimento_demografico"),
    "Valor do crescimento demográfico da população de alunos esperado no modelo a cada ano  (valores anuais separados por vírgula, ponto sendo símbolo de decimal",
    "0, -0.02"
  )
  )
}