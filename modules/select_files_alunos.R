#' Variable selection for plot user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
select_files_alunos <- function(id) {
  ns <- NS(id)
  
  # assemble UI elements
  tagList(
    fileInput(
      "dados_alunos",
      "Selecione arquivo CSV com dados de alunos",
      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
    ),
    fileInput(
      "ponderador_alunos",
      "Selecione arquivo CSV com dados de peso de etapa de alunos",
      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
    )
  )
  


}