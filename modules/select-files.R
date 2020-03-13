#' Variable selection for plot user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
select_files <- function(id) {
  ns <- NS(id)
  
  # assemble UI elements
  tagList(
    fileInput(
      ns("dados_social"),
      "Selecione arquivo CSV com dados de informações sociais",
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      )
    ),
    fileInput(
      ns("dados_financeiro"),
      "Selecione arquivo CSV com dados de finanças",
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      )
    )
  )
  
}