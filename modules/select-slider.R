#' Variable selection for plot user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
select_slider <- function(id) {
  ns <- NS(id)
  
  # assemble UI elements
  tagList(
  sliderInput(
    ns("parametro_social"),
    "Define intervalo do fator socioeconômico:",
    min = 1,
    max = 2,
    value = c(1, 1.3)
  ),
  
  sliderInput(
    ns("parametro_financeiro"),
    "Define intervalo do fator fiscal:",
    min = 1,
    max = 2,
    value = c(1, 1.3)
  ))
  
}