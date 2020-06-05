
select_slider <- function(id) {
  ns <- NS(id)
  tagList(
  sliderInput(
    ns("parametro_social"),
    "Define intervalo do fator socioeconômico:",
    min = 1,
    max = 2,
    value = c(1, 1.2)
  ),
  
  sliderInput(
    ns("parametro_financeiro"),
    "Define intervalo do fator fiscal:",
    min = 1,
    max = 2,
    value = c(1, 1.2)
  ))
  
}