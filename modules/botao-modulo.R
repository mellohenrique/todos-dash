# Módulo com o botão para simulação usado no aplicativo
botao_modulo <- function(id) {
  ns <- NS(id)
  actionBttn(ns("botao"), "Simular", style = "jelly", size = "lg", block = TRUE)
}