alunos_modulo <- function(input, output, session) {
  reactive({
  inFile <- input$dados_alunos
  if (is.null(inFile))
    return(alunos_2015)
  df <- read_csv2(inFile$datapath)
  return(df)
})}


ponderador_alunos_modulo <-  function(input, output, session) {
  reactive({
  inFile <- input$ponderador_alunos
  if (is.null(inFile))
    return(ponderador_alunos)
  df <- read_csv2(inFile$datapath)
  return(df)
})}

socioeco_modulo <-  function(input, output, session) {
  reactive({
  inFile <- input$dados_social
  if (is.null(inFile))
    return(socioeco_2015)
  df <- read_csv2(inFile$datapath)
  return(df)
  })}

financeiro_modulo <-  function(input, output, session) {
  reactive({
  inFile <- input$dados_financeiro
  if (is.null(inFile))
    return(financas_2015)
  df <- read_csv2(inFile$datapath)
  return(df)
})}