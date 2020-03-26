simula <- function(input, output, session, alunos, ponderador_alunos, socioeco, financeiro) {
  simplifica_text_input <-
    function(texto) {
      stringr::str_split(texto, ",", simplify = TRUE) %>% as_vector %>% as.numeric()
    }
  
  eventReactive(input$botao, {
    if (input$modelo == "fundeb") {
      df <-
        simular_modelo_fundeb_tempo(
          alunos,
          ponderador_alunos,
          socioeco,
          financeiro,
          considerar = input$considerar,
          fatores_intra_equidade = as.logical(input$distribuicao_social),
          equalizacao_socio = input$equalizacao_socio,
          desconsidera_estados = input$gov_estado_intraestadual,
          condicao_rede = input$condicao_rede,
          min_social = input$parametro_social[[1]],
          max_social = input$parametro_social[[2]],
          min_disp_fiscal = input$parametro_financeiro[[1]],
          max_disp_fiscal = input$parametro_financeiro[[2]],
          complem_uniao = simplifica_text_input(input$complem_uniao),
          crescimento_economico = simplifica_text_input(input$crescimento_economico),
          crescimento_demografico = simplifica_text_input(input$crescimento_demografico)
          
        )
    } else if (input$modelo == "vaat") {
      df <-
        simular_modelo_vaat_tempo(
          alunos,
          ponderador_alunos,
          socioeco,
          financeiro,
          considerar = input$considerar,
          fatores_intra_equidade = as.logical(input$distribuicao_social),
          equalizacao_socio = input$equalizacao_socio,
          desconsidera_estados = input$gov_estado_intraestadual,
          condicao_rede = input$condicao_rede,
          min_social = input$parametro_social[[1]],
          max_social = input$parametro_social[[2]],
          min_disp_fiscal = input$parametro_financeiro[[1]],
          max_disp_fiscal = input$parametro_financeiro[[2]],
          complem_uniao = simplifica_text_input(input$complem_uniao_vaat),
          crescimento_economico = simplifica_text_input(input$crescimento_economico),
          crescimento_demografico = simplifica_text_input(input$crescimento_demografico)
        )
    }  else if (input$modelo == "hibrido") {
      df <-
        simular_modelo_hibrido_tempo(
          alunos,
          ponderador_alunos,
          socioeco,
          financeiro,
          considerar = input$considerar,
          condicao_rede = input$condicao_rede,
          fatores_intra_equidade = as.logical(input$distribuicao_social),
          equalizacao_socio = input$equalizacao_socio,
          desconsidera_estados = input$gov_estado_intraestadual,
          min_social = input$parametro_social[[1]],
          max_social = input$parametro_social[[2]],
          min_disp_fiscal = input$parametro_financeiro[[1]],
          max_disp_fiscal = input$parametro_financeiro[[2]],
          complem_uniao = simplifica_text_input(input$complem_uniao),
          complem_uniao_vaat = simplifica_text_input(input$complem_uniao_vaat),
          crescimento_economico = simplifica_text_input(input$crescimento_economico),
          crescimento_demografico = simplifica_text_input(input$crescimento_demografico)
        )
    }
    df
  })
  
}