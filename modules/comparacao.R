compara <- function(input, output, session, alunos, ponderador_alunos, socioeco, financeiro) {
  
  simplifica_text_input <-
    function(texto) {
      stringr::str_split(texto, ",", simplify = TRUE) %>% as_vector %>% as.numeric()
    }
  eventReactive(input$botao, {
  if (input$modelo_1 == "fundeb") {
    df_1 <-
      simular_modelo_fundeb_tempo(
        alunos,
        ponderador_alunos,
        socioeco,
        financeiro,
        considerar = input$considerar_1,
        fatores_intra_equidade = as.logical(input$distribuicao_social_1),
        equalizacao_socio = input$equalizacao_socio_1,
        condicao_rede = input$condicao_rede_1,
        min_social = input$parametro_social_1[[1]],
        max_social = input$parametro_social_1[[2]],
        min_disp_fiscal = input$parametro_financeiro_1[[1]],
        max_disp_fiscal = input$parametro_financeiro_1[[2]],
        complem_uniao = simplifica_text_input(input$complem_uniao_1),
        crescimento_economico = simplifica_text_input(input$crescimento_economico_1),
        crescimento_demografico = simplifica_text_input(input$crescimento_demografico_1)
        
      )
  } else if (input$modelo_1 == "vaat") {
    df_1 <-
      simular_modelo_vaat_tempo(
        alunos,
        ponderador_alunos,
        socioeco,
        financeiro,
        considerar = input$considerar_1,
        fatores_intra_equidade = as.logical(input$distribuicao_social_1),
        equalizacao_socio = input$equalizacao_socio_1,
        condicao_rede = input$condicao_rede_1,
        min_social = input$parametro_social_1[[1]],
        max_social = input$parametro_social_1[[2]],
        min_disp_fiscal = input$parametro_financeiro_1[[1]],
        max_disp_fiscal = input$parametro_financeiro_1[[2]],
        complem_uniao = simplifica_text_input(input$complem_uniao_vaat_1),
        crescimento_economico = simplifica_text_input(input$crescimento_economico_1),
        crescimento_demografico = simplifica_text_input(input$crescimento_demografico_1)
      )
  }  else if (input$modelo_1 == "hibrido") {
    df_1 <-
      simular_modelo_hibrido_tempo(
        alunos,
        ponderador_alunos,
        socioeco,
        financeiro,
        considerar = input$considerar_1,
        condicao_rede = input$condicao_rede_1,
        fatores_intra_equidade = as.logical(input$distribuicao_social_1),
        equalizacao_socio = input$equalizacao_socio_1,
        min_social = input$parametro_social_1[[1]],
        max_social = input$parametro_social_1[[2]],
        min_disp_fiscal = input$parametro_financeiro_1[[1]],
        max_disp_fiscal = input$parametro_financeiro_1[[2]],
        complem_uniao = simplifica_text_input(input$complem_uniao_1),
        complem_uniao_vaat = simplifica_text_input(input$complem_uniao_vaat_1),
        crescimento_economico = simplifica_text_input(input$crescimento_economico_1),
        crescimento_demografico = simplifica_text_input(input$crescimento_demografico_1)
      )
  }
  if (input$modelo_2 == "fundeb") {
      df_2 <-
        simular_modelo_fundeb_tempo(
          alunos,
          ponderador_alunos,
          socioeco,
          financeiro,
          considerar = input$considerar_2,
          fatores_intra_equidade = as.logical(input$distribuicao_social_2),
          equalizacao_socio = input$equalizacao_socio_2,
          condicao_rede = input$condicao_rede_2,
          min_social = input$parametro_social_2[[1]],
          max_social = input$parametro_social_2[[2]],
          min_disp_fiscal = input$parametro_financeiro_2[[1]],
          max_disp_fiscal = input$parametro_financeiro_2[[2]],
          complem_uniao = simplifica_text_input(input$complem_uniao_2),
          crescimento_economico = simplifica_text_input(input$crescimento_economico_2),
          crescimento_demografico = simplifica_text_input(input$crescimento_demografico_2)
          
        )
    } else if (input$modelo_2 == "vaat") {
      df_2 <-
        simular_modelo_vaat_tempo(
          alunos,
          ponderador_alunos,
          socioeco,
          financeiro,
          considerar = input$considerar_2,
          fatores_intra_equidade = as.logical(input$distribuicao_social_2),
          equalizacao_socio = input$equalizacao_socio_2,
          condicao_rede = input$condicao_rede_2,
          min_social = input$parametro_social_2[[1]],
          max_social = input$parametro_social_2[[2]],
          min_disp_fiscal = input$parametro_financeiro_2[[1]],
          max_disp_fiscal = input$parametro_financeiro_2[[2]],
          complem_uniao = simplifica_text_input(input$complem_uniao_vaat_2),
          crescimento_economico = simplifica_text_input(input$crescimento_economico_2),
          crescimento_demografico = simplifica_text_input(input$crescimento_demografico_2)
        )
    }  else if (input$modelo_2 == "hibrido") {
      df_2 <-
        simular_modelo_hibrido_tempo(
          alunos,
          ponderador_alunos,
          socioeco,
          financeiro,
          considerar = input$considerar_2,
          condicao_rede = input$condicao_rede_2,
          fatores_intra_equidade = as.logical(input$distribuicao_social_2),
          equalizacao_socio = input$equalizacao_socio_2,
          min_social = input$parametro_social_2[[1]],
          max_social = input$parametro_social_2[[2]],
          min_disp_fiscal = input$parametro_financeiro_2[[1]],
          max_disp_fiscal = input$parametro_financeiro_2[[2]],
          complem_uniao = simplifica_text_input(input$complem_uniao_2),
          complem_uniao_vaat = simplifica_text_input(input$complem_uniao_vaat_2),
          crescimento_economico = simplifica_text_input(input$crescimento_economico_2),
          crescimento_demografico = simplifica_text_input(input$crescimento_demografico_2)
        )
    }
    bind_rows(df_1, df_2, .id = "modelo")
    })
 
}