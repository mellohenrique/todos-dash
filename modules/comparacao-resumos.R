# Modulo com dataframe com medidas resumo
## Tabela criada com dois vetores, 
## o primeiro com o nome da medida resumo
## o segundo com os valores de resumo

comparacao_resumo <- function(dados){
  tibble(Medidas = c("Média", "Mediana", "Máximo", "Mínimo", "Razão interquartial", "Razão interdecil", "Razão Valor máximo e mínimo", "Desvio Padrão do VAA", "Somatório do Desvio Padrão dos Estados", "Índice de Gini"),
         Valor = c(
           mean(dados$vaa_final),
           median(dados$vaa_final),
           max(dados$vaa_final),
           min(dados$vaa_final),
           dados %>% summarise(resumo = (quantile(vaa_final, 0.75)/quantile(vaa_final, 0.25)) %>% round(2)) %>% pull(resumo),
           dados %>% summarise(resumo = (quantile(vaa_final, 0.9)/quantile(vaa_final, 0.1)) %>% round(2)) %>% pull(resumo),
           dados %>% summarise(resumo = (max(vaa_final)/min(vaa_final)) %>% round(2)) %>% pull(resumo),
           dados %>% summarise(resumo = sd(vaa_final)) %>% pull(resumo),
           dados %>% filter(estado != "DF") %>%  group_by(estado) %>%  summarise(resumo = sd(vaa_final)) %>%  pull(resumo) %>% sum(),
           ineq::Gini(dados$vaa_final)
         )) }