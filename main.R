library(janitor)
library(ggplot2)

df <- read.csv('questionario_alcool.csv', 
               sep = ',', 
               fileEncoding = 'UTF-8',
               header = TRUE)

df <- df %>% janitor::clean_names()

df1 <- df[,-1]

antigo_nomes <- as.vector(colnames(df1))
novos_nomes <- c('curso', 'atividade_remunerada', 'tipo_atividade_remunerada',
                 'ingere_alcool', 'se_fornao_ja_ingeriu', 'bebe_onde', 
                 'sintomas', 'frequencia', 'ja_teve_problemas', 'gasto_mensal', 
                 'ja_tentou_parar', 'o_que_leva_a_beber')


df2 <- df1 %>% 
  dplyr::rename_with(
    ~novos_nomes, all_of(antigo_nomes)
  )




# contar palavras nas colunas que é possível mais de uma resposta

valores <- strsplit(df2$bebe_onde, ', ')

funcao_cont <- function(vetor_listas) {
  valores_gerais <- unlist(vetor_listas)
  total <- table(valores_gerais)
  return(total)
}

funcao_cont(valores)

