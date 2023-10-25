library(janitor)
library(ggplot2)

df <- read.csv('questionario_alcool.csv', 
               sep = ',', 
               fileEncoding = 'UTF-8',
               header = TRUE)

df <- df %>% janitor::clean_names() # limpeza de nomes de colunas, mas não precisava

df1 <- df[,-1] # tirando a primeira coluna, é inútil 

antigo_nomes <- as.vector(colnames(df1))
novos_nomes <- c('curso', 'atividade_remunerada', 'tipo_atividade_remunerada',
                 'ingere_alcool', 'se_fornao_ja_ingeriu', 'bebe_onde', 
                 'sintomas', 'frequencia', 'ja_teve_problemas', 'gasto_mensal', 
                 'ja_tentou_parar', 'o_que_leva_a_beber')


df2 <- df1 %>% 
  dplyr::rename_with(
    ~novos_nomes, all_of(antigo_nomes)
  ) # substituindo os nomes antigos pelos novos




# contar palavras nas colunas que é possível mais de uma resposta


valores <- strsplit(df2$bebe_onde, ', ') # separa os valores com vírgula da coluna bebe_onde

funcao_cont <- function(vetor_listas) {
  valores_gerais <- unlist(vetor_listas) # tira aquele amontoado de listas com várias posições joga tudo num vetor só
  total <- table(valores_gerais) # usa table pra me dar a contagem
  return(total)
}

# tentei fazer uma função que passava o nome da coluna, e o df2 automaticamente fazia tudo, mas deu errado. 



valoresBebe_onde <- funcao_cont(valores)

teste <- as.data.frame(valoresBebe_onde)
teste$Freq <- sort(teste$Freq, decreasing = T) # aqui, reeordenei praficar mais bonito na ordenação das barras

grafico_bebeOnde <-ggplot(data = teste, aes(x = valores_gerais, y = Freq, fill = valores_gerais)) +
  geom_bar(stat = "identity", width = 0.6, color = 'black') +
  labs(title = "Número de ambientes que se toma álcool", 
       x = "", y = "Frequência") +
  guides(fill = guide_legend(title = 'Lugares')) + 
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") 
grafico_bebeOnde



