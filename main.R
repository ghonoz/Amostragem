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

# tentei fazer uma função que passava o nome da coluna, e o df2 automaticamente fazia tudo, mas deu errado



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

# chatgpt ajudou a customizar



# fazendo a intersecção da coluna ingere_alcool 

unique(df2$sintomas)
unique(df2$ingere_alcool)

df3 <- df2

df3 <- within(df3, sintomas[ingere_alcool == 'Não'] <- "Não bebo")
View(df3)

# talvez dá pra pensar que as pessoas que marcaram não no ingere alcool, já fizeram consumo em algum momento da vida, e marcaram os sintomas e lugares no que tinham antes de parar. acha melhor mudar ou não? 






# gráfico sintomas --------------------------------------------------------


coluna_sintomas <- strsplit(df2$sintomas, ', ')
valores_sintomas <- funcao_cont(coluna_sintomas)
t <- as.data.frame(valores_sintomas)

# tem uma resposta zoada na linha 9, sem dúvidas excluir. 
# aproveitando, excluir outras também
library(dplyr)

t <- t[-c(1, 9),]
unique(t$valores_gerais)



  