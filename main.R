install.packages('tm')
install.packages('wordcloud')
install.packages("SnowballC")
library(janitor)
library(ggplot2)
library(forcats)
library(tidyverse)
library(tm)
library(stringi)
library(wordcloud)
library(SnowballC)

pct_format = scales::percent_format(accuracy = .1)

df <- read.csv('questionario_alcool.csv', 
               sep = ',', 
               fileEncoding = 'UTF-8',
               header = TRUE)
df5
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

str(df2)


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

formato <- theme(                                                       
  plot.title = element_text(size = 14, hjust = 0.5),
  axis.title.y = element_text(size = 12, vjust = 0.5, angle= 90),
  axis.title.x = element_text(size = 12, vjust = -0.2),
  axis.text.y = element_text(size = 10),
  axis.text.x = element_text(size = 10)
)

grafico_bebeOnde1 <-ggplot(data = teste, aes(x = valores_gerais, y = Freq, fill = valores_gerais)) +
  geom_bar(stat = "identity", width = 0.6, color = 'black') +
  labs(title = "Número de ambientes que se toma álcool", 
       x = "", y = "Frequência") +
  guides(fill = guide_legend(title = 'Lugares')) + 
  theme_minimal() +
  formato+
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



# Padronizando dados ------------------------------------------------------

df4 <- df3

for(i in 1:(ncol(df4)-1)){print(table(df4[i]))}

## Tipo de atividade remunerada
table(df4$tipo_atividade_remunerada)                 # Gsubs pra filtrar e alterar os dados
df4$tipo_atividade_remunerada <- gsub('Au.*', 'Trabalho Integral (CLT)', df4$tipo_atividade_remunerada)
df4$tipo_atividade_remunerada <- gsub('De.*', 'Não realizo', df4$tipo_atividade_remunerada)
df4$tipo_atividade_remunerada <- gsub('Trabalho .*', 'Trabalho Integral (CLT)', df4$tipo_atividade_remunerada)
df4$tipo_atividade_remunerada <- gsub('Corretor.*', 'Trabalho Integral (CLT)', df4$tipo_atividade_remunerada)
df4$tipo_atividade_remunerada <- gsub('prof.*', 'Trabalho Integral (CLT)', df4$tipo_atividade_remunerada)
df4$tipo_atividade_remunerada <- gsub('HOME .*', 'Trabalho Integral (CLT)', df4$tipo_atividade_remunerada)

## Frequência
table(df4$frequencia)
df4$frequencia <- gsub('.*0 e 1.*', 'Zero a uma vez', df4$frequencia)
df4$frequencia <- gsub('.*1 e 2.*', 'Uma a duas vezes', df4$frequencia)
df4$frequencia <- gsub('.*Mais.*', 'Mais de duas vezes', df4$frequencia)

## Gasto mensal
table(df4$gasto_mensal)
df4$gasto_mensal <- gsub('.*0 a 20.*', '0 a 20 reais', df4$gasto_mensal) 
df4$gasto_mensal <- gsub('.*20 a 50.*', '20 a 50 reais', df4$gasto_mensal)
df4$gasto_mensal <- gsub('.*50 a 100.*', '50 a 100 reais', df4$gasto_mensal)


write.csv(df4, file = 'dados_corrigidos.csv', fileEncoding = 'UTF-8',)      # Pro relatório
save(df4, file = 'df4.Rdata')






# Descritiva --------------------------------------------------------------



# Descrição da amostra ----------------------------------------------------



names(df4)

## Curso
data.curso <- data.frame(table(df4$curso))     # gráfico univariado de barras para o curso
ggplot(data = data.curso, aes(x = reorder(Var1, -Freq), y = Freq, fill = reorder(Var1, -Freq))) +
  geom_bar(stat = "identity", width = 0.8, color = 'black') +
  labs(title = "Curso realizado", 
       x = "", y = "Frequência") +
  guides(fill = guide_legend(title = 'Curso')) + 
  theme_minimal() +
  formato+
  scale_fill_brewer(palette = "Set1") +

  geom_label(aes(                          # Adicionando o número nas barras
    label = sprintf(
      '%d (%s)',
      Freq,
      pct_format(Freq / sum(Freq))
    )), stat='identity', fill='white', vjust=1.2)

ggsave('img/uni/curso.jpg', width = 10, height = 8)

## Ingestão de alcool
data.curso <- data.frame(table(df4$ingere_alcool))  # gráfico univariado de barras para ingestão de alcool
ggplot(data = data.curso, aes(x = reorder(Var1, Freq), y = Freq, fill = reorder(Var1, Freq))) +
  geom_bar(stat = "identity", width = 0.8, color = 'black') +
  labs(title = "Gráfico de barras da ingestão ou não de álcool", 
       x = "", y = "Frequência") +
  guides(fill = guide_legend(title = 'Ingere álcool?')) + 
  theme_minimal() +
  formato+
  scale_fill_brewer(palette = "Set1") +
  
  geom_label(aes(                                 # Adicionando o número nas barras
    label = sprintf(
      '%d (%s)',
      Freq,
      pct_format(Freq / sum(Freq))
    )), stat='identity', fill='white', vjust=1.2)
ggsave('img/uni/ingestao.jpg', width = 10, height = 8)


table(df4[df4$se_fornao_ja_ingeriu != '',]$se_fornao_ja_ingeriu)/68 
# aproximadamento 18% da nossa amostra nunca ingeriu alcool

data.curso <- data.frame(table(df4[df4$se_fornao_ja_ingeriu != '',]$se_fornao_ja_ingeriu))
ggplot(data = data.curso, aes(x = reorder(Var1, Freq), y = Freq, fill = reorder(Var1, Freq))) +
  geom_bar(stat = "identity", width = 0.8, color = 'black') +
  labs(title = "Gráfico de barras da ingestão anterior ou não de álcool", 
       x = "", y = "Frequência") +
  guides(fill = guide_legend(title = 'Já ingeriu álcool?')) + 
  theme_minimal() +
  formato+
  scale_fill_brewer(palette = "Set1") +
  
  geom_label(aes(                                  # Adicionando o número nas barras
    label = sprintf(
      '%d (%s)',
      Freq,
      pct_format(Freq / sum(Freq))
    )), stat='identity', fill='white', vjust=1.2)
ggsave('img/uni/ingestao_previa.jpg', width = 10, height = 8)

<<<<<<< HEAD
=======
  
  
data.curso <- data.frame(table(df4[df4$atividade_remunerada != '',]$atividade_remunerada))
ggplot(data = data.curso, aes(x = reorder(Var1, Freq), y = Freq, fill = reorder(Var1, Freq))) +
  geom_bar(stat = "identity", width = 0.8, color = 'black') +
  labs(title = "Gráfico de barras da realização de atividade remunerada", 
       x = "", y = "Frequência") +
  guides(fill = guide_legend(title = 'Realiza atividade?')) + 
  theme_minimal() +
  formato+
  scale_fill_brewer(palette = "Set1") +
  
  geom_label(aes(                              # Adicionando o número nas barras
    label = sprintf( 
      '%d (%s)',
      Freq,
      pct_format(Freq / sum(Freq))
    )), stat='identity', fill='white', vjust=1.2)
ggsave('img/uni/remuneracao.jpg', width = 10, height = 8)

# Criando um dataframe para aqueles que preencheram a questão
data.curso <- data.frame(table(df4[df4$tipo_atividade_remunerada != '',]$tipo_atividade_remunerada))
ggplot(data = data.curso, aes(x = reorder(Var1, -Freq), y = Freq, fill = reorder(Var1, -Freq))) +
  geom_bar(stat = "identity", width = 0.8, color = 'black') +
  labs(title = "Gráfico de barras da realização de atividade remunerada", 
       x = "", y = "Frequência") +
  guides(fill = guide_legend(title = 'Tipo de atividade')) + 
  theme_minimal() +
  formato+
  scale_fill_brewer(palette = "Set1") +
  
  geom_label(aes(                             # Adicionando o número nas barras
    label = sprintf(
      '%d (%s)',
      Freq,
      pct_format(Freq / sum(Freq))
    )), stat='identity', fill='white', vjust=1.2)
ggsave('img/uni/remuneracao.jpg', width = 10, height = 8)


# Análises ----------------------------------------------------------------

df_teste <- df4[df4$gasto_mensal != 'Não bebo',]
df_teste <- data.frame(table(df_teste$atividade_remunerada, df_teste$gasto_mensal)) 
# Fazendo um dataframe da frequência das variáveis de interesse

ggplot(data = df_teste, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.8, color = 'black', position =  "dodge") +
  labs(title = "Realização de atividade remunerada X Faixa de gasto", 
       x = "", y = "Frequência") +
  guides(fill = guide_legend(title = 'Realização de atividade')) + 
  theme_minimal() +
  formato+
  scale_fill_brewer(palette = "Set1") +
  
  geom_label(aes(                                    # Adicionando o número nas barras com posição dodge
    label = sprintf(
      '%s (%s)',
      x = Freq,
      pct_format((Freq / sum(Freq)))
    ), group = Var2), position = position_dodge2(width = 0.9, preserve ='total'),
    col = "black", fill='white',vjust = -.2, hjust = 0.5)
ggsave('img/uni/remuneracao.jpg', width = 10, height = 8)





# Tentativa de Word Cloud -------------------------------------------------

df5 <- df4

df5$o_que_leva_a_beber <- str_replace_all(df5$o_que_leva_a_beber, 
                                          "[[:punct:]]", "") # retira acento
df5$o_que_leva_a_beber <- stri_trans_general(df5$o_que_leva_a_beber, "Latin-ASCII") # retira pontuação

df5 <- df5 %>% 
  mutate(
    o_que_leva_a_beber = tolower(o_que_leva_a_beber)
  ) # joga tudo pra lowercase

df5$o_que_leva_a_beber <- gsub("\\b(bebo|nao|geralmente|estar|apenas|loucona|porque|beber|algo|pra|bebida)\\b", "", df5$o_que_leva_a_beber, ignore.case = TRUE)


corpus <- df5$o_que_leva_a_beber %>% 
  tm::VectorSource() %>% 
  Corpus()

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
auxCorpus <- tm_map(corpus, removeWords, stopwords('pt')) 
auxCorpus <- tm_map(auxCorpus, stemDocument)
wordcloud(auxCorpus,max.words=20,colors=c("blue","red"))



# data mining, que não faço ideia de como funciona por trás

# por algum motivo, ele as vezes ignora a ultima letra da palavra


# tem esse customizado também, vai de saberr qual fica melhor


wordcloud(words = auxCorpus, scale = c(3, 0.5), min.freq = 1, colors = brewer.pal(8, "Dark2")) +
  ggtitle("Customized Word Cloud") +
  xlab("Words") +
  ylab("Frequency")












