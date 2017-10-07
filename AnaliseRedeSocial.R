# Análise de Dados de Redes Sociais

# ***** Esta é a versão 2.0 deste script, atualizado em 23/05/2017 *****
# ***** Esse script pode ser executado nas versões 3.3.1, 3.3.2, 3.3.3 e 3.4.0 da linguagem R *****
# ***** Recomendamos a utilização da versão 3.4.0 da linguagem R *****

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
setwd("C:/Users/Lauro Martins/Dropbox/Curso Formação Cientista de Dados/1. Big Data Analytics com R e Microsoft Azure Machine Learning/Projetos/1. Análise de Redes Sociais")
getwd()




## Etapa 1 - Pacotes e Autenticação

# Instalando e Carregando o Pacote twitteR
install.packages("twitteR")
install.packages("httr")
library(twitteR)
library(httr)

# Carregando a biblioteca com funções de limpeza
source('utils.R')


# Criando autenticacao no Twitter
# Voce precisa ter uma conta no twitter e criar uma aplicacao - https://apps.twitter.com
# Troque os valores abaixo pelos seus valores criados na aplicacao
key <- "K6qiYSiJVZs9N6Q0LDAyVBPeD"
secret <- "ph4DfkQSKZtJsDQpvZ3FkPzP8tAolkFeXwDG0FjmEmsKQNY7XF"
token <- "101514901-voF8xyGElivMTncLvHmaOhWBqADKTHktOqPFd1y8"
tokensecret <- "ziK0zYAjUh73KvFu95Bvs6Y18VRwJMAKsVsK0YCUM2QOd"

# Autenticação - Responda 1 quando perguntado sobre utilizar direct connection.
setup_twitter_oauth(key, secret, token, tokensecret)




## Etapa 2 - Conexão e captura dos tweets

# Verificando a timeline do usuário
userTimeline("dsacademybr")
userTimeline("lauromartins")
userTimeline("Microsoft")

# Capturando os tweets
tema <- "BigData"
qtd_tweets <- 100
lingua <- "pt"
tweetdata = searchTwitter(tema, n = qtd_tweets, lang = lingua)

# Visualizando as primeiras linhas do objeto tweetdata
head(tweetdata)





## Etapa 3 - Tratamento dos dados coletados através de text mining

# Instalando o pacote para Text Mining.
install.packages("tm")
install.packages("SnowballC")
library(SnowballC)
library(tm)

# Tratamento (limpeza, organização e transformação) dos dados coletados
tweetlist <- sapply(tweetdata, function(x) x$getText()) #transforma o objeto tweetdata (que é uma lista) em um vetor de caractere
tweetlist <- iconv(tweetlist, to = "utf-8", sub="") #converte os caracteres para o formato utf-8
tweetlist <- limpaTweets(tweetlist) #chama a funcao limpaTweets, que esta no arquivo utils.R, para realizar todo o processo de limpeza. Por exemplo, removendo hashtags, espacos, links, etc.
tweetcorpus <- Corpus(VectorSource(tweetlist)) #transforma o tweetlist em um Corpus (colecao de documentos que armazena dados e metadados)
tweetcorpus <- tm_map(tweetcorpus, removePunctuation) #remove pontuacoes do Corpus usando a funcao do pacto tm
tweetcorpus <- tm_map(tweetcorpus, content_transformer(tolower)) #forca uma transformacao dos caracteres para letra minuscula
tweetcorpus <- tm_map(tweetcorpus, function(x)removeWords(x, stopwords())) #remove as stopwords (palavras comuns do idioma)

# Convertendo o objeto Corpus para texto plano
# tweetcorpus <- tm_map(tweetcorpus, PlainTextDocument)
termo_por_documento = as.matrix(TermDocumentMatrix(tweetcorpus), control = list(stopwords = c(stopwords("portuguese"))))







## Etapa 4 - Wordcloud, associação entre as palavras e dendograma

# Instalando o pacote wordcloud
install.packages("RColorBrewer")
install.packages("wordcloud")
library(RColorBrewer)
library(wordcloud)

# Gerando uma nuvem palavras
pal2 <- brewer.pal(8,"Dark2")

wordcloud(tweetcorpus, 
          min.freq = 2, 
          scale = c(5,1), 
          random.color = F, 
          max.word = 60, 
          random.order = F,
          colors = pal2)

# Convertendo o objeto texto para o formato de matriz
tweettdm <- TermDocumentMatrix(tweetcorpus)
tweettdm

# Encontrando as palavras que aparecem com mais frequência
findFreqTerms(tweettdm, lowfreq = 11)

# Buscando associações
findAssocs(tweettdm, 'datascience', 0.60)

# Removendo termos esparsos (não utilizados frequentemente)
tweet2tdm <-removeSparseTerms(tweettdm, sparse = 0.9)

# Criando escala nos dados
tweet2tdmscale <- scale(tweet2tdm)

# Distance Matrix
tweetdist <- dist(tweet2tdmscale, method = "euclidean")

# Preprando o dendograma
tweetfit <- hclust(tweetdist)

# Criando o dendograma (verificando como as palvras se agrupam)
plot(tweetfit)

# Verificando os grupos
cutree(tweetfit, k = 3)

# Visualizando os grupos de palavras no dendograma
rect.hclust(tweetfit, k = 3, border = "red")






## Etapa 5 - Análise de Sentimento

# Criando uma função para avaliar o sentimento
install.packages("stringr")
install.packages("plyr")
library(stringr)
library(plyr)

sentimento.score = function(sentences, pos.words, neg.words, .progress = 'none')
{
  
  # Criando um array de scores com lapply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   sentence = gsub("[[:punct:]]", "", sentence)
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   sentence =gsub('\\d+', '', sentence)
                   tryTolower = function(x)
                   {
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   
                   sentence = sapply(sentence, tryTolower)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress = .progress )
  
  scores.df = data.frame(text = sentences, score = scores)
  return(scores.df)
}

# Mapeando as palavras positivas e negativas
pos = readLines("palavras_positivas.txt")
neg = readLines("palavras_negativas.txt")

# Criando massa de dados para teste
teste = c("Big Data is the future", "awesome experience",
          "analytics could not be bad", "learn to use big data")

# Testando a função em nossa massa de dados dummy
testesentimento = sentimento.score(teste, pos, neg)
class(testesentimento)

# Verificando o score
# 0 - expressão não possui palavra em nossas listas de palavras positivas e negativas ou
# encontrou uma palavra negativa e uma positiva na mesma sentença
# 1 - expressão possui palavra com conotação positiva 
# -1 - expressão possui palavra com conotação negativa
testesentimento$score





## Etapa 6 - Gerando Score da Análise de Sentimento

# Tweets por país
catweets = searchTwitter("ca", n = 500, lang = "en")
usatweets = searchTwitter("usa", n = 500, lang = "en")

# Obtendo texto
catxt = sapply(catweets, function(x) x$getText())
usatxt = sapply(usatweets, function(x) x$getText())

# Vetor de tweets dos países
paisTweet = c(length(catxt), length(usatxt))

# Juntando os textos
paises = c(catxt, usatxt)

# Aplicando função para calcular o score de sentimento
scores = sentimento.score(paises, pos, neg, .progress = 'text')

# Calculando o score por país
scores$paises = factor(rep(c("ca", "usa"), paisTweet))
scores$muito.pos = as.numeric(scores$score >= 1)
scores$muito.neg = as.numeric(scores$score <= -1)

# Calculando o total
numpos = sum(scores$muito.pos)
numneg = sum(scores$muito.neg)

# Score global
global_score = round( 100 * numpos / (numpos + numneg) )
head(scores)
boxplot(score ~ paises, data = scores)

# Gerando um histograma com o lattice
install.packages("lattice")
library("lattice")
histogram(data = scores, ~score|paises, main = "Análise de Sentimentos", xlab = "", sub = "Score")









