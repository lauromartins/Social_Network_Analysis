# Analise de Dados de Redes Sociais usando o classificador naive bayes

# https://cran.r-project.org/src/contrib/Archive/Rstem/
# https://cran.r-project.org/src/contrib/Archive/sentiment/


# Troque pelo diretorio de trabalho que esta usando no seu computador
install.packages("C:/Users/Lauro Martins/Dropbox/Curso Formação Cientista de Dados/1. Big Data Analytics com R e Microsoft Azure Machine Learning/Projetos/1. Análise de Redes Sociais/Rstem_0.4-1.tar.gz", repos = NULL, type = "source")
install.packages("C:/Users/Lauro Martins/Dropbox/Curso Formação Cientista de Dados/1. Big Data Analytics com R e Microsoft Azure Machine Learning/Projetos/1. Análise de Redes Sociais/sentiment_0.2.tar.gz", repos = NULL, type = "source")
install.packages("ggplot2")
library(Rstem)
library(sentiment)
library(ggplot2)


# Coletando os tweets
tweetpt = searchTwitter("bigdata", n = 1500, lang = "pt")

# Obtendo o texto
tweetpt = sapply(tweetpt, function(x) x$getText())

# Removendo caracteres especiais
tweetpt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweetpt)

# Removendo @
tweetpt = gsub("@\\w+", "", tweetpt)

# Removendo pontuação
tweetpt = gsub("[[:punct:]]", "", tweetpt)

# Removendo digitos
tweetpt = gsub("[[:digit:]]", "", tweetpt)

# Removendo links html
tweetpt = gsub("http\\w+", "", tweetpt)

# Removendo espacos desnecessários
tweetpt = gsub("[ \t]{2,}", "", tweetpt)
tweetpt = gsub("^\\s+|\\s+$", "", tweetpt)



# Criando função para tolower
try.error = function(x)
{
  # Criando missing value
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

# Lower case
tweetpt = sapply(tweetpt, try.error)

# Removendo os NAs
tweetpt = tweetpt[!is.na(tweetpt)]
names(tweetpt) = NULL

# Classificando emocao
class_emo = classify_emotion(tweetpt, algorithm = "bayes", prior = 1.0)
emotion = class_emo[,7]

# Substituindo NA's por "Desconhecido"
emotion[is.na(emotion)] = "Desconhecido"

# Classificando polaridade
class_pol = classify_polarity(tweetpt, algorithm = "bayes")
polarity = class_pol[,4]

# Gerando um dataframe com o resultado
sent_df = data.frame(text = tweetpt, emotion = emotion,
                     polarity = polarity, stringsAsFactors = FALSE)

# Ordenando o dataframe
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels = names(sort(table(emotion), 
                                                                decreasing=TRUE))))


# Emoções encontradas
ggplot(sent_df, aes(x = emotion)) +
  geom_bar(aes(y = ..count.., fill = emotion)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Categorias", y = "Numero de Tweets") 

# Polaridade
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x = "Categorias de Sentimento", y = "Numero de Tweets")











