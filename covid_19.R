
# Carregando os pacotes que serão utilizados

library(lubridate)
library(dplyr)
library(prophet)
library(forecast)
library(magrittr)
library(ggplot2)

# # Carregando o dataset de Covid-19 do Brasil.io e análisando as variáveis e obs.
# https://brasil.io/dataset/covid19/caso_full/

df <- read.csv("https://brasil.io/dataset/covid19/caso_full/?format=csv", stringsAsFactors = FALSE)
str(df)
View(df)

# Análise Exploratória

max(df$date)
min(df$date)

# Verificando os estados que tem novos óbitos por Covid-19
# Utilizando o pacote dplyr.

df %>%
  group_by(Estado = state) %>%
  summarise(Total = sum(new_deaths)) %>%
  arrange(desc(Total))

# Notamos que os estados mais atingindos são SP,RJ e CE. Vamos plotar em um gráfico para melhorar
# a visualização

vec1 <- c("SP","RJ","CE")  # Criando um vetor com os estados mais afetados.

df %>%
  group_by(Data = date, Estado = state) %>%
  summarise(Total = sum(new_deaths)) %>%
  filter(Estado %in% vec1 ) %>%
  ggplot(aes(x = Data, y = Total, group = Estado, colour = Estado)) +
  geom_line() +
  geom_smooth(method = "loess") 

# Podemos observar que o estado de SP é o mais atingido, entre 15 de Maio e 01 Junho o estado do RJ
# Se aproximou do patamar em SP e agora demonstra tendência de queda. 

# Vamos criar o mesmo cenário, para novos casos de contaminação:

# Identificando os estados mais afetados:

df %>%
  group_by(Estado = state) %>%
  summarise(Total = sum(new_confirmed)) %>%
  arrange(desc(Total))

# Criando um vetor com estados:

vec2 <- c("SP", "RJ", "CE", "PA")


df %>%
  group_by(Data = date, Estado = state) %>%
  summarise(Total = sum(new_confirmed)) %>%
  filter(Estado %in% vec2 ) %>%
  ggplot(aes(x = Data, y = Total, group = Estado, colour = Estado)) +
  geom_line() +
  geom_smooth(method = "loess")

# Notamos que a curva de novos casos em SP continua com uma tendência de aumento, enquanto os demais
# estados apararentam um achatamento

# Utilização do Facebook Prophet para realização de predição de novos óbitos por covid-19 em SP.

# O Prophet é construído com seu back-end em STAN, uma linguagem de programação probabilística. 
# Isso permite que o Prophet tenha muitas das vantagens oferecidas pelas estatísticas bayesianas, incluindo sazonalidade, inclusão de conhecimento de domínio e intervalos de confiança para criar uma estimativa de risco baseada em dados.
 

# Formato das colunas de dados: ds e y.
# A coluna ds (date) deve ter o formato esperado pelo Pandas, idealmente AAAA-MM-DD para uma data ou AAAA-MM-DD HH: MM: SS para timestamp.

# A coluna y deve ser numérica e representa a medida que desejamos prever.

# Criando as colunas ds e y:

df2 <- df %>%
        group_by(ds = date) %>%
        subset(df$state == "SP") %>%
        summarise(y = sum(new_deaths))

View(df2)
  

# Confiança
# Nas Séries Temporais a tendencia muda o tempo todo. Isso é histórico. Por isso é impossível ter certeza, então faremos a coisa mais razoável possível e assumimos que o futuro sofrerá mudanças de tendência.
# Nesse caso, assumiremos um intervalo de confiança de 95%, ou seja admitimos um erro de 5%.

dias <- c(30)

m <- prophet(df2, interval.width = 0.95)

future <- make_future_dataframe(m, periods = dias)

predicao <- predict(m, future)

tail(predicao[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
View(predicao)

plot(m, predicao)


prophet_plot_components(m, predicao)
