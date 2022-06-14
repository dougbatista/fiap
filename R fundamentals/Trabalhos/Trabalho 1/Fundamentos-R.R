## ARQUIVO CADASTRAL ##
## IMPORT DATASET - CADASTRAL ##

library(readxl)
Cadastral <- read_excel("Cadastral.xlsx")
View(Cadastral)

## 1) Tire uma tabela de frequência usando a função
## freq (summarytools) na variável Sexo.
## Quantos homens e quantas mulheres têm no arquivo?

install.packages("summarytools")
library(summarytools)
freq(Cadastral$Sexo, order="freq")

## 2) Remova os casos duplicados (olha para linha toda).
## E coloque esse arquivo limpo dentro de um outro arquivo chamado A.
## Para isso se utiliza a função unique no arquivo inteiro.

A <- unique(Cadastral)
View(A)


## 3) No arquivo A. Tire uma frequência usando a função
## freq (summarytools) na variável Sexo.
## Quantos homens e quantas mulheres possui o arquivo A?
## Precisa ser diferentes da primeira pergunta.

freq(A$Sexo, order = "freq")

## 5) Crie uma variável data atual (utilize a função Sys.date)
## e acrescenta essa variável no arquivo A.

A$DataAtual <- Sys.Date()

## 6) Verifique se a variável salario é numérica?
is.numeric(A$salario)

## 7) Mostre o mínimo e o máximo da variável salario.
summary(A$salario)

## 8) Crie uma variável faixa de salario (função cut) com as
## seguintes quebras: 1574, 3000, 5000, 7000, 13500.
faixa_salario <- cut(A$salario,
                     breaks = c(1574,3000,5000,7000,13500),
                     labels = c("D", "C", "B", "A"))

faixa_salario


## ARQUIVO TRANSACIONAL ##

## 9) Abra o arquivo transacional e chama ele de B.

B <- read_excel("Transacional.xlsx")
View(B)


## 10) Faça a união do arquivo A,B usando left join
## (função merge –packages base) do R. Consulte a função merge all.x

new_data = merge(A, B, all.x = TRUE)
View(new_data)
ncol(new_data)

## 11) Crie uma variável comprometimento de renda usando as
## variáveis ValorEmprestimo e Salario. Para isso utilize
## a expressão. (ValorEmprestimo / salario). Quantas variáveis ficaram no arquivo?

new_data$comprometimento = (new_data$ValorEmprestimo / new_data$salario)
ncol(new_data)

# Ficaram 17 colunas no arquivo.

## 12) Faça um tabela cruzada usando duas variáveis categóricas
## (escolha do aluno) usando o packages gmodels. Explique a tabela.

install.packages('gmodels')
library(gmodels)

CrossTable(new_data$default, new_data$Sexo, chisq = T, digits = 2)

# linha 1 (N) - Número de registros onde as duas variáveis comparadas se cruzam.

# linha 2 (contribuição do chi-quadrado) - Significa que quanto mais alto ele for, 
# significa que é maior a diferença da frequência ocorrida do que era esperada pela tabela.

# linha 3 - O número de ocorrências em relação ao número de linhas. (Frequencia relativa)

# linha 4 - O número de ocorrências em relação ao número de colunas.

# linha 5 - o número de ocorrências em relação a tabela total.

# Teste do chi quadrado:  SE p-valor > 0.5 aceita H0.
# SE p-valor < 0.5 Rejeita H0 e aceita H1.

# Se a H1 for aceita, significa que as variáveis possuem correlação.


## 13) Escolha uma variável de seu preferencia e faça um previsão
## no tempo utilizando o packages forecast.

install.packages("forecast", dependencies=TRUE)
install.packages("ggplot2")
install.packages("plotly")

library(ggplot2)
library(forecast)
library(plotly)

modelo <- auto.arima(new_data$TempodeResidencia)
previsao <- forecast(modelo , h=12)
previsao
plot(previsao)

## 14) Porque devemos criar novas variáveis?

## Devemos criar novas variáveis para tomarmos nota do que foi concluído sobre o conjunto de dados atual
# e realizar estudos mais aprofundados do tema que está sendo estudado.


## 15) Faça um gráfico utilizando o packages ggplot2 e plotly.
## O aluno pode escolher qualquer variável.

grafico_barras <- ggplot(new_data, aes(x=default, fill=Sexo))+
  geom_bar(position = 'dodge')+
  xlab("Inadimplência")+
  ylab("Numeros de clientes")+
  ggtitle("Pesquisa de Inadimplência por sexo")

grafico_barras

## Histograma do plotly

fig <- plot_ly(x = ~new_data$salario, type = "histogram")
fig
