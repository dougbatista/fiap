
library(readxl)
Banco <- read_excel("D:/FIAP/Data Science/Banco.xlsx")
View(Banco)

f_abs <-table(Banco$sexo)
f_abs

f_rel <-prop.table(table(Banco$sexo))
f_rel

tabela <-round(t(rbind(f_abs,f_rel)),digits=2)
tabela


### Summatools

library(summarytools)

freq(Banco$sexo)
freq(Banco$catemp, order = "freq")

freq(Banco$catemp, order = "freq", round.digits = 1)
freq(Banco$catemp, totals=FALSE, cumul=FALSE, headings = FALSE)

freq(Banco$catemp, report.nas = F)

with(Banco, by(catemp, sexo, freq))
ctable(x=Banco$catemp, y=Banco$sexo, prop = 'r', round.digits = 2, justify = 'center')

table(Banco$sexo)
table(Banco$sexo,Banco$catemp)

library(gmodels)
CrossTable(Banco$sexo,Banco$catemp, chisq = T)


# dplyr
library(readxl)
Compras <- read_excel("D:/FIAP/Data Science/Compras.xlsx")
View(Compras)


library(dplyr)
library(rfm)
library(lubridate)

agregar <- summarise(group_by(Compras, id),
                     ticket_medio = mean(Valor_Compra),
                     data_max     = max(DT_Compra),
                     count        = n())

agregar


# lubridate
agregar$data_max <-as_date(agregar$data_max)
agregar$data_atual <-as_date(Sys.Date())

names(agregar)

# rfm
df_rfm <-rfm_table_order(agregar, id, data_max, ticket_medio, agregar$data_atual)
df_rfm


## selecionando registros

select <-df_rfm$rfm [df_rfm$rfm$rfm_score >=400,]


# Padronizar as variaveis

options(scipen = 100)
options(digits=3)

hist(Banco$salário)

media <-mean(Banco$salário)
sd    <-sd  (Banco$salário) 
z_salario <-(Banco$salário - media) / sd
Banco$z_salario <- z_salario

mean(Banco$z_salario)
sd(Banco$z_salario)

hist(Banco$z_salario)

Banco$zz_salario <-scale(Banco$salário)


### Função apply
Banco$soma  <- apply(Banco[,9:10],1,sum)
Banco$soma1 <- c(Banco$cartao_credito + Banco$Emprestimos)
Banco$soma2 <- rowSums(Banco[,9:10])

Banco

### Função rm.na
mean(Banco$salarin)
mean(Banco$salarin, na.rm = T)
round(mean(Banco$salarin, na.rm=T))

by(Banco$salário, Banco$sexo, mean)


### Programação estrutural
Banco$media <- (Banco$cartao_credito + Banco$Emprestimos)/2
Banco$classe <- NA
is.numeric(Banco$media)

for (i in 1:nrow(Banco)){
  if(Banco[i,"media"]>=10000){
    Banco[i,"classe"]<-"classe A"
  } else if (Banco[i, "media"]< 10000 & Banco[i,"media"]>=5000){
    Banco[i,"classe"]<- "classe B"
  } else{
    Banco[i, "classe"] <- "classe c"
  }
  }

table(Banco$classe)

Banco$resultado <- ifelse (Banco$estudo > 10, "Dr","Mestre")


## SQLDF
library(sqldf)

masculino <- sqldf("select * from Banco where sexo = 'Masculino'")
head(masculino)
select <- sqldf("select avg(salário) from Banco")
select

count <- sqldf("select count(id) from Banco")
count

group_by <- sqldf("select count(id),catemp from Banco group by catemp")
group_by

table(Banco$catemp)


### Forescating
AirPassengers


#install.packages("forecast")
library(forecast)
arima <-auto.arima(AirPassengers)

previsao <- forecast(arima, h=12)
previsao

plot(previsao)


### Caret  -  Divisão da amostra

library(readxl)
credito_scoring <- read_excel("D:/FIAP/Data Science/credito_scoring.xlsx")
View(credito_scoring)

names(credito_scoring)
dim(credito_scoring)


# package caret - machine learning - tem outras funções.
library(caret)

# a função set.seed sigifica semente aleatoria, precisamos garantir que estamos 
# trabalhando com a mesma amostra. Exemplo. criamos um modelos agora e ele acerta 80%
# fechamos nosso computador e no dia seguinte voltamos a ligar nossa maquina para continuar
# a criar o modelo. Se fizer a seleção da amostra ele vai pegar outros registros, pq é 
# aleatorio. com a função set.seed ele garante a mesma amostra de ontem por exemplo. assim
# é possivel comparar os modelos.

rnorm(20)
set.seed(123456); rnorm(20)

# aqui estou criando a partição através da função createDataPartition,
# p=.7 significa 70%
# list = FALSE - pq quero todas as colunas então trata como matrix
# times =  1 signifca quero 1 partição. Treinamento e teste.
# se a função times =2 teriamos 3 amostra. treinamento, validação e teste. 
# Algumas empresas usam isso.

treinamento <-createDataPartition(credito_scoring$ID, p=.7,list = FALSE, times = 1)
head(treinamento)

# Aqui estou criando a mostra de treinamento, 
# estou pegando os 70% dos ID criados no código anterior e todas as colunas do credito arvore
# tenho minha amostra de treinamento.
# temos 454 linhas
amostra_treinamento <-credito_scoring[treinamento,]
head(amostra_treinamento)

454/646
# Aqui, estou criando amostra de teste - amostra de treianmento.
# sinal [-treinamento,] significa, todos os registros e colunas menos os que estão
# na amostra de treinamento.
# aqgora temos duas amostras. Criamos o modelo na amostra de treinamento e testamos nosso
# modelo na amostra de teste.
# temos 192 linhas
amostra_teste <-credito_scoring[-treinamento,]
head(amostra_teste)

dim(amostra_treinamento)
dim(amostra_teste)
dim(credito_scoring)

454+192



##### Criando Modelo 

library(rpart)
library(rattle) 

table(credito_scoring$Cargo)


mytree <- rpart(
  Rank_credito ~ Pagamento + Faixa_Etaria + Cartao_Credito + Cargo,
  data = credito_scoring, 
  method = "class", 
  parms = list(split = 'gini'),
  minsplit = 2, 
  minbucket = 1 #  numero minimo em cada nó.
)


mytree


# plot do pacote rattle
fancyRpartPlot(mytree,type=5)

mytree$variable.importance

credito_scoring$prob <- predict(mytree, newdata = credito_scoring, type = "prob")
credito_scoring$class <- predict(mytree, newdata = credito_scoring, type = "class")


acerto <-table(credito_scoring$Rank_credito, credito_scoring$class)
acerto

table(credito_scoring$Rank_credito)
acerto_geral <- (acerto[1]+acerto[4])/sum(acerto)
acerto_geral

# Função caret
confusionMatrix(factor(credito_scoring$Rank_credito),factor(credito_scoring$class))


path.rpart(mytree,node=2)
path.rpart(mytree,node=3)
path.rpart(mytree,node=4)
path.rpart(mytree,node=5)
path.rpart(mytree,node=6)
path.rpart(mytree,node=7)
path.rpart(mytree,node=10)
path.rpart(mytree,node=11)





########################## Criando o modelo na amostra de Treinamento ###########

# amostra de treinamento
attach(amostra_treinamento)

mytree_treinamento <- rpart(Rank_credito ~ Pagamento + Faixa_Etaria + Cartao_Credito + Cargo,
                            data = amostra_treinamento, 
                            method = "class",  # poisson
                            parms = list(split = 'information'),
                            minsplit = 2, 
                            minbucket = 1 #  numero minimo em cada nó.
)

mytree_treinamento

dim(amostra_treinamento)

# plot do pacote rattle
fancyRpartPlot(mytree_treinamento, type=1)

mytree_treinamento$variable.importance

amostra_treinamento$prob <- predict(mytree_treinamento, newdata = amostra_treinamento, type = "prob")
amostra_treinamento$class <- predict(mytree_treinamento, newdata = amostra_treinamento, type = "class")


acerto <-table(amostra_treinamento$Rank_credito, amostra_treinamento$class)
acerto
acerto_geral <- (acerto[1]+acerto[4])/sum(acerto)
acerto_geral



###############  Amostra Menor estou criando modelo e não estou aplicando ####################################

# amostra de teste

amostra_teste$prob <- predict(mytree_treinamento, newdata = amostra_teste, type = "prob")
amostra_teste$class <- predict(mytree_treinamento, newdata = amostra_teste, type = "class")

fancyRpartPlot(mytree_treinamento, type=1)


acerto <-table(amostra_teste$Rank_credito, amostra_teste$class)
acerto
acerto_geral <- (acerto[1]+acerto[4])/sum(acerto)
acerto_geral


#############################  Aplicando modelo na Amostra de Teste ####################################

library(readxl)
novos_dados_credito_scoring <- read_excel("D:/FIAP/Data Science/novos_dados_credito_scoring.xlsx")
View(novos_dados_credito_scoring)

attach(novos_dados_credito_scoring)


novos_dados_credito_scoring$prob <- predict(mytree_treinamento, newdata = novos_dados_credito_scoring, type = "prob")
novos_dados_credito_scoring$class <- predict(mytree_treinamento, newdata = novos_dados_credito_scoring, type = "class")





