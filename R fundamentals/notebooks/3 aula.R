

library(readxl)
Banco <- read_excel("E:/Fiap Turma DTS/Banco.xlsx")
View(Banco)

names(Banco)
summary(Banco$salário)
Banco$fx_salario <-cut(Banco$salário,
                       breaks = c(15749,24000,29100,38100,135000),
                       labels=c("15749 - 24000","24001 - 29100",
                                "29101 -38100","38101 - 135000"))


table(Banco$fx_salario)


#### aula de hoje

Banco$media <-c(Banco$cartao_credito + Banco$Emprestimos)/2
Banco$classe <- NA

is.numeric(Banco$media)

for (i in 1:nrow(Banco)){
  if (Banco[i, "media"]>= 10000){
    Banco[i, "classe"] <- "classe A"
} else if (Banco[i, "media"] < 10000 & Banco[i, "media"]>= 5000){
  Banco[i, "classe"] <-"classe B"
}else{
  Banco[i,"classe"] <- "classe C"
}
}

table(Banco$classe)

Banco$resultado <- ifelse(Banco$estudo > 10, "doutorado","mestrado")
table(Banco$resultado)


### SQL
# install.packages("sqldf")
library(sqldf)

masculino <- sqldf("select * from Banco where sexo = 'Masculino'")
masculino


table(masculino$sexo)

quantsexo <- sqldf("select sexo, count(*) from Banco group by sexo")
quantsexo

quantcatemp <- sqldf("select catemp,count(*) from Banco group by catemp")
quantcatemp

Banco <- sqldf(c("update Banco set salário = 5000 where id = 473",
                 "select * from Banco"))


### Dados no tempo

AirPassengers
plot(AirPassengers)

install.packages("forecast")
library(forecast)
modelo <- auto.arima(AirPassengers)
previsao <-forecast(modelo , h=12)
previsao
plot(previsao)



### 
install.packages(c("caret","rpart","rattle"))
library(caret)
library(rpart)
library(rattle)

rnorm(20)
rnorm(20)
set.seed(12345);rnorm(20)
set.seed(12345);rnorm(20)


library(readxl)
credito_scoring <- read_excel("E:/Fiap Turma DTS/credito_scoring.xlsx")
View(credito_scoring)

names(credito_scoring)

mytree <- rpart(Rank_credito ~  Cargo+ Pagamento+Faixa_Etaria+Cartao_Credito,
                data =credito_scoring,
                method = "class",
                parms = list(split = 'gini'),
                minsplit =2,
                minbucket = 1)

mytree

table(credito_scoring$Rank_credito)
table(credito_scoring$Cargo)
table(credito_scoring$Faixa_Etaria)
table(credito_scoring$Pagamento)
table(credito_scoring$Cartao_Credito)


fancyRpartPlot(mytree)

mytree$variable.importance


credito_scoring$prob <-predict(mytree, newdata = credito_scoring, type="prob")
credito_scoring$classificacao <-predict(mytree, newdata = credito_scoring, type="class")

tabela <-table(credito_scoring$Rank_credito, credito_scoring$classificacao)
tabela

acuracia <- (tabela[1]+tabela[4])/sum(tabela)
acuracia

confusionMatrix(factor(credito_scoring$Rank_credito),
                factor(credito_scoring$classificacao))




### Treinamento e teste
treinamento <-createDataPartition(credito_scoring$ID, p=.7, list=F, times=1)
head(treinamento)

amostra_trn <-credito_scoring[treinamento,]
amostra_trn

amostra_teste <-credito_scoring[-treinamento,]
amostra_teste 

# modelo TRN
mytree1 <- rpart(Rank_credito ~  Cargo+ Pagamento+Faixa_Etaria+Cartao_Credito,
                data = amostra_trn,
                method = "class",
                parms = list(split = 'gini'),
                minsplit =2,
                minbucket = 1)

mytree1

fancyRpartPlot(mytree1)


amostra_trn$prob <-predict(mytree1, newdata = amostra_trn, type="prob")
amostra_trn$classificacao <-predict(mytree1, newdata = amostra_trn, type="class")

tabela1 <-table(amostra_trn$Rank_credito, amostra_trn$classificacao)
tabela1

acuracia1 <- (tabela1[1]+tabela1[4])/sum(tabela1)
acuracia1


###  Aplicar no amostra Teste
amostra_teste$prob <-predict(mytree1, newdata = amostra_teste, type="prob")
amostra_teste$classificacao <-predict(mytree1, newdata = amostra_teste, type="class")


View(amostra_teste)
tabela11 <-table(amostra_teste$Rank_credito, amostra_teste$classificacao)
tabela11

acuracia11 <- (tabela11[1]+tabela11[4])/sum(tabela11)
acuracia11
