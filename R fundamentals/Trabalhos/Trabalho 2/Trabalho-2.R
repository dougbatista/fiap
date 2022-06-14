library(readxl)
Consolidado <- read_excel("consolidado.xlsx")
View(Consolidado)

str(Consolidado)

summary(Consolidado)

dadosquant=subset(Consolidado,select=c(TempodeServiço, EstadoCivil, NumerodeFilhos, 
                                       TempodeResidencia, 
                                       salario, QtdaParcelas, 
                                       ValorEmprestimo, QtdaPagas))
options(scipen = 999)

summary(dadosquant)

length(dadosquant$TempodeServiço)

mc = cor(dadosquant);mc

mc

# QtddaParcelas - QtdaPagas - Valor Emprestimo

brkTempoServico <- seq(min(dadosquant$TempodeServiço),max(dadosquant$TempodeServiço), 5)
brkTempoServico

brkNumFilhos <- seq(min(dadosquant$NumerodeFilhos), max(dadosquant$NumerodeFilhos), 2)
brkNumFilhos

brkTempResidencia = seq(min(dadosquant$TempodeResidencia),max(dadosquant$TempodeResidencia), 10)

brkQtParcelas <- seq(min(dadosquant$QtdaParcelas),max(dadosquant$QtdaParcelas), 5)
brkQtParcelas

brkQtParcelasPagas <- seq(min(dadosquant$QtdaPagas),max(dadosquant$QtdaPagas), 5)
brkQtParcelasPagas

brkValorEmprestimo <- seq(min(dadosquant$ValorEmprestimo),max(dadosquant$ValorEmprestimo), 45)
brkValorEmprestimo


Consolidado$fx_tempoServico <- cut(Consolidado$TempodeServiço, breaks = brkTempoServico, 
                                   labels = c("[63-68)", "[68-73)", "[73-78)",
                                              "[78-83)", "[83-88)", "[88-93)", "[93-98]"),right = TRUE)

Consolidado$fx_qtParcelas <- cut(Consolidado$QtdaParcelas, breaks = brkQtParcelas, 
                                 labels = c("[14-19)", "[19-24)", 
                                            "[24-29)", "[29-34)", "[34-39)", 
                                            "[39-44]"), right = TRUE)

Consolidado$fx_qtParcelasPagas <- cut(Consolidado$QtdaPagas, breaks = brkQtParcelasPagas, 
                                      labels = c("[5-10)", "[10-15]"),right = TRUE)

Consolidado$fx_qtFilhos <- cut(Consolidado$NumerodeFilhos, breaks = brkNumFilhos, 
                               labels = c("[0-2)", "[2-4)", "[4-6]"),right = TRUE)


Consolidado$fx_tempResidencia <- cut(Consolidado$TempodeResidencia, breaks = brkTempResidencia, 
                                     labels = c("[0-10)", "[10-20)", "[20-30)", "[30-40)", "[40-50]"),right = TRUE)

Consolidado$fx_vlEmprestimo <- cut(Consolidado$ValorEmprestimo, breaks = brkValorEmprestimo, 
                                   labels = c("[490-535)", "[535-580)", "[580-625)", "[625-670)", 
                                              "[670-715)", "[715-760)", "[760-805)", "[805-850)", 
                                              "[850-895)", "[895-940)", "[940-985)", "[985-1030)", 
                                              "[1030-1075)", "[1075-1120)", "[1120-1165)", "[1165-1210)", 
                                              "[1210-1255)", "[1255-1300)", "[1300-1345)", "[1345-1390)", "[1390-1435)",
                                              "[1435-1480)","[1480-1525)","[1525-1570]"),right = TRUE)


# Checagem do CHISQ

table(Consolidado$fx_tempoServico)
table(Consolidado$fx_qtParcelas)
table(Consolidado$fx_qtParcelasPagas)
table(Consolidado$fx_qtFilhos)
table(Consolidado$fx_tempResidencia)
table(Consolidado$fx_vlEmprestimo)

tab1 = table(Consolidado$fx_tempoServico, Consolidado$default)
tab1
chq1 = chisq.test(tab1)
chq1

tab2 = table(Consolidado$fx_qtParcelas, Consolidado$default)
tab2
chq2 = chisq.test(tab2)
chq2

tab3 = table(Consolidado$fx_qtParcelasPagas, Consolidado$default)
tab3
chq3 = chisq.test(tab3)
chq3

tab4 = table(Consolidado$fx_qtFilhos, Consolidado$default)
tab4
chq4 = chisq.test(tab4)
chq4

tab5 = table(Consolidado$fx_tempResidencia, Consolidado$default)
tab5
chq5 = chisq.test(tab5)
chq5

tab6 = table(Consolidado$fx_vlEmprestimo, Consolidado$default)
tab6
chq6 = chisq.test(tab6)
chq6



## Modelos

install.packages(c("caret","rpart","rattle"))
library(caret)
library(rpart)
library(rattle)


## Treinamento e teste
treinamento <-createDataPartition(Consolidado$ID, p=.7, list=F, times=1)
head(treinamento)

amostra_trn <- Consolidado[treinamento,]
amostra_trn

amostra_teste <- Consolidado[-treinamento,]
amostra_teste

# modelo TRN

# salario + fx_tempResidencia + fx_vlEmprestimo + fx_qtParcelasPagas
mytree1 <- rpart(default ~  fx_qtParcelasPagas + fx_qtParcelas + fx_tempoServico,
                 data = amostra_trn,
                 method = "class",
                 parms = list(split = 'gini'),
                 minsplit =2,
                 minbucket = 1)

mytree1

fancyRpartPlot(mytree1)

mytree2 <- rpart(default ~  fx_qtFilhos + fx_tempResidencia + fx_vlEmprestimo,
                data = amostra_trn,
                method = "class",
                parms = list(split = 'gini'),
                minsplit =2,
                minbucket = 1)

mytree2

fancyRpartPlot(mytree2)

mytree3 <- rpart(default ~  fx_qtFilhos + fx_tempResidencia + fx_vlEmprestimo + fx_qtParcelasPagas,
                 data = amostra_trn,
                 method = "class",
                 parms = list(split = 'gini'),
                 minsplit =2,
                 minbucket = 1)

fancyRpartPlot(mytree3)

mytree4 <- rpart(default ~ fx_tempResidencia + fx_vlEmprestimo + fx_qtParcelasPagas,
                 data = amostra_trn,
                 method = "class",
                 parms = list(split = 'gini'),
                 minsplit =2,
                 minbucket = 1)

fancyRpartPlot(mytree4)

mytree5 <- rpart(default ~ salario + fx_tempResidencia + fx_vlEmprestimo + fx_qtParcelasPagas,
                 data = amostra_trn,
                 method = "class",
                 parms = list(split = 'gini'),
                 minsplit =2,
                 minbucket = 1)
mytree5
fancyRpartPlot(mytree5)

mytree6 <- rpart(default ~  fx_tempResidencia + fx_vlEmprestimo + fx_qtParcelasPagas + fx_qtParcelas + fx_tempoServico,
                 data = amostra_trn,
                 method = "class",
                 parms = list(split = 'gini'),
                 minsplit =2,
                 minbucket = 1)

fancyRpartPlot(mytree6)


#Usando o modelo treinado

#Teste1
amostra_teste$prob <-predict(mytree1, newdata = amostra_teste, type="prob")
amostra_teste$classificacao <-predict(mytree1, newdata = amostra_teste, type="class")

tabela1 <-table(amostra_teste$default, amostra_teste$classificacao)
tabela1

View(amostra_teste)

confusionMatrix(factor(amostra_teste$default),
                factor(amostra_teste$classificacao))

#Teste2
amostra_teste$prob2 <-predict(mytree2, newdata = amostra_teste, type="prob")
amostra_teste$classificacao2 <-predict(mytree2, newdata = amostra_teste, type="class")

tabela2 <-table(amostra_teste$default, amostra_teste$classificacao2)
tabela2

View(amostra_teste)

confusionMatrix(factor(amostra_teste$default),
                factor(amostra_teste$classificacao2))

amostra_trn$prob2 <-predict(mytree2, newdata = amostra_trn, type="prob")
amostra_trn$classificacao2 <-predict(mytree2, newdata = amostra_trn, type="class")

confusionMatrix(factor(amostra_trn$default),
                factor(amostra_trn$classificacao2))

#Teste3

amostra_teste$prob3 <-predict(mytree3, newdata = amostra_teste, type="prob")
amostra_teste$classificacao3 <-predict(mytree3, newdata = amostra_teste, type="class")

tabela3 <-table(amostra_teste$default, amostra_teste$classificacao3)
tabela3

View(amostra_teste)

confusionMatrix(factor(amostra_teste$default),
                factor(amostra_teste$classificacao3))

#Teste4

amostra_teste$prob4 <-predict(mytree4, newdata = amostra_teste, type="prob")
amostra_teste$classificacao4 <-predict(mytree4, newdata = amostra_teste, type="class")

tabela4 <-table(amostra_teste$default, amostra_teste$classificacao4)
tabela4

View(amostra_teste)

confusionMatrix(factor(amostra_teste$default),
                factor(amostra_teste$classificacao4))

#Teste5

amostra_teste$prob5 <-predict(mytree5, newdata = amostra_teste, type="prob")
amostra_teste$classificacao5 <-predict(mytree5, newdata = amostra_teste, type="class")

tabela5 <-table(amostra_teste$default, amostra_teste$classificacao5)
tabela5

View(amostra_teste)

confusionMatrix(factor(amostra_teste$default),
                factor(amostra_teste$classificacao5))

#Teste6

amostra_teste$prob6 <-predict(mytree6, newdata = amostra_teste, type="prob")
amostra_teste$classificacao6 <-predict(mytree6, newdata = amostra_teste, type="class")

tabela6 <-table(amostra_teste$default, amostra_teste$classificacao6)
tabela6

View(amostra_teste)

confusionMatrix(factor(amostra_teste$default),
                factor(amostra_teste$classificacao6))
