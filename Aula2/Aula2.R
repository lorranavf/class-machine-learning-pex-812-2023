
### Aprendizado de Máquinas - aula prática 2

library(readxl)

dados1<-read_excel('aula1.xlsx') # dados de clientes de uma loja
head(dados1)

######### 

# Valores discrepantes (outliers)

# 1) (distribuições simétricas)

z<-scale(RNDTOT)
summary(z)

zo<-abs(z)>=3
head(zo)

sum(zo, na.rm=TRUE) # número de outliers 

# 2) 

summary(RNDTOT)
out<- RNDTOT >= 10984
sum(out, na.rm = TRUE) # número de outliers

# ou 

boxplot(RNDTOT)$out

####### Missing values (NA's)

library(ggplot2)
library(gridExtra)
library(VIM)

aggr(dados1, numbers = TRUE, prop = c(TRUE, FALSE))

# Outro exemplo - base de crédito

# Leitura da base de dados

base<- read.csv('aula2.csv')
head(base)
summary(base)

# Apaga a coluna cliente

base$cliente = NULL

# Valores inconsistentes

base$idade<- ifelse(base$idade < 0,NA, base$idade)
summary(base)

# Valores faltantes (NA's)

aggr(base, numbers = TRUE, prop = c(TRUE, FALSE))

# imputação média e mediana

base1<-base
base2<-base
base3<-base
base4<-base

mean(base$idade, na.rm = TRUE)
median(base$idade, na.rm = TRUE)

base1$idade = ifelse(is.na(base1$idade), mean(base1$idade, na.rm = TRUE), base1$idade)

base2$idade = ifelse(is.na(base2$idade), median(base2$idade, na.rm = TRUE), base2$idade)

base3<- kNN(base3, variable = 'idade', k = 5) # kNN

library(missForest)

base41 <- data.frame(
  original = base$idade,
  imputed_missForest = missForest(base)$ximp$idade) # RF
base41

library(dplyr)

base41 <- rename(base41, idade = imputed_missForest)
base41

base4<-select(base4, -'idade')
head(base4)

idade<-base41$idade

base41<-data.frame(base4,idade)
head(base41)


# outro método irmi (iterative robust model based imputation - método multivariado)

base5<-base

base5<- irmi(base5)
head(base5)

# final

mean(base1$idade)
mean(base2$idade)
mean(base3$idade)
mean(base41$idade)
mean(base5$idade)

# Exercício: Fazer a imputação para variável RESID da base de dados da aula 1.

### Transformações nas variáveis

rm=list(ls())

ggplot(base, aes(x = emprestimo)) +
  geom_histogram(bins = 30, fill = 'lightblue', colour = 'darkblue')

boxplot(base$emprestimo)$out

library(EnvStats)

emprestimobc<-boxcox(base$emprestimo)
emprestimobc

# PPCC - Probability plot correlation coefficient

# vamos considerar a transformação log

base$emprestimo= base$emprestimo = log(base$emprestimo)
head(base)

ggplot(base, aes(x = emprestimo)) +
  geom_histogram(bins = 30, fill = 'lightblue', colour = 'darkblue')

# outras transformações

j<-nchar(max(base$renda))
rendat<-(base$renda)/(10^j)
head(rendat)

# transformação baseado na amplitude interquartílica

q1<-summary(base$renda)[2]
q3<-summary(base$renda)[5]

rendat2<- (base$renda - q3)/(q3 - q1)

# normalização

rendat3<- scale(base$renda)

# histogramas 

basenova<-data.frame(base,rendat,rendat2,rendat3)

ggplot(base, aes(x = renda)) +
  geom_histogram(bins = 30, fill = 'lightblue', colour = 'darkblue')

ggplot(base, aes(x = rendat)) +
  geom_histogram(bins = 30, fill = 'lightblue', colour = 'darkblue')

ggplot(base, aes(x = rendat2)) +
  geom_histogram(bins = 30, fill = 'lightblue', colour = 'darkblue')

ggplot(base, aes(x = rendat3)) +
  geom_histogram(bins = 30, fill = 'lightblue', colour = 'darkblue')


# Exercício: Fazer as transformações com a variável TMPRSD da base de dados aula 1


#### Categorização de uma variável quantitativa

head(base$renda)
amplitude<-max(base$renda) - min(base$renda)
amplitude

fxrenda<- cut(base$renda, breaks = 4, dig.lab = 5) # cinco dígitos
table(fxrenda)

quartilrenda<-cut(base$renda, 
                  breaks = c(quantile(base$renda,0),
                            quantile(base$renda,0.25),
                            quantile(base$renda,0.5),
                            quantile(base$renda,0.75),
                            quantile(base$renda,1),
                            inclue.lowest = T, dig.lab = 5))
table(quartilrenda) # será que é interessante?

catrenda<-cut(base$renda, breaks = c(0,40000,60000,max(base$renda)),
              labels = c("Baixa","Média","Alta"),include.lowest = T,dig.lab = 5)

table(catrenda)

