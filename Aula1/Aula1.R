### Aprendizado de Máquinas - aula prática 1

# Análise exploratória de dados

# Análise univariada - variável qualitativa

library(readxl)

dados1<-read_excel('aula1.xlsx') # dados de clientes de uma loja

head(dados1)
tail(dados1)
summary(dados1) # resumo dos dados

library(dplyr)

glimpse(dados1) # tipos dos dados

attach(dados1)
table(UNIFED) # tabela - unidades da federação
prop.table(table(UNIFED))

library(ggplot2)

ggplot(dados1, aes(x = UNIFED)) + geom_bar() # gráfico de barras

# correção na variável UNIFED

UNIFED[UNIFED =="S.P."]="SP"
UNIFED[UNIFED == "BH"] = "MG"
table(UNIFED)

ggplot(dados1, aes(x = UNIFED)) + geom_bar()

# variável RESID - residência

table(RESID)

RESID[RESID =='pROP']='PROP'
table(RESID)

# variável RST - restrição creditícia

table(RST)

RST[RST=="2"]=NA
table(RST)

# Análise univariada - variável quantitativa

# variável RNDTOT - salários e outros rendimentos

summary(RNDTOT) # resumo

mean(RNDTOT) # média
median(RNDTOT) # mediana
sd(RNDTOT) # desvio padrão
var(RNDTOT) # variância

cv<-(sd(RNDTOT)/mean(RNDTOT))*100 # coeficiente de variação
cv

ggplot(dados1, aes(x = RNDTOT)) +
  geom_histogram(bins = 30, fill = "lightblue", color = 'darkblue')+
  ggtitle("Histograma com 30 classes") # histograma

ggplot(dados1, aes(x = RNDTOT)) +
  geom_boxplot(width = 0.3, outlier.color = "RED") +
  ggtitle('Box-plot da renda total') # gráfico box-plot

library(moments)
skewness(RNDTOT) # assimetria
kurtosis(RNDTOT) # curtose

# Análise bivariada

# duas variáveis qualitativas

prop.table(table(RESID,STATUS),1) # Status - bom ou ma pagador

plotdata<-as.data.frame.table(prop.table(table(RESID,STATUS),1))
plotdata

ggplot() + geom_bar(aes(y=Freq, x = RESID, fill = STATUS), data = plotdata,
                    stat = 'identity') +
  geom_text(data = plotdata, aes (x = RESID, y = Freq, label = paste0(round(Freq,2)*100,"%")),size = 4)

# Variável qualitativa e quantitativa

ggplot(dados1, aes(x = STATUS, y = RNDTOT)) + 
  geom_boxplot(with = 0.5, outlier.color = "RED") + 
  ggtitle("Box-plot da Renda Total por Status")

boxplot(RNDTOT ~ STATUS)$stats # valor do boxplot

# Duas variáveis quantitativas

# TMPRSD - tempo de residência # RNDTOT - renda total

ggplot(dados1, aes(x = TMPRSD, y = RNDTOT)) + geom_point() +
  geom_smooth(method = lm, se = FALSE) # ajusta uma reta aos pontos

cor(TMPRSD, RNDTOT) # não funciona, por que?

cor(TMPRSD, RNDTOT, use = 'complete') elimina valores faltantes

#gráficos com mais de duas variáveis

boxplot(RNDTOT ~STATUS + RESID, border = 'darkblue',
ylab = "Renda", xlab = "Status e tipo de residência", main = "")

# opcional

library(car)

scatterplot(RNDTOT ~ TMPRSD|STATUS, smoother = F, legend.coods = 'topright', boxplot = 'xy')

# gráfico de três variáveis quantitativas

library(psych)

pairs.panels(cbind(RNDTOT,IDADE,TMPRSD), smoother = FALSE)

# Outliers

renda_z<-scale(RNDTOT)
renda_z
summary(renda_z) 

boxplot(RNDTOT)$out # obtenção dos outliers

