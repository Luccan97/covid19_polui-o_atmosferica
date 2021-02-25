setwd("C:\\GISA_Lucca\\Projetos_Lucca_GISA\\Outros\\Monitoria_covid19\\cor")

library(readxl)
library(dplyr)
library(corrplot)
library(plotly)
library(tidyr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
Dados <- read.csv("tabela_principal.csv", sep = ';', dec = ',')


Dados2 <- Dados[,c('cardiopatia','diabetes', 'pm25', 'mm_pm25', 'pm10', 'mm_pm10', 'temperatura', 'umidade', 'mm_card')]

# Gráfico de Correlação entre todas as variáveis!

cor <- cor(Dados2, use = "pairwise.complete.obs")

corrplot(cor, method = 'number', bg ="grey", cl.pos = "b", tl.srt = 40, 
         addgrid.col = 'lightgrey', tl.col = 'black')


# Gráfico de Dispersão (casos cardiopatia x pm10)

my_screen_step1 <- split.screen(c(2, 1))

# I add one graph on the screen number 1 which is on top :
screen(my_screen_step1[1])
plot(Dados$cardiopatia, Dados$mm_pm10, pch=20 ,ylab = "",
     xlab="", main = "Gráfico de Dispersão: valores da média móvel de pm 10 e número de casos de COVID-19 com cardiopatia",
     cex=3 , col=rgb(0.4,0.9,0.8,0.5) ) 

# I divide the second screen in 2 columns :
my_screen_step2 <- split.screen(c(1, 2), screen = my_screen_step1[2])
screen(my_screen_step2[1])
hist(Dados2$Dados.cardiopatia, border = F , col=rgb(0.2,0.2,0.8,0.7) , main="" , xlab=" Distribuição do número de casos COVID-19 com cardiopatia por dia")
screen(my_screen_step2[2])
hist(Dados2$Dados.mm_pm10, border=F , col=rgb(0.8,0.2,0.8,0.7) , main="" ,  xlab="Distribuição da Média Móvel Material Partículado 10 por dia")

#Adicionando coluna do mês

Dados[,12] <- month(Dados$data, abbr = T, label = T)
Dados <- rename(Dados, 'Mês' = V12)



## Série temporal material particulado 10 e média móvel
str(Dados)

spread

Dados$data <- dmy(Dados$data)

g <- ggplot(Dados, aes(x = data)) + 
  geom_line(aes(y = pm10, color = 'pm 10'), linetype = 'dotdash', size = 0.5) +
  geom_line(aes(y = mm_pm10, color = 'média móvel semanal pm 10'), size = 0.9)
 
g + labs(color = '', x = '', y = ("μg/m³")) + 
  scale_x_date(date_labels = '%d %b', date_breaks = '2 week') + theme_gray() 

## Série temporal material particulado 10 e média móvel
str(Dados)

spread

Dados$data <- dmy(Dados$data)

g2<- ggplot(Dados, aes(x = data)) + 
  geom_line(aes(y = cardiopatia, color = 'casos de cardiopatia'), linetype = 'dotdash', size = 0.5) +
  geom_line(aes(y = mm_card, color = 'média móvel semanal casos cardiopatia'), size = 0.9)

g2 + labs(color = '', x = '', y = ("número de casos")) + 
  scale_x_date(date_labels = '%d %b', date_breaks = '2 week') + theme_gray() 

## Série temporal material particulado 2.5 e média móvel

g3<- ggplot(Dados, aes(x = data)) + 
  geom_line(aes(y = pm25, color = 'pm 2.5'), linetype = 'dotdash', size = 0.5) +
  geom_line(aes(y = mm_pm25, color = 'média móvel semanal pm 2.5'), size = 0.9)

g3 + labs(color = '', x = '', y = ("μg/m³")) + 
  scale_x_date(date_labels = '%d %b', date_breaks = '2 week') + theme_gray() 



#### Serie temporal 2 variáveis

coeffg4 <- 2.5

pm10Color <- "coral4"
cardColor <- 'cyan4'

g4 <- ggplot(Dados, aes(x=data)) +
  
  geom_line( aes(y= mm_pm10), size=2, color= pm10Color) + 
  geom_line( aes(y= mm_card / coeffg4), size=2, color = cardColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "média móvel dos valores de pm 10 na atmosfera (μg/m³) ",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name=" média móvel dos casos de COVID-19 com cardiopatia")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = pm10Color, size=13),
    axis.title.y.right = element_text(color = cardColor, size=13)
  ) + ggtitle("Poluição atmosférica e morbidade (cardiopatia) em casos de COVID-19") +
  scale_x_date(date_labels = '%d %b', date_breaks = '2 week')
  
coeffg5 <- 5
g5 <- ggplot(Dados, aes(x=data)) +
  
  geom_line( aes(y= mm_pm25), size=2, color= pm10Color) + 
  geom_line( aes(y= mm_card / coeffg5), size=2, color = cardColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "média móvel dos valores de pm 2.5 na atmosfera (μg/m³) ",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name=" média móvel dos casos de COVID-19 com cardiopatia")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = pm10Color, size=13),
    axis.title.y.right = element_text(color = cardColor, size=13)
  ) + ggtitle("Poluição atmosférica e morbidade (cardiopatia) em casos de COVID-19") +
  scale_x_date(date_labels = '%d %b', date_breaks = '2 week')



#### Gráfico de dispersão x Correlação


g6 <- ggplot(Dados, aes(x= mm_pm10, y= mm_card)) +
  geom_point(aes(color = Mês, size = 1.2)) +
  geom_smooth(method=lm , color="black", se=F) +
  theme_ipsum() + labs(x='poluição atmosférica pm 10 (μg/m³)', y = 'casos covid-19 com cardiopatia', 
                       title  = 'Correlação de Pearson entre pm10 e casos cardiopatia = 0.49 / Intervalo de Confiança: 0.37 - 0.59')
g6 + scale_color_manual(values = c("red", "orange","yellow", "green", "violet", "blue", "grey"))

cor.test(Dados$mm_pm10, Dados$mm_card)

g7 <- ggplot(Dados, aes(x= mm_pm25, y= mm_card)) +
  geom_point(aes(color = Mês, size = 1.2)) +
  geom_smooth(method=lm , color = 'black', se=F) +
  theme_ipsum() + labs(x='poluição atmosférica pm 2.5 (μg/m³)', y = 'casos covid-19 com cardiopatia', 
                       title  = 'Correlação de Pearson entre pm 2.5 e casos cardiopatia = 0.42 / Intervalo de Confiança: 0.30 - 0.50') 
  
g7 + scale_color_manual(values = c("red", "orange","yellow", "green", "violet", "blue", "grey"))

cor.test(Dados$mm_pm25, Dados$mm_card)

g5

ggplotly(g5)

g7

dados_c_mar <- filter(Dados, Mês == 'mar')

cor.test(dados_c_mar, mm_pm10, mm_card)
