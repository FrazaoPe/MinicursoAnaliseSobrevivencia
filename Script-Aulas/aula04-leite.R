#AULA04-Leite.R
#Minicurso - Análise de sobrevivência
#Universidade federal fluminense
#Instituto de matemática e estatística
#Bacharelado em estatística
#Minicurso: Análise de sobrevivência
#Semestre 2024.2
#Professora: Núbia Karla
#Programadores: Pedro Frazão Dutra e Lucas Oliveira
#Carregamento de pacotes necessários
library(tidyverse)
library(survival)
library(survminer)

## Carregamento dos dados 
leite = read_table(file = "leite.txt")

## Rodando o modelo Km
y <- Surv(leite$tempo,leite$status)
km <- survfit(y~1, data = leite)

## Gráficos de sobrevivência e risco acumulado
graf = ggsurvplot(km, censor = F, conf.int = F, break.time.by = 100,
                  color = "#017530", xlab = "Tempo (meses)", ylab = "S(t)",
                  ggtheme = theme_bw(), surv.median.line = "hv")

graf$plot + 
  scale_x_continuous(breaks = seq(0,13,1)) +
  scale_y_continuous(breaks = seq(0,1,0.20))


## Rodando o modelo Nelson-Aalen
nelson <- survfit(coxph(y~1,data = pneu))

## Gráfico de risco
graf1 = ggsurvplot(nelson,censor = F, data = pneu, fun = "cumhaz", conf.int = F,
           break.time.by = 50, color = "red", xlab = "Tempo (dias)", ylab = "Λ(t)",
           ggtheme = theme_bw())

graf1$plot +
  scale_x_continuous(breaks = seq(0,13,1)) +
  scale_y_continuous(limits = c(0,3.5), breaks = seq(0,4,0.5))
