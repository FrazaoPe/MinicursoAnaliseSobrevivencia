#AULA04-Ipec.R
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
ipec = read_csv2(file = "ipec.csv")

## Rodando o modelo Km
y <- Surv(ipec$tempo,ipec$status)
km <- survfit(y~1, data = ipec)

## Gráfico da sobrevivência
graf = ggsurvplot(km, censor = F, conf.int = F, break.time.by = 100,
                  color = "#017530", xlab = "Tempo (dias)", ylab = "S(t)",
                  ggtheme = theme_bw(), surv.median.line = "hv")

graf$plot + 
scale_x_continuous(breaks = seq(0,3228,250)) +
scale_y_continuous(breaks = seq(0,1,0.25))

## Rodando o modelo Nelson-Aalen
nelson <- survfit(coxph(y~1,data = ipec))

## Gráfico de risco acumulado
graf1 = ggsurvplot(nelson,censor = F, data = ipec, fun = "cumhaz", conf.int = F,
                   break.time.by = 50, color = "red", xlab = "Tempo (dias)", ylab = "Λ(t)",
                   ggtheme = theme_bw())

graf1$plot +
  scale_x_continuous(breaks = seq(0,3228,250)) +
  scale_y_continuous(limits = c(0,1.5), breaks = seq(0,1.5,0.25))
