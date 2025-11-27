#AULA05-Inca.R
#Minicurso - Análise de sobrevivência
#Universidade federal fluminense
#Instituto de matemática e estatística
#Bacharelado em estatística
#Minicurso: Análise de sobrevivência
#Semestre 2024.1
#Professora: Núbia Karla
#Programador: Pedro Frazão Dutra 
#Carregamento de pacotes necessários
require(tidyverse)
require(survival)
library(survminer)


###########################################################
## Carregamentos dos dados
inca <- read.csv2(file = "inca.csv")

## Renomeação de variáveis
inca <- inca |> rename(inicio = DATAINITRT,
                       fim = DATAOBITO)

## Transformando colunas em data
inca <- inca |> 
  mutate(
    inicio = as.Date(inicio, format = "%d/%m/%Y"),
    fim = as.Date(fim, format = "%d/%m/%Y")
  )

## Criando variável status
inca <- inca |> mutate(status = ifelse(is.na(fim),0,1))

## Trocando NA's pela data de terminação do estudo 
inca$fim[is.na(inca$fim) == TRUE] = as.Date("2023-02-23")

## Criando variável tempo 
inca <- inca |> 
  mutate(tempo = as.numeric(fim - inicio))

## Categorizando as idades
inca <- inca |> mutate(faixa_etaria = if_else(IDADE < 60, "20-59", "60+"))

## Categorizando o estadiamento
inca <- inca |> mutate(estadiamento = case_when(
  ESTADIAM == "0" ~ 0,
  ESTADIAM == "1" ~ 1,
  ESTADIAM == "1A" ~ 1,
  ESTADIAM == "1B" ~ 1,
  ESTADIAM == "2" ~ 2,
  ESTADIAM == "2A" ~ 2,
  ESTADIAM == "2B" ~ 2,
  ESTADIAM == "3" ~ 3,
  ESTADIAM == "3B" ~ 3,
  ESTADIAM == "3C" ~ 3,
  ESTADIAM == "4" ~ 4,
  ESTADIAM == "4A" ~ 4,
  ESTADIAM == "4B" ~ 4,
  ESTADIAM == "4C" ~ 4))


###########################################################

## Rodando o modelo Km estratificado por sexo
y <- Surv(inca$tempo,inca$status)
km1 <- survfit(y~SEXO, data = inca)

## Obtendo os valores da função de aalen segundo sexo
aalen1 <- survfit(coxph(y~strata(inca$SEXO), data =  inca))

## Curva de sobrevivência estratificada por sexo
graf1 <- ggsurvplot(km1, censor = F,
                  xlab = "Tempo (dias)", ylab = "S(t)",
                  ggtheme = theme_classic(),
                  title = "Sobrevivência segundo sexo",
                  legend.labs = c("Masculino", "Feminino"),
                  font.tickslab = c(11, "plain", "black"),
                  legend.labs.fontsize = 12,
                  font.x = c(12,"black"),
                  font.y = c(12,"black"),
                  font.title = c(14,"bold","black"))
                  

graf1 <- graf1$plot + 
  scale_x_continuous(breaks = seq(0,774,75)) +
  scale_y_continuous(breaks = seq(0,1,0.05),
                     limits = c(0.8,1)) +
  scale_color_manual(values = c("Masculino" = "#00a0b0", 
                                "Feminino" = "#cc333f"))+
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5))

graf1

## Curva de risco acumulado estratificada por sexo

graf2 <- ggsurvplot(aalen1, data = inca , fun  = "cumhaz", censor = F,
                   xlab = "Tempo (dias)", ylab = "Λ(t)",
                   ggtheme = theme_classic(),
                   title = "Risco acumulado segundo sexo",
                   legend.labs = c("Masculino", "Feminino"),
                   font.tickslab = c(11, "plain", "black"),
                   legend.labs.fontsize = 12,
                   font.x = c(12,"black"),
                   font.y = c(12,"black"),
                   font.title = c(14,"bold","black"))

graf2 <- graf2$plot + 
  scale_x_continuous(breaks = seq(0,774,75)) +
  scale_y_continuous(breaks = seq(0,4,0.02)) +
  scale_color_manual(values = c("Masculino" = "#00a0b0", 
                                "Feminino" = "#cc333f"))+
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5))

graf2

## Teste de log-rank para sexo

logrank1 <- survdiff(y~inca$SEXO, data = inca)
logrank1

# p < 0.05; portanto há evidências estatísticas de que as
# curvas de sobrevida são diferentes.

###########################################################

## Rodando o modelo Km estratificado por faixa etária
km2 <- survfit(y~inca$faixa_etaria, data = inca)

## Obtendo a função de aalen segundo faixa-etária 
aalen2 <- survfit(coxph(y~strata(inca$faixa_etaria), data =  inca))

## Curva de sobrevivência segundo faixa etária
graf3 <- ggsurvplot(km2, censor = F,
                  xlab = "Tempo (dias)", ylab = "S(t)",
                  ggtheme = theme_classic(), surv.median.line = "hv",
                  title = "Sobrevivência segundo faixa etária",
                  legend.labs = c("20-59 anos", "+60 anos"),
                  font.tickslab = c(11, "plain", "black"),
                  font.x = c(12,"black"),
                  font.y = c(12,"black"),
                  font.title = c(14,"bold","black"))
graf3 <- graf3$plot + 
  scale_x_continuous(breaks = seq(0,774,75)) +
  scale_y_continuous(breaks = seq(0,1,0.05),
                     limits = c(0.8,1)) +
  scale_color_manual(values = c("20-59 anos" = "#00a0b0", 
                                "+60 anos" = "#cc333f")) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5))

graf3

## Curva de risco acumulado estratificada por faixa etária
graf4 <- ggsurvplot(aalen2,data = inca, fun = "cumhaz", censor = F,
                    xlab = "Tempo (dias)", ylab = "Λ(t)",
                    ggtheme = theme_classic(), surv.median.line = "hv",
                    title = "Risco acumulado segundo faixa etária",
                    legend.labs = c("20-59 anos", "+60 anos"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(12,"black"),
                    font.y = c(12,"black"),
                    font.title = c(14,"bold","black"))
graf4 <- graf4$plot + 
  scale_x_continuous(breaks = seq(0,774,75)) +
  scale_y_continuous(breaks = seq(0,4,0.02)) +
  scale_color_manual(values = c("20-59 anos" = "#00a0b0", 
                                "+60 anos" = "#cc333f")) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5))

graf4

## Teste de log-rank para faixa etária

logrank2 <- survdiff(y~inca$faixa_etaria, data = inca)
logrank2

###########################################################

## Rodando modelo Km estratificado por tabagismo
km3 <- survfit(y~inca$TABAGISM, data = inca)

## Função de aalen segundo tabagismo
aalen3 <- survfit(coxph(y~strata(inca$TABAGISM), data =  inca))

## Curva de sobrevivência segundo tabagismo
graf5 <- ggsurvplot(km3, censor = F,
                   xlab = "Tempo (dias)", ylab = "S(t)",
                   ggtheme = theme_classic2(),
                   title = "Sobrevivência segundo tabagismo",
                   legend.labs = c("nunca","ex-fumante",
                                   "sim"),
                   font.tickslab = c(11, "plain", "black"),
                   font.x = c(12,"black"),
                   font.y = c(12,"black"),
                   font.title = c(14, "bold", "black"))
graf5 <- graf5$plot + 
  scale_x_continuous(breaks = seq(0,774,75)) +
  scale_y_continuous(breaks = seq(0.7,1,0.05),
                     limits = c(0.7,1)) +
  scale_color_manual(values = c("nunca" = "#020305", 
                                "ex-fumante" = "#ec225e",
                                "sim" = "#269199")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"), 
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5))

graf5

## Curva de risco acumulado estratificada por tabagismo

graf6 <- ggsurvplot(aalen3,data = inca, fun = "cumhaz", censor = F,
                    xlab = "Tempo (dias)", ylab = "Λ(t)",
                    ggtheme = theme_classic2(),
                    title = "Risco acumulado segundo tabagismo",
                    legend.labs = c("nunca","ex-fumante",
                                    "sim"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(12,"black"),
                    font.y = c(12,"black"),
                    font.title = c(14, "bold", "black"))
graf6 <- graf6$plot + 
  scale_x_continuous(breaks = seq(0,774,75)) +
  scale_y_continuous(breaks = seq(0,3,0.02)) +
  scale_color_manual(values = c("nunca" = "#020305", 
                                "ex-fumante" = "#ec225e",
                                "sim" = "#269199")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"), 
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5))

graf6

## Teste de log-rank para tabagismo

logrank3 <- survdiff(y~inca$faixa_etaria, data = inca)
logrank3

###########################################################

## Rodando modelo km estratificado por alcoolismo
km4 <- survfit(y~inca$ALCOOLIS, data  =  inca)

## Obtendo a função de aalen segundo o alcoolismo
aalen4 <- survfit(coxph(y~strata(inca$ALCOOLIS), data =  inca))

##Curva de sobrevivência segundo alcolismo
graf7 <- ggsurvplot(km4, censor = F,
                   xlab = "Tempo (dias)", ylab = "S(t)",
                   ggtheme = theme_classic2(),
                   title = "Sobrevivência segundo alcoolismo",
                   legend.labs = c("nunca","ex-consumidor",
                                   "sim"),
                   font.tickslab = c(11, "plain", "black"),
                   font.x = c(12,"black"),
                   font.y = c(12,"black"),
                   font.title = c(14, "bold", "black"))
graf7 <- graf7$plot + 
  scale_x_continuous(breaks = seq(0,774,75)) +
  scale_y_continuous(breaks = seq(0,1,0.05),
                     limits = c(0.8,1)) +
  scale_color_manual(values = c("nunca" = "#020305", 
                                "ex-consumidor" = "#ec225e",
                                "sim" = "#269199")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"), 
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5))

graf7

## Curva de risco acumulado segundo alcoolismo 

graf8 <- ggsurvplot(aalen4, data = inca, fun = "cumhaz", censor = F,
                   xlab = "Tempo (dias)", ylab = "Λ(t)",
                   ggtheme = theme_classic2(),
                   title = "Risco acumulado segundo alcoolismo",
                   legend.labs = c("nunca","ex-consumidor",
                                   "sim"),
                   font.tickslab = c(11, "plain", "black"),
                   font.x = c(12,"black"),
                   font.y = c(12,"black"),
                   font.title = c(14, "bold", "black"))
graf8 <- graf8$plot + 
  scale_x_continuous(breaks = seq(0,774,75)) +
  scale_y_continuous(breaks = seq(0,5,0.02)) +
  scale_color_manual(values = c("nunca" = "#020305", 
                                "ex-consumidor" = "#ec225e",
                                "sim" = "#269199")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"), 
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5))

graf8

## Teste de log-rank para alcoolismo

logrank4 <- survdiff(y~inca$ALCOOLIS, data = inca)
logrank4

###########################################################

## Rodando modelo km estratificado por estadiamento
km5 <- survfit(y~inca$estadiamento, data = inca)

## Obtendo a função de aalen segundo o estadiamento
aalen5 <- survfit(coxph(y~strata(inca$estadiamento), data =  inca))

## Curva de sobrevivência segundo estadiamento
graf9 <- ggsurvplot(km5, censor = F,
                   xlab = "Tempo (dias)", ylab = "S(t)",
                   ggtheme = theme_classic(),
                   title = "Sobrevivência segundo estadiamento clínico do tumor",
                   legend.title = "Estadiamento",
                   legend.labs = c("0","1",
                                   "2","3","4"),
                   font.tickslab = c(11, "plain", "black"),
                   font.x = c(12,"black"),
                   font.y = c(12,"black"),
                   font.title = c(14, "bold", "black"))
graf9 <- graf9$plot + 
  scale_x_continuous(breaks = seq(0,774,75)) +
  scale_y_continuous(breaks = seq(0,1,0.05),
                     limits = c(0.7,1)) +
  scale_color_manual(values = c("0" = "#020305", 
                                "1" = "#ec225e",
                                "2" = "#269199",
                                "3" = "#eb9961",
                                "4" = "gray60")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"), 
    legend.title = element_text(size = 12, color = "black"),
    legend.key = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5))

graf9

## Gráfico de risco acumulado segundo estadiamento 
graf10 <- ggsurvplot(aalen5, data = inca, fun = "cumhaz", censor = F,
                    xlab = "Tempo (dias)", ylab = "Λ(t)",
                    ggtheme = theme_classic(),
                    legend.title = "Estadiamento",
                    legend.labs = c("0","1","2","3","4"),
                    title = "Risco acumulado segundo estadiamento clínico do tumor",
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(12,"black"),
                    font.y = c(12,"black"),
                    font.title = c(14, "bold", "black"))
graf10 <- graf10$plot + 
  scale_x_continuous(breaks = seq(0,774,75)) +
  scale_y_continuous(breaks = seq(0,1,0.05)) +
  scale_color_manual(values = c("0" = "#020305", 
                                "1" = "#ec225e",
                                "2" = "#269199",
                                "3" = "#eb9961",
                                "4" = "gray60")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"), 
    legend.title = element_text(size = 12, color = "black"),
    legend.key = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5))

graf10

## Teste de log-rank para estadiamento

logrank5 <- survdiff(y~inca$estadiamento, data = inca)
logrank5

## Terminação
