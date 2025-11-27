#AULA05-Ipec.R
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
require(survminer)

## Carregamentos dos dados
ipec <- read_csv2(file = "ipec.csv")

## Criando variável "anos"
ipec <- ipec |>  mutate(
  anos = (tempo/365)
)

## Rodando o modelo Kaplan-Meier estratificado por sexo
y <- Surv(ipec$anos,ipec$status)
km1 <- survfit(y~ipec$sexo, data = ipec)

## Função de aalen para sexo
aalen1 <- survfit(y~strata(ipec$sexo), data = ipec)

## Curva de sobrevivência estratificada por sexo
graf1 <- ggsurvplot(km1, censor = F,
                   xlab = "Tempo (anos)", ylab = "S(t)",
                   ggtheme = theme_classic(),
                   title = "Função de sobrevivência dos sexos feminino e masculino",
                   legend.labs = c("feminino","masculino"),
                   font.tickslab = c(11, "plain", "black"),
                   font.x = c(12,"black"),
                   font.y = c(12,"black"),
                   font.title = c(14, "bold", "black"))
graf1 <- graf1$plot + 
  scale_x_continuous(breaks = seq(0,9,1),
                     limits = c(0,9)) +
  scale_y_continuous(breaks = seq(0,1,0.20)) +
  scale_color_manual(values = c("feminino" = "#ec225e", 
                                "masculino" = "black")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5))

graf1


## Curva de risco acumulado estratificada por sexo
graf2 <- ggsurvplot(aalen1, data = ipec, fun = "cumhaz", censor = F,
                    xlab = "Tempo (anos)", ylab = "Λ(t)",
                    ggtheme = theme_classic(),
                    title = "Risco acumulado segundo sexo",
                    legend.labs = c("feminino","masculino"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(12,"black"),
                    font.y = c(12,"black"),
                    font.title = c(14, "bold", "black"))
graf2 <- graf2$plot + 
  scale_x_continuous(breaks = seq(0,9,1),
                     limits = c(0,9)) +
  scale_y_continuous(breaks = seq(0,1.6,0.2)) +
  scale_color_manual(values = c("feminino" = "#ec225e", 
                                "masculino" = "black")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5))

graf2

## Teste de logrank para sexo
logrank1 <- survdiff(y~ipec$sexo, data = ipec)
logrank1

## Rodando o modelo kaplan-meier estratificado por escolaridade
km2 <- survfit(y~ipec$escola, data = ipec)

## Função de aalen para escolaridade
aalen2 <- survfit(y~ipec$escola, data = ipec)

## Curva de sobrevivência estratificada por escolaridade
graf3 <- ggsurvplot(km2, censor = F,
                   xlab = "Tempo (anos)", ylab = "S(t)",
                   ggtheme = theme_classic(),
                   title = "Sobrevivência segundo escolaridade",
                   legend.labs = c("sem escolaridade","ensino fundamental",
                                   "ensino médio","ensino superior"),
                   font.tickslab = c(11, "plain", "black"),
                   font.x = c(12,"black"),
                   font.y = c(12,"black"),
                   font.title = c(14, "bold", "black"))
graf3 <- graf3$plot + 
  scale_x_continuous(breaks = seq(0,9,1),
                     limits = c(0,9)) +
  scale_y_continuous(breaks = seq(0,1,0.20)) +
  scale_color_manual(values = c("sem escolaridade" = "#020305", 
                                "ensino fundamental" = "#ec225e",
                                "ensino médio" = "#269199",
                                "ensino superior" = "#eb9961")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"), 
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5))

graf3

## Curva de risco acumulado estratificada por escolaridade
graf4 <- ggsurvplot(aalen2, data = ipec, fun = "cumhaz", censor = F,
                    xlab = "Tempo (anos)", ylab = "Λ(t)",
                    ggtheme = theme_classic(),
                    title = "Risco acumulado segundo escolaridade",
                    legend.labs = c("sem escolaridade","ensino fundamental",
                                    "ensino médio","ensino superior"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(12,"black"),
                    font.y = c(12,"black"),
                    font.title = c(14, "bold", "black"))
graf4 <- graf4$plot + 
  scale_x_continuous(breaks = seq(0,9,1),
                     limits = c(0,9)) +
  scale_y_continuous(breaks = seq(0,1.2,0.2),
                     limits = c(0,1.2)) +
  scale_color_manual(values = c("sem escolaridade" = "#020305", 
                                "ensino fundamental" = "#ec225e",
                                "ensino médio" = "#269199",
                                "ensino superior" = "#eb9961")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"), 
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5))

graf4

## Teste de logrank para escolaridade
logrank2 <- survdiff(y~ipec$escola, data = ipec)
logrank2

## Rodando o modelo Kaplan-Meier estratificado por tratamento
km3 <- survfit(y~ipec$tratam, data = ipec)

## Função de aalen estratificada por tratamento
aalen3 <- survfit(y~strata(ipec$tratam), data = ipec)

## Curva de sobrevivência segundo tratamento
graf5 <- ggsurvplot(km3, censor = F,
                   xlab = "Tempo (anos)", ylab = "S(t)",
                   ggtheme = theme_classic(),
                   legend.title = "Terapia antirretroviral",
                   title = "Sobrevivência segundo tratamento",
                   legend.labs = c("nenhuma","mono",
                                   "combinada","potente"),
                   font.tickslab = c(11, "plain", "black"),
                   font.x = c(12,"black"),
                   font.y = c(12,"black"),
                   font.title = c(13, "bold", "black"))
graf5 <- graf5$plot + 
  scale_x_continuous(breaks = seq(0,9,1),
                     limits = c(0,9)) +
  scale_y_continuous(breaks = seq(0,1,0.20)) +
  scale_color_manual(values = c("nenhuma" = "#020305", 
                                "mono" = "#ec225e",
                                "combinada" = "#269199",
                                "potente" = "#eb9961")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"), 
    legend.title = element_text(size = 12, color = "black"),
    legend.key = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5))

graf5

## Curva de risco acumulado segundo tratamento
graf6 <- ggsurvplot(aalen3, data = ipec, fun = "cumhaz", censor = F,
                    xlab = "Tempo (anos)", ylab = "Λ(t)",
                    ggtheme = theme_classic(),
                    legend.title = "Terapia antirretroviral",
                    title = "Risco acumulado segundo tratamento",
                    legend.labs = c("nenhuma","mono",
                                    "combinada","potente"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(12,"black"),
                    font.y = c(12,"black"),
                    font.title = c(13, "bold", "black"))
graf6 <- graf6$plot + 
  scale_x_continuous(breaks = seq(0,9,1),
                     limits = c(0,9)) +
  scale_y_continuous(breaks = seq(0,2.5,0.5),
                     limits = c(0,2.5)) +
  scale_color_manual(values = c("nenhuma" = "#020305", 
                                "mono" = "#ec225e",
                                "combinada" = "#269199",
                                "potente" = "#eb9961")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"), 
    legend.title = element_text(size = 12, color = "black"),
    legend.key = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5))

graf6

## Teste de logrank para tratamento
logrank3 <- survdiff(y~ipec$tratam, data = ipec)
logrank3

## Terminação
