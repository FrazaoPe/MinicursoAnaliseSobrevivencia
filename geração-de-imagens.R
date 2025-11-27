#GeraçãoDeGráficos.R
#Minicurso - Análise de sobrevivência
#Universidade federal fluminense
#Instituto de matemática e estatística
#Bacharelado em estatística
#Minicurso: Análise de sobrevivência
#Semestre 2024.1
#Professora: Núbia Carla
#Monitores: Pedro Frazão Dutra

## Carregamento dos pacotes necessários ##
require(tidyverse)
require(survival)
require(survminer)
require(ggfortify)
require(ggsurvfit)
require(cowplot)

aids <- read_table(file = "adesao.txt")
ipec <- read_csv2(file = "ipec.csv")
dialise <- read_csv(file = "dialise.csv")
dialise1 <- read_csv2(file = "banquinho1.csv")
dialise2 <- read_csv2(file = "banquinho2.csv")
dialise3 <- read_csv2(file = "banquinho3.csv")
paciente1 <- read_csv2(file = "pacientao.csv")
pacienteA <- read_delim("pacientaoA.csv", 
                         delim = ";", escape_double = FALSE, 
                         col_types = cols(...9 = col_skip(), 
                                          ...10 = col_skip(), 
                                          ...11 = col_skip(), 
                                          ...12 = col_skip()), trim_ws = TRUE)

dialise1 <- dialise1 |> 
  mutate(evento = ifelse(status == 1, "óbito", "censura"))

dialise2 <- dialise2 |> mutate(evento = ifelse(status == 1, "óbito", "censura"))

dialise3 <- dialise3 |> mutate(evento = ifelse(status == 1, "óbito", "censura"))


## Figura 2.1
ggplot(dialise1, aes(y = factor(paciente))) +
  geom_segment(aes(x = 0, xend = tempo, yend = factor(paciente)), color = "darkblue") + 
  geom_point(aes(x = tempo, shape = evento), size = 3, color = "black") + 
  scale_shape_manual(values = c("óbito" = 4, "censura" = 1)) +
  labs(x = "tempo (meses)", y = "paciente", shape = " ") +
  theme_cowplot() +
  scale_x_continuous(expand = expansion(add = c(0, 0)), limits = c(0,50),
                     breaks = seq(0,40,10))

## Figura 2.2
ggplot(dialise2, aes(y = factor(paciente))) +
  geom_segment(aes(x = 0, xend = tempo, yend = factor(paciente)), color = "darkblue") + 
  geom_point(aes(x = tempo, shape = evento), size = 3, color = "black") + 
  scale_shape_manual(values = c("óbito" = 4, "censura" = 1)) +
  labs(x = "tempo (meses)", y = "paciente", shape = " ") +
  theme_cowplot() +
  scale_x_continuous(expand = expansion(add = c(0, 0)), limits = c(0,50),
                     breaks = seq(0,40,10))

## Figura 2.6
dialise <- data.frame(
  id = 1:10,
  tempo = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
  inicio = c(0,13,0,5,10,0,0,11,4,14),
  fim = c(22,21,11,45,34,12,31,28,37,39),
  status = c(1, 0, 1, 0, 1, 1, 1, 0, 1, 1)
)

dialise <- dialise |> mutate(event = ifelse(status == 1, "óbito", "censura"))

ggplot(dialise, aes(y = factor(id))) +
  geom_segment(aes(x = inicio, xend = fim, yend = factor(id)), color = "darkblue") + 
  geom_point(aes(x = fim, shape = event), size = 3, color = "black") + 
  scale_shape_manual(
    values = c("óbito" = 4, "censura" = 1),
    name = " "
  ) +
  labs(x = "tempo (meses)", y = "paciente", color = " ") +
  theme_cowplot() + 
  scale_x_continuous(expand = expansion(add = c(0, 0)), limits = c(0,50),
                     breaks = seq(0,40,10))
  

## Figuras 2.6

# Indicador de falhas
dados <- data.frame(
  t = c(0,21.999,22,22.1,40),
  y = c(0,0,1,1,1)
)

obito <- dados[dados$t == 22, ]

ggplot() +
  geom_step(data = dados[dados$t < 22, ], aes(x = t, y = y), direction = "hv") + 
  geom_step(data = dados[dados$t > 22, ], aes(x = t, y = y), direction = "hv") +
  geom_segment(aes(x = 22, xend = 22, y = 0, yend = 1), linetype = "dotted") +
  geom_point(data = obito, aes(x = t, y = y), size = 3) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  labs(y = expression(N[1](t)), x = NULL) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) + 
  theme_cowplot()

# Indicador do grupo de risco

dados <- data.frame(
  t = c(0,21.999,22,22.1,40),
  y = c(1,1,0,0,0)
)

obito <- dados[dados$t == 22, ]

ggplot() +
  geom_step(data = dados[dados$t < 22, ], aes(x = t, y = y), direction = "hv") + 
  geom_step(data = dados[dados$t > 22, ], aes(x = t, y = y), direction = "hv") +
  geom_segment(aes(x = 22, xend = 22, y = 0, yend = 1), linetype = "dotted") +
  geom_point(data = obito, aes(x = t, y = 1), size = 3) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  labs(y = expression(Y[1](t)), x = NULL) +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) + 
  theme_cowplot()

## Figuras 2.8A

## Indicador de risco
dados <- data.frame(
  t = c(0,5.999,6,6.0001,40),
  y = c(1,1,0,0,0)
)

obito <- dados[dados$t == 6, ]

ggplot() +
  geom_step(data = dados[dados$t < 6, ], aes(x = t, y = y), direction = "hv") + 
  geom_step(data = dados[dados$t > 6, ], aes(x = t, y = y), direction = "hv") +
  geom_segment(aes(x = 6, xend = 6, y = 0, yend = 1), linetype = "dotted") +
  geom_point(data = obito, aes(x = t, y = 1), size = 3) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  labs(y = expression(Y[A](t)), x = NULL) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) + 
  theme_cowplot()

## Indicador do número de óbitos

dados <- data.frame(
  t = c(0,9.999,10,10.1,40),
  y = c(0,0,0,0,0)
)

obito <- dados[dados$t == 10, ]

ggplot() +
  geom_step(data = dados[dados$t < 10, ], aes(x = t, y = y), direction = "hv") + 
  geom_step(data = dados[dados$t > 10, ], aes(x = t, y = y), direction = "hv") +
  scale_y_continuous(breaks = seq(0,1,1), labels = c("0", "1"),
                     limits = c(0,1)) +
  labs(y = expression(N[A](t)), x = NULL) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) + 
  theme_cowplot()

## Figuras 2.8B

dados <- data.frame(
  t = c(0,9.999,10,10.1,40),
  y = c(1,1,1,1,1)
)

ggplot() +
  geom_step(data = dados[dados$t < 10, ], aes(x = t, y = y), direction = "hv") + 
  geom_step(data = dados[dados$t > 10, ], aes(x = t, y = y), direction = "hv") +
  scale_y_continuous(breaks = seq(0,1,1), labels = c("0", "1"),
                     limits = c(0,1)) +
  labs(y = expression(Y[B](t)), x = NULL) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) + 
  theme_cowplot()

dados <- data.frame(
  t = c(0,9.999,10,10.1,40),
  y = c(0,0,0,0,0)
)

ggplot() +
  geom_step(data = dados[dados$t < 10, ], aes(x = t, y = y), direction = "hv") + 
  geom_step(data = dados[dados$t > 10, ], aes(x = t, y = y), direction = "hv") +
  scale_y_continuous(breaks = seq(0,1,1), labels = c("0", "1"),
                     limits = c(0,1)) +
  labs(y = expression(N[B](t)), x = NULL) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) + 
  theme_cowplot()

## Figuras 2.9A

## Indicador de risco

dados <- data.frame(
  t = c(0,9.999,10,10.1,32.999,33,33.001,40),
  y = c(0,0,1,1,1,0,0,0)
)

obito1 <- dados[dados$t == 10, ]
obito2 <- dados[dados$t == 33,]

ggplot() +
  geom_step(data = dados[dados$t < 10, ], aes(x = t, y = y), direction = "hv") + 
  geom_segment(aes(x = 10, xend = 10, y = 0, yend = 1), linetype = "dotted") +
  geom_step(data = dados[dados$t > 10 & dados$t < 33, ], aes(x = t, y = y), direction = "hv") +
  geom_segment(aes(x = 33, xend = 33, y = 0, yend = 1), linetype = "dotted") +
  geom_step(data = dados[dados$t > 33, ], aes(x = t, y = y), direction = "hv") +
  geom_point(data = obito1, aes(x = t, y = 1), size = 3) +
  geom_point(data = obito2, aes(x = t, y = 1), size  = 3) + 
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  labs(y = expression(Y[5](t)), x = NULL) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme_cowplot()

## Indicador do número de óbitos

dados <- data.frame(
  t = c(0,32.999,33,33.001,40),
  y = c(0,0,1,1,1)
)

obito <- dados[dados$t == 33, ]

ggplot() +
  geom_step(data = dados[dados$t < 33, ], aes(x = t, y = y), direction = "hv") + 
  geom_step(data = dados[dados$t > 33, ], aes(x = t, y = y), direction = "hv") +
  geom_segment(aes(x = 33, xend = 33, y = 0, yend = 1), linetype = "dotted") +
  geom_point(data = obito, aes(x = t, y = 1), size = 3) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  labs(y = expression(N[5](t)), x = NULL) +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) + 
  theme_cowplot()

## Figuras 2.9B

## Indicador de risco
dados <- data.frame(
  t = c(0,11.999,12,12.001,29.999,30,30.1,40),
  y = c(0,0,1,1,1,0,0,0)
)

obito1 <- dados[dados$t == 12, ]
obito2 <- dados[dados$t == 30,]

ggplot() +
  geom_step(data = dados[dados$t < 12, ], aes(x = t, y = y), direction = "hv") + 
  geom_segment(aes(x = 12, xend = 12, y = 0, yend = 1), linetype = "dotted") +
  geom_step(data = dados[dados$t > 12 & dados$t < 30, ], aes(x = t, y = y), direction = "hv") +
  geom_segment(aes(x = 30, xend = 30, y = 0, yend = 1), linetype = "dotted") +
  geom_step(data = dados[dados$t > 30, ], aes(x = t, y = y), direction = "hv") +
  geom_point(data = obito1, aes(x = t, y = 1), size = 3) +
  geom_point(data = obito2, aes(x = t, y = 1), size  = 3) + 
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  labs(y = expression(Y[8](t)), x = NULL) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme_cowplot()

## Indicador do número de falhas
dados <- data.frame(
  t = c(0,29.999,30,30.1,40),
  y = c(0,0,0,0,0)
)

obito <- dados[dados$t == 30, ]

ggplot() +
  geom_step(data = dados[dados$t < 30, ], aes(x = t, y = y), direction = "hv") + 
  geom_step(data = dados[dados$t > 30, ], aes(x = t, y = y), direction = "hv") +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1"),
                     limits = c(0,1)) +
  labs(y = expression(N[8](t)), x = NULL) +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) + 
  theme_cowplot()

## Figuras do ex1.A

dados <- data.frame(
  t = c(0,1.999,2,2.0001,3.999,4,4.00001,12),
  y = c(0,0,1,1,1,0,0,0)
)

obito1 <- dados[dados$t == 2, ]
obito2 <- dados[dados$t == 4,]

ggplot() +
  geom_step(data = dados[dados$t < 2, ], aes(x = t, y = y), direction = "hv") + 
  geom_segment(aes(x = 2, xend = 2, y = 0, yend = 1), linetype = "dotted") +
  geom_step(data = dados[dados$t > 2 & dados$t < 4, ], aes(x = t, y = y), direction = "hv") +
  geom_segment(aes(x = 4, xend = 4, y = 0, yend = 1), linetype = "dotted") +
  geom_step(data = dados[dados$t > 4, ], aes(x = t, y = y), direction = "hv") +
  geom_point(data = obito1, aes(x = t, y = 1), size = 3) +
  geom_point(data = obito2, aes(x = t, y = 1), size  = 3) + 
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  scale_x_continuous(breaks = seq(0,12,2)) +
  labs(y = expression(Y[A](t)), x = NULL) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme_cowplot()

dados <- data.frame(
  t = c(0,3.999,4,4.00001,12),
  y = c(0,0,1,1,1)
)

obito <- dados[dados$t == 4, ]

ggplot() +
  geom_step(data = dados[dados$t < 4, ], aes(x = t, y = y), direction = "hv") + 
  geom_step(data = dados[dados$t > 4, ], aes(x = t, y = y), direction = "hv") +
  geom_segment(aes(x = 4, xend = 4, y = 0, yend = 1), linetype = "dotted") +
  geom_point(data = obito, aes(x = t, y = 1), size = 3) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  scale_x_continuous(breaks = seq(0,12,2)) +
  labs(y = expression(N[A](t)), x = NULL) +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) + 
  theme_cowplot()

## Figuras do ex2.B

## Indicador de risco
dados <- data.frame(
  t = c(0,7.999,8,8.00001,12),
  y = c(1,1,0,0,0)
)

obito <- dados[dados$t == 8, ]

ggplot() +
  geom_step(data = dados[dados$t < 8, ], aes(x = t, y = y), direction = "hv") + 
  geom_step(data = dados[dados$t > 8, ], aes(x = t, y = y), direction = "hv") +
  geom_segment(aes(x = 8, xend = 8, y = 0, yend = 1), linetype = "dotted") +
  geom_point(data = obito, aes(x = t, y = 1), size = 3) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  scale_x_continuous(breaks = seq(0,12,2)) +
  labs(y = expression(Y[B](t)), x = NULL) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) + 
  theme_cowplot()

## Indicadora do número de falhas
dados <- data.frame(
  t = c(0,7.999,8,8.00001,12),
  y = c(0,0,1,1,1)
)

obito <- dados[dados$t == 8, ]

ggplot() +
  geom_step(data = dados[dados$t < 8, ], aes(x = t, y = y), direction = "hv") + 
  geom_step(data = dados[dados$t > 8, ], aes(x = t, y = y), direction = "hv") +
  geom_segment(aes(x = 8, xend = 8, y = 0, yend = 1), linetype = "dotted") +
  geom_point(data = obito, aes(x = t, y = 1), size = 3) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
  scale_x_continuous(breaks = seq(0,12,2)) +
  labs(y = expression(N[B](t)), x = NULL) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  ) + 
  theme_cowplot()

## Figura 3.1
dados <- data.frame(
  tempo = c(5, 8, 12, 18, 25, 31, 36, 42, 50, 61, 70),
  status = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)
surv_obj <- Surv(dados$tempo, dados$status)
fit_weibull <- survreg(surv_obj ~ 1, dist = "weibull")
shape_weibull <- 1 / fit_weibull$scale
scale_weibull <- 35 / (log(2)^(1 / shape_weibull))
tempo_pred <- seq(0, 80, by = 1)
weibull_surv <- 1 - pweibull(tempo_pred, shape = shape_weibull, 
                             scale = scale_weibull)
df_plot <- data.frame(tempo = tempo_pred, S_t = weibull_surv)
ggplot(df_plot, aes(x = tempo, y = S_t)) + 
  geom_line(color = "darkblue") +
  labs(x = "Tempo (t)", y = "S(t)") +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(0, 70, 5), limits = c(0, 80)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  geom_segment(aes(x = 35, xend = 35, 
                   y = 0, yend = 0.5),  
               color = "darkblue", linetype = "dotted") +
  geom_segment(aes(x = 0, xend = 35, 
                   y = 0.5, yend = 0.5),  
               color = "darkblue", linetype = "dotted") +
  theme_cowplot()

## Figura 3.2
ipec2 <- ipec |> filter(status == 1) |> slice_head(n = 32)

## Montando tabela com os dados de sobrevivência gerados pela função survfit
k = survival::survfit(Surv(ipec2$tempo,ipec2$status)~1, data = ipec2)
tempo = k[["time"]]
R = k[["n.risk"]]
N = k[["n.event"]]
tabela = data.frame(tempo,R,N)
dimensao = dim(tabela)

## Limite inferior dos intervalos de tempo
tabela$limInf[1] = 0
for(i in 2:nrow(tabela)){
  tabela$limInf[i]= tabela$tempo[i-1]
}

## Sobrevivência
tabela$sobrevivencia= round(tabela$R/tabela$R[1],4)

## Curva de sobrevivência
ggplot(tabela, aes(limInf,sobrevivencia)) + geom_step(color = "darkgreen") +
  scale_y_continuous(name= "S(t)", breaks = seq(0,1,0.20)) +
  scale_x_continuous(name= "tempo (dias)", breaks = seq(0,1600,200),
                     limits = c(0,1600)) +
  theme_cowplot() + geom_segment(aes(x = 194, xend = 194, 
                                     y = 0, yend = 0.5),  
                                 color = "darkblue", linetype = "dotted") +
  geom_segment(aes(x = 0, xend = 194, 
                   y = 0.5, yend = 0.5),  
               color = "darkblue", linetype = "dotted")

## Observação: 6 linhas foram removidas para fazer um gráfico semelhante ao
## do livro

## Figura 3.3A
ggplot() + 
  geom_segment(aes(x = 0, xend = 40, y = 1), color = "darkblue") +
  labs(x = "tempo", y = expression(lambda(t)), color = " ") +
  scale_x_continuous(breaks = seq(0,40,10),
                     limits = c(0,40)) + 
  scale_y_continuous(breaks = seq(0,1.4,0.2),
                     limits = c(0,1.4))+
  theme_cowplot()

## Figura 3.3B
x <- seq(0.01, 50, by = 0.01)
y <- 1 / x
data1 <- data.frame(tempo = x, risco = y)

ggplot(data1, aes(x,y)) + geom_line(color = "darkblue") +
  scale_y_continuous(name= expression(lambda(t)), limits = c(0,1.4),
                     breaks = seq(0,1.4,0.2)) +
  scale_x_continuous(name= "tempo", limits = c(0,50),
                     breaks = seq(0,40,10)) +
  theme_cowplot()

## Figura 3.3C
x <- seq(1, 50, by = 0.1)
y <- 2*log(x)
dados2 <- data.frame(x,y)
ggplot(dados2, aes(x, y)) +
  geom_step(color = "darkblue") +
  scale_y_continuous(name = expression(lambda(t)), limits = c(0,10),
                     breaks = seq(0,10, 2)) +
  scale_x_continuous(name = "tempo", limits = c(0, 50),
                     breaks = seq(0, 40, 10)) +
  theme_cowplot()

## Figura 3.3D
tempo1 <- seq(0, 20, by = 0.1)
crescimento <- function(t, L=1, k=0.7, t0=10) {
  L / (1 + exp(-k * (t - t0)))
}
risco1 <- crescimento(tempo1)
dados1 <- data.frame(tempo = tempo1, risco = risco1)
t <- seq(20, 30, by = 0.1)
decrescimento_fator <- 0.0041
r <- numeric(length(t))
for (i in 1:length(r)) {
  r[i] <- risco1[length(risco1)] - decrescimento_fator * (t[i] - 20)
}
dados2 <- data.frame(tempo = t, risco = r)
dados_final <- rbind(dados1, dados2)
ggplot(dados_final, aes(tempo,risco)) +
  geom_line(color = "darkblue") +
  scale_y_continuous(name = expression(lambda(t))) +
  scale_x_continuous(name = "tempo", limits = c(0,30),
                     breaks = seq(0,30,5)) +
  theme_cowplot()

## Figura 3.3E
x <- seq(5, 45, by=0.1)
y <- 0.0001*(x - 22)^2 + 0.01

dados4 <- data.frame(x,y)
dados4$x = dados4$x - 5
ggplot(dados4, aes(x, y)) +
  geom_line(color = "darkblue") +
  scale_y_continuous(name = expression(lambda(t))) +
  scale_x_continuous(name = "tempo", limits = c(0, 50),
                     breaks = seq(0, 40, 10)) +
  theme_cowplot()

## Figura 3.3F
x <- seq(0.9, 14, length.out = 1000)
y <- dgamma(x, shape = 2, rate = 0.5)

data = data.frame(x,y)
ggplot(data, aes(x, y)) +
  geom_line(color = "darkblue")+ scale_y_continuous(limits = c(0,0.20),
                                                    breaks = seq(0,0.20,0.05)) +
  scale_x_continuous(limits = c(0,14), breaks = seq(0,14,2)) +
  labs(x = "tempo", y = "risco") + 
  theme_cowplot()

## Figura 4.1

ipec90 <- ipec |> filter(tempo < 90)

km90 = survfit(Surv(ipec90$tempo,ipec90$status)~1, data = ipec90)

autoplot(km90, censor = TRUE,censor.colour = "darkblue", conf.int = FALSE, surv.colour = "darkblue") +
  labs(x = "tempo", y = "S(t)") +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) +
  scale_x_continuous(breaks = seq(0,80,20),
                     limits = c(0,90)) +
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0,1,0.2)) +
  annotate("text", x = 75, y = 0.7, label = "+ censura", color = "darkblue",
           size = 4, color = "black") + 
  theme_cowplot()

## Figura 4.2  
graf1 <- ggplot2::autoplot(km90, fun = "cumhaz", censor = TRUE, conf.int = FALSE,
                  surv.colour = "darkblue", censor.colour = "darkblue") +
  labs(x = "tempo", y = expression(Lambda(t))) +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) + theme_cowplot()
graf1


## Figura 4.3
ggplot2::autoplot(km90, censor = TRUE, conf.int = TRUE, conf.int.alpha = 0,
                  conf.int.linetype = "dashed", surv.colour = "darkblue", 
                  censor.colour = "darkblue") +
  labs(x = "tempo", y = "S(t)") +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) +
  scale_x_continuous(breaks = seq(0,80,20),
                     limits = c(0,90)) +
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0,1,0.2)) +
  theme_cowplot()

## Figura 4.4
graf2 <- ggsurvfit(km90,color = "darkblue") + 
  add_censor_mark(color = "darkblue") +
  add_confidence_interval(type = "lines", color = "darkblue") +
  add_quantile(y_value = 0.5, linetype = "dashed", color = "darkblue") +
  labs(x = "tempo", y = "S(t)") +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,80,20),
                     limits = c(0,90)) +
  coord_cartesian(ylim = c(0,1)) + 
scale_y_continuous(limits = c(0,1),
                   breaks = seq(0,1,0.2)) + 
  annotate("text", x = 52 + 1, y = 0.02, 
           label = "Tempo mediano", 
           color = "black", size = 4, hjust = 0, angle  = 90) +
  geom_segment(aes(x = 37, xend = 37, 
                   y = 0, yend = 0.5),  
               color = "darkblue", linetype = "dotted") +
  annotate("text",  x = 37 + 1, y = 0.02,
           label = "Limite inferior IC95%", color = "black",
           size = 4, hjust = 0, angle = 90) + 
  annotate("text", x = 0, y = 0.48,
           label = "S(t) = 0.5", size  = 4, hjust = 0, angle = 0) +
  theme_cowplot()

graf2

## Figura 4.5A

km = survfit(Surv(ipec$tempo,ipec$status)~1, data = ipec)

ggsurvfit(km,color = "darkblue", theme = theme_classic()) +
  labs(x = "tempo (dias)", y = "S(t)") +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) +
  scale_x_continuous(breaks = seq(0,3000,500),
                     limits = c(0,3500)) +
  coord_cartesian(ylim = c(0,1)) + 
scale_y_continuous(limits = c(0,1),
                   breaks = seq(0,1,0.2)) + 
  theme_cowplot()

## Figura 4.5B

graf3 <- ggplot2::autoplot(km, fun = "cumhaz", censor = FALSE, conf.int = FALSE,
                           surv.colour = "darkblue", censor.colour = "darkblue") +
  labs(x = "dias", y = expression(Lambda(t))) +
  scale_x_continuous(breaks = seq(0, 3500, 500), limits = c(0, 3500)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)) + 
  theme_cowplot()

graf3

## Figura 4.6
y <- Surv(ipec$tempo,ipec$status)

km1 <- survfit(y~ipec$sexo, data = ipec)

graf <- ggsurvplot(km1, censor = F,
                    xlab = "Tempo (dias)", ylab = "S(t)",
                    legend.title = " ",
                    legend.labs = c("feminino","masculino"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(12,"black"),
                    font.y = c(12,"black"),
                    font.title = c(14, "bold", "black"))
graf <- graf$plot + 
  scale_x_continuous(breaks = seq(0, 3000, 500), limits = c(0, 3500)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("feminino" = "#ec225e", 
                                "masculino" = "black")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)) + 
  theme_cowplot()

graf

## Figura 4.7

graf <- ggsurvplot(km1, censor = F, conf.int = T, conf.int.style = "step",
                   linetype = "solid",
                   xlab = "Tempo (dias)", ylab = "S(t)",
                   legend.title = " ",
                   legend.labs = c("feminino","masculino"),
                   font.tickslab = c(11, "plain", "black"), conf.int.size = 3,
                   font.x = c(12,"black"),
                   font.y = c(12,"black"),
                   font.title = c(14, "bold", "black"))
graf <- graf$plot + 
  scale_x_continuous(breaks = seq(0, 3000, 500), limits = c(0, 3500)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("feminino" = "#ec225e", 
                                "masculino" = "black")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)) + 
  theme_cowplot()

graf

graf <- ggsurvplot(km1, censor = F, conf.int = T,
                   xlab = "Tempo (dias)", ylab = "S(t)",
                   legend.title = " ",
                   legend.labs = c("feminino","masculino"),
                   font.tickslab = c(11, "plain", "black"), conf.int.size = 3,
                   font.x = c(12,"black"),
                   font.y = c(12,"black"),
                   font.title = c(14, "bold", "black"))
graf <- graf$plot + 
  scale_x_continuous(breaks = seq(0, 3000, 500), limits = c(0, 3500)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("feminino" = "#ec225e", 
                                "masculino" = "black")) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)) + 
  theme_cowplot()

graf

