# Instalar as bibliotecas necessárias:
install.packages("caret")
install.packages("mfx")


# Carregar as bibliotecas:
library(readr)
library(ggplot2)
library(dplyr)
library(caret)
library(mfx)


# Configurar notação decimal:
options(scipen = 999)


# Carregar dataset:
plano_de_saude <- read_csv2("")


# ======= Regressão Logística =======

# == Teste 01: Uso x IMC ==
# Y = Uso
# X = IMC


# Criar modelo:
modelo_01 <- glm(
    uso ~ imc,
    family = binomial(link="logit"),
    data = plano_de_saude
)


# Visualizar estatísticas do modelo:
modelo_01
summary(modelo_01)


# Fazer predição do "y_pred_01":
plano_de_saude$y_pred_01 <- predict(
    modelo_01,
    newdata=plano_de_saude[,c('imc','uso')],
    type="response"
)


# Definir pontos de corte do "y_pred_01" em 0.5:
plano_de_saude <- mutate(
    plano_de_saude,
    adesao_01 = ifelse(y_pred_01 >= 0.5, 1, 0),
    adesao_01 = factor(adesao_01)
)


# Gerar primeira Matriz de Confusão:
conf_matrix_01 <- confusionMatrix(
    factor(plano_de_saude$adesao_01, levels = c(0, 1)),
    factor(plano_de_saude$uso, levels = c(0, 1)),
    positive = "1"
)


# Visualizar matriz:
conf_matrix_01$table


# Vsualizar métricas da matriz:
conf_matrix_01


# Calcular Razão de Chances:
logitor(adesao_01~imc, data = plano_de_saude)