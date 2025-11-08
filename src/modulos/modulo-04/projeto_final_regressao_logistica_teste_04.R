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

# == Teste 03: Uso x IMC + Tabagismo + Número de Dependentes ==
# Y = Uso
# X1 = IMC
# X2 = Tabagismo
# X3 = Dependentes


# Criar modelo:
modelo_04 <- glm(
    uso ~ imc + tabagismo + dependentes,
    family = binomial(link="logit"),
    data = plano_de_saude
)


# Visualizar estatísticas do modelo:
modelo_04
summary(modelo_04)


# Fazer predição do "y_pred_03" utilizando o modelo:
plano_de_saude$y_pred_04 <- predict(
    modelo_04,
    newdata=plano_de_saude[,c('imc','tabagismo','dependentes','uso')],
    type="response"
)


# Definir pontos de corte do "y_pred_04" em 0.5:
plano_de_saude <- mutate(
    plano_de_saude,
    adesao_04 = ifelse(y_pred_04 >= 0.5, 1, 0),
    adesao_04 = factor(adesao_04)
)


# Gerar quarta Matrix de Confusão:
conf_matrix_04 <- confusionMatrix(
    factor(plano_de_saude$adesao_04, levels = c(0, 1)),
    factor(plano_de_saude$uso, levels = c(0, 1)),
    positive = "1"
)


# Visualizar a Matrix:
conf_matrix_04$table


# Vsualizar métricas da Matrix:
conf_matrix_04


# Calcular Razão de Chances:
logitor(adesao_04~imc+tabagismo+dependentes, data = plano_de_saude)