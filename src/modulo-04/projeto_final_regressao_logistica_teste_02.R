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

# == Teste 02: Uso x Tabagismo ==
# Y = Uso
# X = Tabagismo


# Criar modelo:
modelo_02 <- glm(
    uso ~ tabagismo,
    family = binomial(link="logit"),
    data = plano_de_saude
)


# Visualizar estatísticas do modelo:
modelo_02
summary(modelo_02)


# Fazer predição do "y_pred_02":
plano_de_saude$y_pred_02 <- predict(
    modelo_02,
    newdata=plano_de_saude[,c('tabagismo','uso')],
    type="response"
)


# Definir pontos de corte do "y_pred_02" em 0.5:
plano_de_saude <- mutate(
    plano_de_saude,
    adesao_02 = ifelse(y_pred_02 >= 0.5, 1, 0),
    adesao_02 = factor(adesao_02)
)


# Gerar segunda Matriz de Confusão:
conf_matrix_02 <- confusionMatrix(
    factor(plano_de_saude$adesao_02, levels = c(0, 1)),
    factor(plano_de_saude$uso, levels = c(0, 1)),
    positive = "1"
)


# Visualizar matriz:
conf_matrix_02$table


# Vsualizar métricas da matriz:
conf_matrix_02


# Calcular Razão de Chances:
logitor(adesao_02~tabagismo, data = plano_de_saude) # <- O algoritmo não converge