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

# == Teste 03: Uso x Número de Dependentes ==
# Y = Uso
# X = Dependentes


# Criar modelo:
modelo_03 <- glm(
    uso ~ dependentes,
    family = binomial(link="logit"),
    data = plano_de_saude
)


# Visualizar estatísticas do modelo:
modelo_03
summary(modelo_03)


# Fazer predição do "y_pred_03" utilizando o modelo:
plano_de_saude$y_pred_03 <- predict(
    modelo_03,
    newdata=plano_de_saude[,c('dependentes','uso')],
    type="response"
)


# Definir pontos de corte do "y_pred_03" em 0.5:
plano_de_saude <- mutate(
    plano_de_saude,
    adesao_03 = ifelse(y_pred_03 >= 0.5, 1, 0),
    adesao_03 = factor(adesao_03)
)


# Gerar terceira Matrix de Confusão:
conf_matrix_03 <- confusionMatrix(
    factor(plano_de_saude$adesao_03, levels = c(0, 1)),
    factor(plano_de_saude$uso, levels = c(0, 1)),
    positive = "1"
)


# Visualizar a Matrix:
conf_matrix_03$table


# Vsualizar métricas da Matrix:
conf_matrix_03


# Calcular Razão de Chances:
logitor(adesao_03~dependentes, data = plano_de_saude) # <- O algoritmo não converge