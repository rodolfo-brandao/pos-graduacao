# Instalar as bibliotecas necessárias:
install.packages("caret")


# Carregar as bibliotecas:
library(readr)
library(ggplot2)
library(dplyr)
library(caret)


# Configurar notação decimal:
options(scipen = 999)


# Carregar dataset:
plano_de_saude <- read_csv2("")


# === Regressão Linear ===

# Teste 01: Preço x Idade:
# Y = Preço
# X = Idade


# Sumarizar colunas Preço (Y) e Idade (X):
summary(plano_de_saude$preco)
summary(plano_de_saude$idade)


# Correlação de Pearson:
cor.test(x = plano_de_saude$idade, y = plano_de_saude$preco, method = "pearson")


# Modelo de Regressão Linear (y ~ x):
modelo_plano_de_saude <- lm(preco ~ idade, data = plano_de_saude)
modelo_plano_de_saude


# Observar dados do modelo:
summary(modelo_plano_de_saude)


# Plotar gráfico de disperção com reta ajustada:
ggplot(plano_de_saude, aes(x=idade, y=preco)) +
    geom_point() + 
    geom_smooth(method='lm', se = FALSE) +
    ggtitle("Dispersão entre Preço x Idade") +
    xlab("Idade") +
    ylab("Preço") +
    theme_bw()


# Teste 02: Preço x Idade x Sexo
# Y = Preço
# X1 = Idade
# X2 = Sexo


# Transformar variável dicotômica "sexo", onde seus valores correspondem a: Feminino = 0 / Masculino = 1
plano_de_saude <- mutate(plano_de_saude, sexo_dic = ifelse(sexo == 'feminino', 0, 1))


# Correlação de Pearson:
cor.test(x = plano_de_saude$sexo_dic, y = plano_de_saude$preco, method = "pearson")


# Modelo de Regressão Linear (y ~ x1 + x2):
modelo_plano_de_saude2 <- lm(preco ~ idade + sexo_dic, data = plano_de_saude)
modelo_plano_de_saude2


# Observar dados do novo modelo:
summary(modelo_plano_de_saude2)


# Usar o último modelo para predizer o preço do plano de saúde:
# Em um dataframe, organizar dados de 6 candidatos que desejam aderir ao plano de saúde;
# Sendo estes: 3 mulheres com idade de 18, 35 e 60 anos, e 3 homens com as mesmas idades.
candidatos_plano_de_saude <- data.frame(
    sexo_dic = c(0, 0, 0, 1, 1, 1),
    idade = c(18, 35, 60, 18, 35, 60)
)


# Visualizar o dataframe recém criado:
candidatos_plano_de_saude


# Fazer predição utilizando o modelo:
candidatos_plano_de_saude$precificacao <- predict.lm(modelo_plano_de_saude2, newdata=candidatos_plano_de_saude)
candidatos_plano_de_saude


# Teste 03: Preço x Idade x Sexo x Região
# Y = Preço
# X1 = Idade
# X2 = Sexo (F:0 /M:1)
# X3 = Região


# Normalizar coluna "regiao", onde:
# Nordeste = 1
# Sudeste = 2
# Noroeste = 3
# Sudoeste = 4
plano_de_saude = plano_de_saude %>%
    mutate(regiao2 = case_when(
        regiao == "nordeste" ~ 1,
        regiao == "sudeste" ~ 2,
        regiao == "noroeste" ~ 3,
        regiao == "sudoeste" ~ 4,
        TRUE ~ 0
    ))


# Correlação de Pearson:
cor.test(x = plano_de_saude$regiao2, y = plano_de_saude$preco, method = "pearson")


# Modelo de Regressão Linear (y ~ x1 + x2 + x3):
modelo_plano_de_saude3 <- lm(preco ~ idade + sexo_dic + regiao2, data = plano_de_saude)
modelo_plano_de_saude3


summary(modelo_plano_de_saude3)


# Sobrescrever o dataframe passado para incluir os dados da região:
# Dessa vez os canditados serão 4 mulheres e 4 homens, sendo dois de cada sexo
# com meia idade (35 anos) e dois idosos (60 anos), respectivamente
candidatos_plano_de_saude <- data.frame(
    sexo_dic = c(0, 0, 0, 0, 1, 1, 1, 1),
    idade = c(18, 35, 45, 60, 18, 35, 45, 60),
    regiao2 = c(1, 2, 3, 4, 1, 2, 3, 4)
)


# Fazer predição utilizando o modelo:
candidatos_plano_de_saude$precificacao <- predict.lm(modelo_plano_de_saude3, newdata=candidatos_plano_de_saude)
candidatos_plano_de_saude