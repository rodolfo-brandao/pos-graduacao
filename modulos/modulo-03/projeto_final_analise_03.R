# PROJETO FINAL: Microdados Enem 2023 - Nordeste
# AUTOR: Rodolfo Brandão

# PROPOSTA:
# Avaliar a proporção de inscritos treineiros em relação
# aos demais. É considerado treineiro todo inscrito que não
# havia concluído o ensino médio e não o concluiria em 2023.



# Instalar pacotes (caso necessário):
# install.packages("dplyr")
# install.packages("tidyverse")



# Importar pacotes:
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)



# Carregar dataset:
microdados_enem_2023_ne <- read_csv2(
    "",
    show_col_types = FALSE
)



# (opcional) Listar colunas do dataset:
colnames(microdados_enem_2023_ne)



# Separar somente com as colunas necessárias em um novo dataset:
situacao_escolaridade = microdados_enem_2023_ne %>%
    select(
        TP_ST_CONCLUSAO
    )



# Agrupar por escolaridade:
situacao_escolaridade_agrupado <- situacao_escolaridade %>%
    count(TP_ST_CONCLUSAO, name = "QTD_INSCRITOS")



# Criar novo dataframe com a soma por treineiros e não treineiros:
soma_situacao_escolaridade <- situacao_escolaridade_agrupado %>%
    summarise(
        QTD_TREINEIROS = sum(QTD_INSCRITOS[
            TP_ST_CONCLUSAO == "Estou cursando e concluirei o Ensino Médio após 2023" |
            TP_ST_CONCLUSAO == "Não concluí e não estou cursando o Ensino Médio"]),
        QTD_NAO_TREINEIROS = sum(QTD_INSCRITOS[
            TP_ST_CONCLUSAO == "Estou cursando e concluirei o Ensino Médio em 2023" |
            TP_ST_CONCLUSAO == "Já concluí o Ensino Médio"])
    )



# Reorganizar colunas em um novo dataframe para melhorar a visualização dos dados em um gráfico:
situacao_escolaridade_normalizado <- soma_situacao_escolaridade %>%
    rename(TREINEIRO = QTD_TREINEIROS, NAO_TREINEIRO = QTD_NAO_TREINEIROS) %>%
    pivot_longer(
        cols = everything(),
        names_to = "Situação",
        values_to = "Total"
    )

# Gerar gráfico de pizza ilustrando a proporção de entre os dois grupos de inscritos:
ggplot(situacao_escolaridade_normalizado, aes(x = "", y = Total, fill = Situação)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    labs(title = "Inscritos Treineiros vs. Não Treineiros",
         fill = "Situação") +
    theme_void() + 
    theme(plot.margin = margin(20, 20, 20, 20))