# PROJETO FINAL: Microdados Enem 2023 - Nordeste
# AUTOR: Rodolfo Brandão

# PROPOSTA:
# Através de visualização de dados, observar a distribuição entre
# os grupos de gênero masculino e feminino, em relação a faixa etária,
# dos inscritos. Idealmente, deve-se tratar e separar os dados necessários
# para gerar 3 gráficos dos quais seja possível observar a distribuição dos
# grupos masculino e feminino, além de ter um comparativo entre os dois grupos.



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



# (opcional) Listar colunas do dataset (opcional):
colnames(microdados_enem_2023_ne)



# Separar somente com as colunas necessárias em um novo dataset:
faixa_etaria_por_genero = microdados_enem_2023_ne %>%
    select(TP_FAIXA_ETARIA, TP_SEXO)



# Simplificar os valores da coluna "TP_FAIXA_ETARIA" para melhorar a
# visualização nos gráficos:
faixa_etaria_por_genero = faixa_etaria_por_genero %>%
    mutate(TP_FAIXA_ETARIA = case_when(
        TP_FAIXA_ETARIA == "Menor de 17 anos" ~ "< 17",
        TP_FAIXA_ETARIA == "17 anos" ~"17",
        TP_FAIXA_ETARIA == "18 anos" ~ "18",
        TP_FAIXA_ETARIA == "19 anos" ~ "19",
        TP_FAIXA_ETARIA == "20 anos" ~ "20",
        TP_FAIXA_ETARIA == "21 anos" ~ "21",
        TP_FAIXA_ETARIA == "22 anos" ~ "22",
        TP_FAIXA_ETARIA == "23 anos" ~ "23",
        TP_FAIXA_ETARIA == "24 anos" ~ "24",
        TP_FAIXA_ETARIA == "25 anos" ~ "25",
        TP_FAIXA_ETARIA == "Entre 26 e 30 anos" ~ "26~30",
        TP_FAIXA_ETARIA == "Entre 31 e 35 anos" ~ "31~35",
        TP_FAIXA_ETARIA == "Entre 36 e 40 anos" ~ "36~40",
        TP_FAIXA_ETARIA == "Entre 41 e 45 anos" ~ "41~45",
        TP_FAIXA_ETARIA == "Entre 46 e 50 anos" ~ "46~50",
        TP_FAIXA_ETARIA == "Entre 51 e 55 anos" ~ "51~55",
        TP_FAIXA_ETARIA == "Entre 56 e 60 anos" ~ "56~60",
        TP_FAIXA_ETARIA == "Entre 61 e 65 anos" ~ "61~65",
        TP_FAIXA_ETARIA == "Entre 66 e 70 anos" ~ "66~70",
        TP_FAIXA_ETARIA == "Maior de 70 anos" ~ "> 70",
        TRUE ~ TP_FAIXA_ETARIA
    ))



# Agrupar por faixa etária:
faixa_etaria_por_genero <- faixa_etaria_por_genero %>%
    count(TP_FAIXA_ETARIA, TP_SEXO, name = "QTD_SEXO")



# Separar grupo de pessoas do gênero masculino:
grupo_faixa_etaria_masculino <- faixa_etaria_por_genero %>%
    filter(TP_SEXO == "Maculino")

# Gerar gráfico de barras para visualizar a distribuição de pessoas do gênero masculino:
ggplot(grupo_faixa_etaria_masculino, aes(x = TP_FAIXA_ETARIA, y = QTD_SEXO)) +
    geom_col(fill = "cornflowerblue") +
    labs(
        title = "Pessoas do Sexo Masculino por Faixa Etaria",
        x = "Faixa Etaria",
        y = "Quantidade"
    ) +
    theme_minimal()



# Separar grupo de pessoas do gênero feminino:
grupo_faixa_etaria_feminino <- faixa_etaria_por_genero %>%
    filter(TP_SEXO == "Feminino")

# Gerar gráfico de barras para visualizar a distribuição de pessoas do gênero feminino:
ggplot(grupo_faixa_etaria_feminino, aes(x = TP_FAIXA_ETARIA, y = QTD_SEXO)) +
    geom_col(fill = "cornflowerblue") +
    labs(
        title = "Pessoas do Sexo Feminino por Faixa Etaria",
        x = "Faixa Etaria",
        y = "Quantidade"
    ) +
    theme_minimal()



# Gerar gráfico de barras comparativo entre as duas distribuições criadas anteriormente
ggplot(faixa_etaria_por_genero, aes(x = TP_FAIXA_ETARIA, y = QTD_SEXO, fill = TP_SEXO)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = comma) +
    labs(
        title = "Comparativo entre Grupos de Genero por Faixa Etaria",
        x = "Faixa Etaria",
        y = "Quantidade",
        fill = "Genero"
    ) +
    theme_minimal()