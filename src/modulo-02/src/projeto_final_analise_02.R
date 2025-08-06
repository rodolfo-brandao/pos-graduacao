# PROJETO FINAL: Microdados Enem 2023 - Nordeste
# AUTOR: Rodolfo Brandão

# PROPOSTA:
# Conduzir uma análise sobre a evasão de inscritos, correlacionando
# os estados. Essa análise deve, inicialmente, metrificar a quantidade
# de pessoas que estiveram presentes para fazer a prova, junto com o
# montante de pessoas que estiveram ausentes durante a realização da mesma,
# além da quantidade daqueles que foram eliminados. Num segundo momento,
# trazer um apanhado dos top 5 estados com maior taxa de evasão.



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
status_provas_por_estado = microdados_enem_2023_ne %>%
    select(
        SG_UF_PROVA, TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT
    )



# Consolidar, em um novo dataframe, os valores das colunas "TP_PRESENCA_CN", "TP_PRESENCA_CH", "TP_PRESENCA_LC" e "TP_PRESENCA_MT"
# em uma nova única coluna, chamada "ST_GERAL", para indicar quando:
# 1. A pessoa esteve presente em todas as 4 provas: P (presente)
# 2. A pessoa faltou, pelo menos, uma das 4 provas: F (faltou)
# 3. A pessoa foi eliminada em, pelo menos, uma das 4 provas: E (eliminado)
# 4. Caso contrário, a coluna nova recebe "NA"
status_provas_por_estado_consolidado <- status_provas_por_estado %>%
    mutate(
        ST_GERAL = case_when(
            TP_PRESENCA_CN == "Presente na prova" & TP_PRESENCA_CH == "Presente na prova" &
            TP_PRESENCA_LC == "Presente na prova" & TP_PRESENCA_MT == "Presente na prova" ~ "P",

            TP_PRESENCA_CN == "Faltou à prova" | TP_PRESENCA_CH == "Faltou à prova" |
            TP_PRESENCA_LC == "Faltou à prova" | TP_PRESENCA_MT == "Faltou à prova" ~ "F",

            TP_PRESENCA_CN == "Eliminado na prova" | TP_PRESENCA_CH == "Eliminado na prova" |
            TP_PRESENCA_LC == "Eliminado na prova" | TP_PRESENCA_MT == "Eliminado na prova" ~ "E",

            TRUE ~ NA_character_
        )
    ) %>%
    select(SG_UF_PROVA, ST_GERAL)



# Agrupar por estado:
status_provas_por_estado <- status_provas_por_estado_consolidado %>%
    count(SG_UF_PROVA, ST_GERAL, name = "QTD_PESSOAS")



# Separar estados por situação de pessoas presentes (P):
pessoas_presentes_por_estado = status_provas_por_estado %>%
    filter(ST_GERAL == "P")

# Gerar gráfico de barras para visualizar a distribuição de pessoas presentes nas 4 provas, por estado:
ggplot(pessoas_presentes_por_estado, aes(x = SG_UF_PROVA, y = QTD_PESSOAS)) +
    geom_col(fill = "cornflowerblue") +
    labs(
        title = "Pessoas Presentes em Todas as Provas",
        x = "UF",
        y = "Quantidade"
    ) +
    theme_minimal()



# Não houve incidência alguma de pessoas faltantes (F). Logo, não houve evasão.



# Separar estados por situação de pessoas eliminadas (E):
pessoas_eliminadas_por_estado = status_provas_por_estado %>%
    filter(ST_GERAL == "E")

ggplot(pessoas_eliminadas_por_estado, aes(x = SG_UF_PROVA, y = QTD_PESSOAS)) +
    geom_col(fill = "cornflowerblue") +
    labs(
        title = "Pessoas Eliminadas",
        x = "UF",
        y = "Quantidade"
    ) +
    theme_minimal()



# Gerar gráfico de barras comparativo entre pessoas presentes e eliminadas por estado:
ggplot(status_provas_por_estado, aes(x = SG_UF_PROVA, y = QTD_PESSOAS, fill = ST_GERAL)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = comma) +
    labs(
        title = "Comparativo entre Pessoas Presentes e Eliminadas",
        x = "UF",
        y = "Quantidade",
        fill = "Status"
    ) +
    theme_minimal()



# Gerar gráfico de pizza com os top 5 estados com a maior quantidade de pessoas eliminadas,
# uma vez que não foi identificada nenhuma evasão (pessoas faltantes):

top5_estados_eliminacoes = pessoas_eliminadas_por_estado %>%
    arrange(desc(QTD_PESSOAS)) %>%
    slice_head(n = 5)

ggplot(top5_estados_eliminacoes, aes(x = "", y = QTD_PESSOAS, fill = SG_UF_PROVA)) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    labs(
        title = "Top 5 Estados com mais Pessoas Eliminadas",
        fill = "UF"
    ) +
    theme_void()