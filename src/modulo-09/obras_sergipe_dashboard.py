"""
This is the Streamlit Dashboard entrypoint.

[venv] On root folder, run:
    $ streamlit run modulos/modulo-09/dashboard.py
"""


import pandas as pd
import streamlit as st


df = pd.read_csv(
    filepath_or_buffer="data/mock_obras_publicas_se_2022_2026.csv"
)


st.set_page_config(
    page_title="Obras de Sergipe – Dashboard",
    page_icon="📊",
    layout="wide",
)
st.title(
    body="📊 Obras de Sergipe – Dashboard"
)
st.markdown(
    body="⚠️ Aviso: Os dados apresentados aqui são puramente fictícios, gerados por Inteligência Artificial."
)


# ===== Section 01 =====
st.subheader("🚧 Obras Atrasadas por Secretaria")

(
    sec1_col1,
    sec1_col2,
    sec1_col3
) = st.columns(3, gap="medium", border=True)

with sec1_col1:
    st.markdown("#### 🗓️ Total de Dias")

    temp_df = (
        df.groupby("secretaria_responsavel")["dias_de_atraso"]
        .sum()
        .reset_index()
    )

    st.bar_chart(
        data=temp_df,
        x="secretaria_responsavel",
        y="dias_de_atraso",
        x_label="Dias Atrasados",
        y_label="Secretaria",
        sort="dias_de_atraso",
        color="#1E90FF",  # dodger blue
        horizontal=True,
        height=300
    )

with sec1_col2:
    st.markdown("#### 💰 Orçamento Inicial")

    temp_df = (
        df.groupby("secretaria_responsavel")["orcamento_inicial_reais"]
        .sum()
        .reset_index()
    )

    st.bar_chart(
        data=temp_df,
        x="secretaria_responsavel",
        y="orcamento_inicial_reais",
        x_label="Orçamento",
        y_label="Secretaria",
        sort="orcamento_inicial_reais",
        color="#1E90FF",  # dodger blue
        horizontal=True,
        height=300
    )

with sec1_col3:
    st.markdown("#### 💸 Orçamento Adicional")

    temp_df = (
        df.groupby("secretaria_responsavel")["valor_aditivos_reais"]
        .sum()
        .reset_index()
    )

    st.bar_chart(
        data=temp_df,
        x="secretaria_responsavel",
        y="valor_aditivos_reais",
        x_label="Orçamento",
        y_label="Secretaria",
        sort="valor_aditivos_reais",
        color="#1E90FF",  # dodger blue
        horizontal=True,
        height=300
    )
