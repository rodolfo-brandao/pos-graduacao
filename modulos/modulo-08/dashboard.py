"""
This is the Streamlit Dashboard entrypoint.


To run this app:
- On root folder, activate the .venv
- Run: $ streamlit modulos/modulo-08/dashboard.py
"""


import pandas as pd
import streamlit as st
from mysql_service import MySqlService


mysql_service = MySqlService()


st.set_page_config(
    page_title="PlusMarket Dashboard",
    page_icon="📊",
    layout="wide"
)
st.title("📊 PlusMarket Dashboard")


# ----- Section 01 -----
st.subheader("🧮 Total de Produtos")
row1_col1, row2_col2, row3_col3 = st.columns(3)

with row1_col1:
    total_items_magalu = mysql_service.get_total_products(marketplace="magalu")
    st.metric(
        label="🔵 Magazine Luiza",
        value=f"{total_items_magalu:,}".replace(',', '.')
    )

with row2_col2:
    total_items_ml = mysql_service.get_total_products(marketplace="mercado_livre")
    st.metric(
        label="🟡 Mercado Livre",
        value=f"{total_items_ml:,}".replace(',', '.')
    )

with row3_col3:
    total_items = total_items_magalu + total_items_ml
    st.metric(
        label="⚪️ Total",
        value=f"{total_items:,}".replace(',', '.')
    )


st.divider()


# ----- Section 02 -----
st.subheader("📦 Total de Produtos por Categoria")
row3_col1, row3_col2, row3_col3 = st.columns(3, gap="medium")

with row3_col1:
    st.markdown("#### 🔵 Magazine Luiza")
    total_by_category_magalu = mysql_service.get_total_products_by_category_by_marketplace(
        marketplace="magalu"
    )

    st.bar_chart(
        data=pd.DataFrame(data=total_by_category_magalu),
        x="category",
        y="total",
        x_label="Categoria",
        y_label="Total",
        sort="total",
        color="#1E90FF"  # dodger blue
    )


with row3_col2:
    st.markdown("#### 🟡 Mercado Livre")
    total_by_category_ml = mysql_service.get_total_products_by_category_by_marketplace(
        marketplace="mercado_livre"
    )

    st.bar_chart(
        data=pd.DataFrame(data=total_by_category_ml),
        x="category",
        y="total",
        x_label="Categoria",
        y_label="Total",
        sort="total",
        color="#FFEE8C"  # pastel yellow
    )

with row3_col3:
    st.markdown("#### ⚪️ Total")
    total_items_by_category = mysql_service.get_total_products_by_category()

    st.bar_chart(
        data=pd.DataFrame(data=total_items_by_category),
        x="category",
        y="total",
        x_label="Categoria",
        y_label="Total",
        sort="total",
        color="#FFFFFF"  # white
    )


st.divider()


# ----- Section 03 -----
st.subheader("💰 Média de Preço por Categoria")
row2_col1, row2_col2 = st.columns(2, gap="large")

with row2_col1:
    st.markdown("#### 🔵 Magazine Luiza")
    magalu_items = mysql_service.get_avg_category_prices(marketplace="magalu")

    st.bar_chart(
        data=pd.DataFrame(data=magalu_items),
        x="category",
        y="price",
        x_label="Categoria",
        y_label="Preço Médio",
        sort="price",
        color="#1E90FF"  # dodger blue
    )

with row2_col2:
    st.markdown("#### 🟡 Mercado Livre")
    ml_items = mysql_service.get_avg_category_prices(marketplace="mercado_livre")

    st.bar_chart(
        data=pd.DataFrame(data=ml_items),
        x="category",
        y="price",
        x_label="Categoria",
        y_label="Preço Médio",
        sort="price",
        color="#FFEE8C"  # pastel yellow
    )


st.divider()


# ----- Section 04 -----
st.subheader("🏆 Top 3 Produtos mais bem Avaliados")
row4_col1, row4_col2 = st.columns(2, gap="large")

with row4_col1:
    st.markdown("#### 🔵 Magazine Luiza")
    top3_magalu = mysql_service.get_top3_best_product_rating(marketplace="magalu")

    st.bar_chart(
        data=pd.DataFrame(data=top3_magalu),
        x="product",
        y="rating",
        x_label="Produto",
        y_label="Avaliação",
        sort="rating",
        color="#1E90FF",  # dodger blue
        horizontal=True
    )

with row4_col2:
    st.markdown("#### 🟡 Mercado Livre")
    top3_ml = mysql_service.get_top3_best_product_rating(marketplace="mercado_livre")

    st.bar_chart(
        data=pd.DataFrame(data=top3_ml),
        x="product",
        y="rating",
        x_label="Produto",
        y_label="Avaliação",
        sort="rating",
        color="#FFEE8C",  # pastel yellow
        horizontal=True
    )


st.divider()


# ----- Section 05 -----
st.subheader("🤑 Top 5 Produtos mais Caros")
row5_col1, row5_col2 = st.columns(2, gap="large")

with row5_col1:
    st.markdown("#### 🔵 Magazine Luiza")
    top5_magalu = mysql_service.get_top5_expensive_products_prices(marketplace="magalu")

    st.bar_chart(
        data=pd.DataFrame(data=top5_magalu),
        x="product",
        y="price",
        x_label="Produto",
        y_label="Preço",
        sort="price",
        color="#1E90FF",  # dodger blue
        horizontal=True
    )

with row5_col2:
    st.markdown("#### 🟡 Mercado Livre")
    top5_ml = mysql_service.get_top5_expensive_products_prices(marketplace="mercado_livre")

    st.bar_chart(
        data=pd.DataFrame(data=top5_magalu),
        x="product",
        y="price",
        x_label="Produto",
        y_label="Preço",
        sort="price",
        color="#FFEE8C",  # pastel yellow
        horizontal=True
    )


st.divider()


# ----- Section 06 -----
st.subheader("📈 Variação de Preço Médio por Categoria")
st.markdown("💡 DICA: Utilize a caixa de seleção ao lado")
selectbox_options = [
    "Caixa de Som",
    "Celular",
    "Fone",
    "Notebook",
    "Smartwatch",
    "Tablet",
    "Teclado",
    "TV"
]
selected_category = st.sidebar.selectbox(
    "CATEGORIAS",
    selectbox_options,
    index=0
)
row6_col1, row6_col2 = st.columns(2, gap="medium")

with row6_col1:
    st.markdown("#### 🔵 Magazine Luiza")
    st.markdown(f"##### ➡️ Categoria Selecionada: {selected_category.upper()}")

    magalu_price_variance = mysql_service.get_avg_category_prices_over_time(
        marketplace="magalu",
        category=selected_category.lower()
    )

    st.line_chart(
        data=magalu_price_variance,
        color="#1E90FF"  # dodger blue
    )

with row6_col2:
    st.markdown("#### 🟡 Mercado Livre")
    st.markdown(f"##### ➡️ Categoria selecionada: {selected_category.upper()}")

    ml_price_variance = mysql_service.get_avg_category_prices_over_time(
        marketplace="mercado_livre",
        category=selected_category.lower()
    )

    st.line_chart(
        data=ml_price_variance,
        color="#FFEE8C"  # pastel yellow
    )
