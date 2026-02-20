"""
This is the Streamlit Dashboard entrypoint.


Initial setup:
1. Create a virtual environment:    $ python3 -m venv .venv
2. Activate it:                     $ source .venv/bin/activate
3. Install requirements:            $ pip install -r requirements.txt
4. Freeze requirements:             $ pip freeze > requirements.txt     (optional)


Run this app:
$ cd modulos/modulo-08/
$ streamlit run dashboard.py
"""


import streamlit as st
from mysql_service import MySqlService
from chart_factory import ChartFactory


mysql_service = MySqlService()
chart_factory = ChartFactory()


st.set_page_config(
    page_title="PlusMarket Dashboard",
    page_icon="📊",
    layout="wide"
)
st.title("📊 PlusMarket Dashboard")


# ----- Section 01 -----
st.subheader("🧮 TOTAL DE PRODUTOS")
row1_col1, row2_col2, row3_col3 = st.columns(3)
total_items_magalu = mysql_service.get_total_products_by_marketplace(marketplace="magalu")
total_items_ml = mysql_service.get_total_products_by_marketplace(marketplace="mercado_livre")
total_items = total_items_magalu + total_items_ml

with row1_col1:
    st.metric(
        label="MAGAZINE LUIZA",
        value=f"{total_items_magalu:,}"
    )

with row2_col2:
    st.metric(
        label="MERCADO LIVRE",
        value=f"{total_items_ml:,}"
    )

with row3_col3:
    st.metric(
        label="TOTAL",
        value=f"{total_items:,}"
    )


# ----- Section 02 -----
st.subheader("💰 MÉDIA DE PREÇO POR CATEGORIA")
row2_col1, row2_col2 = st.columns(2, gap="large")
with row2_col1:
    st.markdown("#### MAGAZINE LUIZA")
    magalu_items = mysql_service.get_avg_price_by_product(marketplace="magalu")
    magalu_avg_price_fig = chart_factory.create_bar_chart(
        xlabel="price",
        ylabel="title",
        display_xlabel="Preço Médio",
        display_ylabel="Categoria",
        chart_title="",
        source=magalu_items
    )
    st.pyplot(magalu_avg_price_fig, width="stretch")

with row2_col2:
    st.markdown("#### MERCADO LIVRE")
    ml_items = mysql_service.get_avg_price_by_product(marketplace="mercado_livre")
    ml_avg_price_fig = chart_factory.create_bar_chart(
        xlabel="price",
        ylabel="title",
        display_xlabel="Preço Médio",
        display_ylabel="Categoria",
        chart_title="",
        source=ml_items
    )
    st.pyplot(ml_avg_price_fig, width="stretch")


# ----- Section 03 -----
st.subheader("📦 DISTRIBUIÇÃO POR CATEGORIA")
row3_col1, row3_col2, row3_col3 = st.columns(3, gap="medium")

with row3_col1:
    st.markdown("#### MAGAZINE LUIZA")
    total_by_category_magalu = mysql_service.get_total_products_by_category_by_marketplace(
        marketplace="magalu"
    )
    total_by_category_magalu_fig = chart_factory.create_pie_chart(
        xlabel="total",
        ylabel="category",
        chart_title="",
        source=total_by_category_magalu
    )
    st.pyplot(total_by_category_magalu_fig, width="stretch")

with row3_col2:
    st.markdown("#### MERCADO LIVRE")
    total_by_category_ml = mysql_service.get_total_products_by_category_by_marketplace(
        marketplace="mercado_livre"
    )
    total_by_category_ml_fig = chart_factory.create_pie_chart(
        xlabel="total",
        ylabel="category",
        chart_title="",
        source=total_by_category_ml
    )
    st.pyplot(total_by_category_ml_fig, width="stretch")

with row3_col3:
    st.markdown("#### TOTAL")
    total_items_by_category = mysql_service.get_total_products_by_category()
    total_items_by_category_fig = chart_factory.create_pie_chart(
        xlabel="total",
        ylabel="category",
        chart_title="",
        source=total_items_by_category
    )
    st.pyplot(total_items_by_category_fig, width="stretch")


# ----- Section 04 -----
st.subheader("🏆 TOP 3 PRODUTOS MAIS BEM AVALIADOS")
row4_col1, row4_col2 = st.columns(2, gap="large")

with row4_col1:
    st.markdown("#### MAGAZINE LUIZA")
    top3_magalu = mysql_service.get_top3_product_rating(marketplace="magalu")
    top3_magalu_fig = chart_factory.create_bar_chart(
        xlabel="rating",
        ylabel="product",
        display_xlabel="Produto",
        display_ylabel="Avaliação",
        chart_title="",
        source=top3_magalu,
        horizontal_bars=False
    )
    st.pyplot(top3_magalu_fig, width="stretch")

with row4_col2:
    st.markdown("#### MERCADO LIVRE")
    top3_ml = mysql_service.get_top3_product_rating(marketplace="mercado_livre")
    top3_ml_fig = chart_factory.create_bar_chart(
        xlabel="rating",
        ylabel="product",
        display_xlabel="Produto",
        display_ylabel="Avaliação",
        chart_title="",
        source=top3_ml,
        horizontal_bars=False
    )
    st.pyplot(top3_ml_fig, width="stretch")


# ----- Section 05 -----
st.subheader("🏆 TOP 5 PRODUTOS MAIS CAROS")
row5_col1, row5_col2 = st.columns(2, gap="large")

with row5_col1:
    st.markdown("#### MAGAZINE LUIZA")
    top5_magalu = mysql_service.get_top5_most_expensive_products_by_marketplace(marketplace="magalu")
    top5_magalu_fig = chart_factory.create_bar_chart(
        xlabel="price",
        ylabel="product",
        display_xlabel="Produto",
        display_ylabel="Preço",
        chart_title="",
        source=top5_magalu,
        horizontal_bars=False
    )
    st.pyplot(top5_magalu_fig, width="stretch")

with row5_col2:
    st.markdown("#### MERCADO LIVRE")
    top5_ml = mysql_service.get_top5_most_expensive_products_by_marketplace(marketplace="mercado_livre")
    top5_ml_fig = chart_factory.create_bar_chart(
        xlabel="price",
        ylabel="product",
        display_xlabel="Produto",
        display_ylabel="Preço",
        chart_title="",
        source=top5_ml,
        horizontal_bars=False
    )
    st.pyplot(top5_ml_fig, width="stretch")


# ----- Section 06 -----
st.subheader("📉 VARIAÇÃO DE PREÇO MÉDIO POR CATEGORIA")
row6_col1, row6_col2 = st.columns(2, gap="medium")

with row6_col1:
    st.markdown("#### MAGAZINE LUIZA")
    st.markdown("##### CATEGORIA: TV 📺")
    magalu_tv_price_variance = mysql_service.get_category_price_variance_over_time(
        marketplace="magalu",
        category="tv"
    )
    magalu_tv_price_variance_fig = chart_factory.create_time_series_chart(
        xlabel="date",
        ylabel="avg_price",
        display_xlabel="DATA DA COLETA",
        display_ylabel="PREÇO MÉDIO",
        chart_title="",
        source=magalu_tv_price_variance
    )
    st.pyplot(magalu_tv_price_variance_fig, width="stretch")

with row6_col2:
    st.markdown("#### MERCADO LIVRE")
    st.markdown("##### CATEGORIA: TV 📺")
    magalu_tv_price_variance = mysql_service.get_category_price_variance_over_time(
        marketplace="mercado_livre",
        category="tv"
    )
    magalu_tv_price_variance_fig = chart_factory.create_time_series_chart(
        xlabel="date",
        ylabel="avg_price",
        display_xlabel="DATA DA COLETA",
        display_ylabel="PREÇO MÉDIO",
        chart_title="",
        source=magalu_tv_price_variance
    )
    st.pyplot(magalu_tv_price_variance_fig, width="stretch")
