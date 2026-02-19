"""
This is the Streamlit Dashboard entrypoint.


Initial setup (on "modulos/modulo-08/"):
1. Create a virtual environment:    $ python3 -m venv .venv
2. Activate it:                     $ source .venv/bin/activate
3. Install requirements:            $ pip install -r requirements.txt
4. Freeze requirements:             $ pip freeze > requirements.txt

To run this app:
$ streamlit run dashboard.py
"""


import streamlit as st
from mysql_service import MySqlService
from chart_factory import ChartFactory


mysql_service = MySqlService()
chart_factory = ChartFactory()


st.set_page_config(
    page_title="Marketplace Dashboard",
    page_icon="📊",
    layout="wide"
)
st.title("📊 Marketplace Dashboard")


# ----- Section 01 -----
st.subheader("🧮 TOTAL DE PRODUTOS")
row1_col1, row2_col2, row3_col3 = st.columns(3)
total_items_magalu = mysql_service.get_total_products_by_marketplace(marketplace_name="magalu")
total_items_ml = mysql_service.get_total_products_by_marketplace(marketplace_name="mercado_livre")
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
    magalu_items = mysql_service.get_avg_price_by_product(marketplace_name="magalu")
    magalu_avg_price_fig = chart_factory.plot_bar_chart(
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
    ml_items = mysql_service.get_avg_price_by_product(marketplace_name="mercado_livre")
    ml_avg_price_fig = chart_factory.plot_bar_chart(
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
        marketplace_name="magalu"
    )
    total_by_category_magalu_fig = chart_factory.plot_pie_chart(
        xlabel="total",
        ylabel="category",
        chart_title="",
        source=total_by_category_magalu
    )
    st.pyplot(total_by_category_magalu_fig, width="stretch")

with row3_col2:
    st.markdown("#### MERCADO LIVRE")
    total_by_category_ml = mysql_service.get_total_products_by_category_by_marketplace(
        marketplace_name="mercado_livre"
    )
    total_by_category_ml_fig = chart_factory.plot_pie_chart(
        xlabel="total",
        ylabel="category",
        chart_title="",
        source=total_by_category_ml
    )
    st.pyplot(total_by_category_ml_fig, width="stretch")

with row3_col3:
    st.markdown("#### TOTAL")
    total_items_by_category = mysql_service.get_total_products_by_category()
    total_items_by_category_fig = chart_factory.plot_pie_chart(
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
    top3_magalu = mysql_service.get_top3_product_rating(marketplace_name="magalu")
    top3_magalu_fig = chart_factory.plot_bar_chart(
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
    top3_ml = mysql_service.get_top3_product_rating(marketplace_name="mercado_livre")
    top3_ml_fig = chart_factory.plot_bar_chart(
        xlabel="rating",
        ylabel="product",
        display_xlabel="Produto",
        display_ylabel="Avaliação",
        chart_title="",
        source=top3_ml,
        horizontal_bars=False
    )
    st.pyplot(top3_ml_fig, width="stretch")
