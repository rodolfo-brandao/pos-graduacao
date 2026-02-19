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


# ----- Dashboard page config -----
st.set_page_config(
    page_title="Marketplace Dashboard",
    page_icon="📊",
    layout="wide"
)
st.title("📊 Marketplace Dashboard")


st.subheader("Total Products per Marketplace")
row1_col1, row2_col2, row3_col3 = st.columns(3)


total_items_magalu = mysql_service.get_total_products(marketplace_name="magalu")
total_items_ml = mysql_service.get_total_products(marketplace_name="mercado_livre")
total_items = total_items_magalu + total_items_ml

with row1_col1:
    st.metric(
        label="Magazine Luiza",
        value=f"{total_items_magalu:,}"
    )

with row2_col2:
    st.metric(
        label="Mercado Livre",
        value=f"{total_items_ml:,}"
    )

with row3_col3:
    st.metric(
        label="Total",
        value=f"{total_items:,}"
    )



st.subheader("💰 Avg. Product Prices")
row2_col1, row2_col2 = st.columns(2, gap="large")
with row2_col1:
    st.markdown("#### TOP 10 Magazine Luiza")

    magalu_items = mysql_service.get_avg_price_by_product(marketplace_name="magalu")
    magalu_avg_price_fig = chart_factory.plot_bar_chart(
        xlabel="price",
        ylabel="title",
        display_xlabel="Avg. Price (R$)",
        display_ylabel="Product",
        chart_title="Top 10 Products by AVG. Price",
        source=magalu_items
    )

    st.pyplot(magalu_avg_price_fig, width="stretch")

with row2_col2:
    st.markdown("#### TOP 10 Mercado Livre")

    ml_items = mysql_service.get_avg_price_by_product(marketplace_name="mercado_livre")
    ml_avg_price_fig = chart_factory.plot_bar_chart(
        xlabel="price",
        ylabel="title",
        display_xlabel="Avg. Price (R$)",
        display_ylabel="Product",
        chart_title="Top 10 Products by AVG. Price",
        source=ml_items
    )

    st.pyplot(ml_avg_price_fig, width="stretch")
