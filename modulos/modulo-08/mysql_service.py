import os
from dotenv import load_dotenv
import mysql.connector
from typing import Dict, List


load_dotenv()
MYSQL_HOST=os.getenv(key="MYSQL_HOST")
MYSQL_PORT=os.getenv(key="MYSQL_PORT")
MYSQL_USER=os.getenv(key="MYSQL_USER")
MYSQL_PASS=os.getenv(key="MYSQL_PASS")
MYSQL_DB=os.getenv(key="MYSQL_DB")


class MySqlService:
    """
    Custom MySQL service to handle:
    - Database connection
    - SQL queries
    """

    def __init__(self) -> None:
        self.connection = mysql.connector.connect(
            host=MYSQL_HOST,
            port=MYSQL_PORT,
            user=MYSQL_USER,
            password=MYSQL_PASS,
            database=MYSQL_DB,
            ssl_disabled=False
        )


    def get_total_products(self, marketplace_name: str) -> int:
        """
        Selects the `COUNT(*)` value from the total of products from the given marketplace.

        :param marketplace_name: The name of the respective marketplace.
        :type marketplace_name: str

        :return: The total of products of a specific marketplace.
        :rtype: int
        """

        query: str = """
            SELECT COUNT(*) AS total
            FROM DIM_listing AS listing
            JOIN DIM_marketplace AS marketplace
            ON listing.sk_marketplace = marketplace.sk_marketplace
            WHERE marketplace.nome = %s
        """
        params: List[str] = [marketplace_name]

        cursor = self.connection.cursor(dictionary=True)
        cursor.execute(query, params)
        rows = cursor.fetchall()

        total_items: int = int(rows[0].get("total", 0))  # type: ignore (Pylance extension)
        return total_items


    def get_avg_price_by_product(self, marketplace_name: str) -> List[Dict[str, float]]:
        """
        Selects the `AVG` price of the each product from the given marketplace.

        :param marketplace_name: The name of the respective marketplace.
        :type marketplace_name: str

        :return: A list with top 10 product avg. prices sorted in descending order.
        :rtype: List[Dict[str, float]]
        """

        query = """
            SELECT LEFT(listing.titulo, 15) AS titulo, AVG(preco.preco) AS preco
            FROM `FATO_preco` AS preco
            JOIN `DIM_listing` AS listing
            ON preco.sk_listing = listing.sk_listing
            JOIN `DIM_marketplace` AS marketplace
            ON listing.sk_marketplace = marketplace.sk_marketplace
            WHERE marketplace.nome = %s and listing.categoria is not NULL
            GROUP BY listing.titulo
            ORDER BY AVG(preco.preco) DESC
        """
        params: List[str] = [marketplace_name]

        cursor = self.connection.cursor(dictionary=True)
        cursor.execute(query, params)
        rows = cursor.fetchall()

        items = [{ "title": item.get("titulo", ""), "price": round(item.get("preco", 0.00), 2) } for item in rows[:10]]  # type: ignore (Pylance extension)
        return items
