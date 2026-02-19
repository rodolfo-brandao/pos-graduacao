import os
from dotenv import load_dotenv
import mysql.connector
from typing import Any, Dict, List


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


    def get_total_products_by_marketplace(self, marketplace_name: str) -> int:
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
            WHERE marketplace.nome = %s;
        """
        params: List[str] = [marketplace_name]

        cursor = self.connection.cursor(dictionary=True)
        cursor.execute(query, params)
        rows = cursor.fetchall()

        total_items: int = int(rows[0].get("total", 0))  # type: ignore (Pylance extension)
        return total_items


    def get_total_products_by_category(self) -> List[Dict[str, int]]:
        """
        Selects the total of each product by their category.

        :return: A list with the categories and their respective amount of products.
        :rtype: List[Dict[str, int]]
        """

        query = """
            SELECT prd_canonico.categoria_canonica AS categoria, COUNT(DIM_listing.titulo) AS total
            FROM DIM_produto_canonico AS prd_canonico
            JOIN BRIDGE_produto_listing AS prd_listing ON prd_canonico.sk_produto = prd_listing.sk_produto
            JOIN DIM_listing ON prd_listing.sk_listing = DIM_listing.sk_listing
            GROUP BY prd_canonico.categoria_canonica;
        """

        cursor = self.connection.cursor(dictionary=True)
        cursor.execute(query)
        rows = cursor.fetchall()

        items = [{ "category": row.get("categoria", ""), "total": int(row.get("total", 0)) } for row in rows]  # type: ignore (Pylance extension)
        return items


    def get_total_products_by_category_by_marketplace(self, marketplace_name: str) -> List[Dict[str, int]]:
        """
        Selects the total of each product by their category, by marketplace.

        :param marketplace_name: The name of the respective marketplace.
        :type marketplace_name: str

        :return: A list with the categories and their respective amount of products.
        :rtype: List[Dict[str, int]]
        """

        query = """
            SELECT prd_canonico.categoria_canonica AS categoria, COUNT(DIM_listing.titulo) AS total
            FROM DIM_produto_canonico AS prd_canonico
            JOIN BRIDGE_produto_listing AS prd_listing ON prd_canonico.sk_produto = prd_listing.sk_produto
            JOIN DIM_listing ON prd_listing.sk_listing = DIM_listing.sk_listing
            JOIN DIM_marketplace ON DIM_listing.sk_marketplace = DIM_marketplace.sk_marketplace
            WHERE DIM_marketplace.nome = %s
            GROUP BY prd_canonico.categoria_canonica;
        """
        params: List[str] = [marketplace_name]

        cursor = self.connection.cursor(dictionary=True)
        cursor.execute(query, params)
        rows = cursor.fetchall()

        items = [{ "category": row.get("categoria", ""), "total": int(row.get("total", 0)) } for row in rows]  # type: ignore (Pylance extension)
        return items


    def get_avg_price_by_product(self, marketplace_name: str) -> List[Dict[str, float]]:
        """
        Selects the `AVG` price of the each product from the given marketplace.

        :param marketplace_name: The name of the respective marketplace.
        :type marketplace_name: str

        :return: A list with the product avg. prices sorted in descending order.
        :rtype: List[Dict[str, float]]
        """

        query = """
            SELECT prd_canonico.categoria_canonica AS categoria, AVG(preco.preco) AS preco
            FROM FATO_preco AS preco
            JOIN DIM_listing AS listing ON preco.sk_listing = listing.sk_listing
            JOIN DIM_marketplace AS marketplace ON listing.sk_marketplace = marketplace.sk_marketplace
            JOIN BRIDGE_produto_listing AS prd_listing ON listing.sk_listing = prd_listing.sk_listing
            JOIN DIM_produto_canonico AS prd_canonico ON prd_listing.sk_produto = prd_canonico.sk_produto
            WHERE marketplace.nome = %s
            GROUP BY prd_canonico.categoria_canonica
            ORDER BY AVG(preco.preco) DESC;
        """
        params: List[str] = [marketplace_name]

        cursor = self.connection.cursor(dictionary=True)
        cursor.execute(query, params)
        rows = cursor.fetchall()

        items = [{ "title": row.get("categoria", ""), "price": round(row.get("preco", 0.00), 2) } for row in rows]  # type: ignore (Pylance extension)
        return items


    def get_top3_product_rating(self, marketplace_name: str) -> List[Dict[str, Any]]:
        """
        Selects the TOP 3 best product rating for the given marketplace.

        :param marketplace_name: The name of the respective marketplace.
        :type marketplace_name: str

        :return: A list with the product ratings, along with the respective marketplace.
        :rtype: List[Dict[str, Any]]
        """

        query = """
            WITH ultimo_preco AS (
                SELECT fp.sk_listing, fp.avaliacao, ROW_NUMBER() OVER (PARTITION BY fp.sk_listing ORDER BY fp.sk_tempo DESC) AS rn
                FROM FATO_preco fp
                WHERE fp.avaliacao IS NOT NULL
            ),
            avaliacao_produto AS (
                SELECT m.sk_marketplace, m.nome AS marketplace, p.sk_produto, p.nome_canonico, p.marca_canonico, AVG(u.avaliacao) AS media_avaliacao
                FROM ultimo_preco u
                JOIN DIM_listing l ON l.sk_listing = u.sk_listing
                JOIN DIM_marketplace m ON m.sk_marketplace = l.sk_marketplace
                JOIN BRIDGE_produto_listing b ON b.sk_listing = l.sk_listing
                JOIN DIM_produto_canonico p ON p.sk_produto = b.sk_produto
                WHERE u.rn = 1
                GROUP BY m.sk_marketplace, m.nome, p.sk_produto, p.nome_canonico, p.marca_canonico
            ),
            ranking AS (
                SELECT ap.*, ROW_NUMBER() OVER (PARTITION BY ap.sk_marketplace ORDER BY ap.media_avaliacao DESC) AS posicao
                FROM avaliacao_produto ap
            )
            SELECT marketplace, posicao, sk_produto, marca_canonico, nome_canonico, media_avaliacao
            FROM ranking
            WHERE posicao <= 3 and marketplace = %s
            ORDER BY marketplace, posicao;
        """
        params: List[str] = [marketplace_name]

        cursor = self.connection.cursor(dictionary=True)
        cursor.execute(query, params)
        rows = cursor.fetchall()

        items = [{ "product": row.get("nome_canonico", "")[:20], "rating": round(row.get("media_avaliacao", 0.00), 2) } for row in rows]  # type: ignore (Pylance extension)
        return items
