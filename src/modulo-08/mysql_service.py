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
    Custom service to handle:
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


    def get_total_products(self, marketplace: str) -> int:
        """
        Selects the `COUNT` value from the total of products.

        :param marketplace: The name of the marketplace to be used as filter (e.g. "magalu", "mercado_livre")
        :type marketplace: str
        """

        query: str = """
            SELECT COUNT(*) AS total
            FROM DIM_listing AS listing
            JOIN DIM_marketplace AS marketplace
            ON listing.sk_marketplace = marketplace.sk_marketplace
            WHERE marketplace.nome = %s;
        """
        params: List[str] = [marketplace]

        cursor = self.connection.cursor(dictionary=True)
        cursor.execute(query, params)
        rows = cursor.fetchall()

        total_items: int = int(rows[0].get("total", 0))  # type: ignore (Pylance extension)
        return total_items


    def get_total_products_by_category(self) -> List[Dict[str, int]]:
        """
        Selects the `COUNT` value from the total of products, for each category.
        """

        query: str = """
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


    def get_total_products_by_category_by_marketplace(self, marketplace: str) -> List[Dict[str, int]]:
        """
        Selects the `COUNT` value from the total of products, for each category.

        :param marketplace: The name of the marketplace to be used as filter (e.g. "magalu", "mercado_livre")
        :type marketplace: str
        """

        query: str = """
            SELECT prd_canonico.categoria_canonica AS categoria, COUNT(DIM_listing.titulo) AS total
            FROM DIM_produto_canonico AS prd_canonico
            JOIN BRIDGE_produto_listing AS prd_listing ON prd_canonico.sk_produto = prd_listing.sk_produto
            JOIN DIM_listing ON prd_listing.sk_listing = DIM_listing.sk_listing
            JOIN DIM_marketplace ON DIM_listing.sk_marketplace = DIM_marketplace.sk_marketplace
            WHERE DIM_marketplace.nome = %s
            GROUP BY prd_canonico.categoria_canonica;
        """
        params: List[str] = [marketplace]

        cursor = self.connection.cursor(dictionary=True)
        cursor.execute(query, params)
        rows = cursor.fetchall()

        items = [{ "category": row.get("categoria", ""), "total": int(row.get("total", 0)) } for row in rows]  # type: ignore (Pylance extension)
        return items


    def get_avg_category_prices(self, marketplace: str) -> List[Dict[str, float]]:
        """
        Selects the `AVG` price of the each product category.

        :param marketplace: The name of the marketplace to be used as filter (e.g. "magalu", "mercado_livre")
        :type marketplace: str
        """

        query: str = """
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
        params: List[str] = [marketplace]

        cursor = self.connection.cursor(dictionary=True)
        cursor.execute(query, params)
        rows = cursor.fetchall()

        items = [{ "category": row.get("categoria", ""), "price": float(round(row.get("preco", 0.00), 2)) } for row in rows]  # type: ignore (Pylance extension)
        return items


    def get_top3_best_product_rating(self, marketplace: str) -> List[Dict[str, float]]:
        """
        Selects the TOP 3 best product ratings.

        :param marketplace: The name of the marketplace to be used as filter (e.g. "magalu", "mercado_livre")
        :type marketplace: str
        """

        query: str = """
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
        params: List[str] = [marketplace]

        cursor = self.connection.cursor(dictionary=True)
        cursor.execute(query, params)
        rows = cursor.fetchall()

        items = [{ "product": row.get("nome_canonico", ""), "rating": float(round(row.get("media_avaliacao", 0.00), 2)) } for row in rows]  # type: ignore (Pylance extension)
        return items


    def get_top5_expensive_products_prices(self, marketplace: str) -> List[Dict[str, float]]:
        """
        Selects the TOP 5 most expensive product prices.

        :param marketplace: The name of the marketplace to be used as filter (e.g. "magalu", "mercado_livre")
        :type marketplace: str
        """

        query: str = """
            WITH ultimo_por_listing AS (
                SELECT fp.sk_listing, fp.preco, fp.avaliacao, fp.sk_tempo, ROW_NUMBER() OVER (PARTITION BY fp.sk_listing ORDER BY fp.sk_tempo DESC) AS rn
                FROM FATO_preco fp
                WHERE fp.preco IS NOT NULL AND fp.avaliacao IS NOT NULL AND fp.avaliacao <> -1
            ),
            produto_marketplace AS (
                SELECT m.sk_marketplace, m.nome AS marketplace, p.sk_produto, p.marca_canonico, p.nome_canonico, AVG(u.preco) AS preco_medio, AVG(u.avaliacao) AS media_avaliacao
                FROM ultimo_por_listing u
                JOIN DIM_listing l ON l.sk_listing = u.sk_listing
                JOIN DIM_marketplace m ON m.sk_marketplace = l.sk_marketplace
                JOIN BRIDGE_produto_listing b ON b.sk_listing = l.sk_listing
                JOIN DIM_produto_canonico p ON p.sk_produto = b.sk_produto
                WHERE u.rn = 1
                GROUP BY m.sk_marketplace, m.nome, p.sk_produto, p.marca_canonico, p.nome_canonico
            ),
            ranking AS (
                SELECT pm.*, ROW_NUMBER() OVER (PARTITION BY pm.sk_marketplace ORDER BY pm.preco_medio DESC, pm.media_avaliacao DESC) AS posicao
                FROM produto_marketplace pm
            )
            SELECT
            marketplace, posicao, sk_produto, marca_canonico, nome_canonico, preco_medio, media_avaliacao
            FROM ranking
            WHERE posicao <= 5 and marketplace = %s
            ORDER BY marketplace, posicao;
        """
        params: List[str] = [marketplace]

        cursor = self.connection.cursor(dictionary=True)
        cursor.execute(query, params)
        rows = cursor.fetchall()

        items = [{ "product": row.get("nome_canonico", "")[:15], "price": float(round(row.get("preco_medio", 0.00), 2)) } for row in rows]  # type: ignore (Pylance extension)
        return items


    def get_avg_category_prices_over_time(self, marketplace: str, category: str) -> Dict[str, List[Any]]:
        """
        Selects the `AVG` price a certain category along with the date on which the data was collected.

        :param marketplace: The name of the marketplace to be used as filter (e.g. "magalu", "mercado_livre")
        :type marketplace: str

        :param category: The name of the category of which a product belongs.
        :type marketplace: str
        """

        query: str = """
            SELECT t.data_coleta, pc.categoria_canonica, AVG(fp.preco) AS preco_medio, MIN(fp.preco) AS preco_min, MAX(fp.preco) AS preco_max
            FROM FATO_preco fp
            JOIN DIM_tempo t ON fp.sk_tempo = t.sk_tempo
            JOIN BRIDGE_produto_listing b ON fp.sk_listing = b.sk_listing
            JOIN DIM_produto_canonico pc ON b.sk_produto = pc.sk_produto
            JOIN DIM_listing AS l ON b.sk_listing = l.sk_listing
            JOIN DIM_marketplace AS dim_m ON l.sk_marketplace = dim_m.sk_marketplace
            WHERE fp.em_estoque = 1 AND dim_m.nome = %s AND pc.categoria_canonica = %s
            GROUP BY t.data_coleta, pc.categoria_canonica
            ORDER BY pc.categoria_canonica, t.data_coleta
        """
        params: List[str] = [marketplace, category]

        cursor = self.connection.cursor(dictionary=True)
        cursor.execute(query, params)
        rows = cursor.fetchall()

        dates: List[str] = [str(row.get("data_coleta", ""))[:10] for row in rows]  # type: ignore (Pylance extension)
        avg_prices: List[float] = [float(round(row.get("preco_medio", 0.00), 2)) for row in rows]  # type: ignore (Pylance extension)
        items: Dict[str, List[Any]] = { "dates": dates, "avg_prices": avg_prices }
        return items
