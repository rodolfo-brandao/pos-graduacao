-- GET TOTAL PRODUCTS BY MARKETPLACE
SELECT COUNT(*) AS total
FROM DIM_listing AS listing
JOIN DIM_marketplace AS marketplace
ON listing.sk_marketplace = marketplace.sk_marketplace
WHERE marketplace.nome = 'magalu'


-- GET TOTAL PRODUCTS BY CATEGORY
SELECT categoria.nome AS categoria, COUNT(DIM_listing.titulo) AS total
FROM DIM_categoria AS categoria
JOIN DIM_produto_canonico AS prd_canonico ON categoria.sk_categoria = prd_canonico.sk_categoria
JOIN BRIDGE_produto_listing AS prd_listing ON prd_canonico.sk_produto = prd_listing.sk_produto
JOIN DIM_listing ON prd_listing.sk_listing = DIM_listing.sk_listing
GROUP BY categoria.nome


-- GET TOTAL PRODUCTS BY CATEGORY BY MARKETPLACE
SELECT categoria.nome AS categoria, COUNT(DIM_listing.titulo) AS total
FROM DIM_categoria AS categoria
JOIN DIM_produto_canonico AS prd_canonico ON categoria.sk_categoria = prd_canonico.sk_categoria
JOIN BRIDGE_produto_listing AS prd_listing ON prd_canonico.sk_produto = prd_listing.sk_produto
JOIN DIM_listing ON prd_listing.sk_listing = DIM_listing.sk_listing
JOIN DIM_marketplace ON DIM_listing.sk_marketplace = DIM_marketplace.sk_marketplace
WHERE DIM_marketplace.nome = 'magalu'
GROUP BY categoria.nome


-- GET AVG PRICE BY PRODUCT
SELECT LEFT(listing.titulo, 15) AS titulo, AVG(preco.preco) AS preco
FROM `FATO_preco` AS preco
JOIN `DIM_listing` AS listing
ON preco.sk_listing = listing.sk_listing
JOIN `DIM_marketplace` AS marketplace
ON listing.sk_marketplace = marketplace.sk_marketplace
WHERE marketplace.nome = 'magalu' and listing.categoria is not NULL
GROUP BY listing.titulo
ORDER BY AVG(preco.preco) DESC