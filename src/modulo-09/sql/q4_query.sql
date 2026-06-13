WITH municipios_story AS (
    SELECT 'ANTES DO COVID' AS periodo, 'BRUMADINHO' AS municipio, 'MG' AS uf
    UNION ALL SELECT 'ANTES DO COVID', 'BARRA DE SÃO FRANCISCO', 'ES'
    UNION ALL SELECT 'ANTES DO COVID', 'BARRA DE SAO FRANCISCO', 'ES'
    UNION ALL SELECT 'ANTES DO COVID', 'SALVADOR', 'BA'
    UNION ALL SELECT 'ANTES DO COVID', 'ITAÓCA', 'SP'
    UNION ALL SELECT 'ANTES DO COVID', 'ITAOCA', 'SP'
    UNION ALL SELECT 'ANTES DO COVID', 'LADAINHA', 'MG'
    UNION ALL SELECT 'DEPOIS DO COVID', 'RIO DE JANEIRO', 'RJ'
    UNION ALL SELECT 'DEPOIS DO COVID', 'NATAL', 'RN'
    UNION ALL SELECT 'DEPOIS DO COVID', 'BRASÍLIA', 'DF'
    UNION ALL SELECT 'DEPOIS DO COVID', 'BRASILIA', 'DF'
    UNION ALL SELECT 'DEPOIS DO COVID', 'MACAPÁ', 'AP'
    UNION ALL SELECT 'DEPOIS DO COVID', 'MACAPA', 'AP'
    UNION ALL SELECT 'DEPOIS DO COVID', 'BRAGANÇA', 'PA'
    UNION ALL SELECT 'DEPOIS DO COVID', 'BRAGANCA', 'PA'
),
desastres_base AS (
    SELECT
        ms.periodo,
        fd.uf,
        fd.municipio,
        SUM(COALESCE(fd.dh_mortos, 0)) AS total_mortes,
        MAX(fd.ano) AS ultimo_ano_desastre
    FROM fato_desastre fd
    JOIN municipios_story ms
      ON UPPER(TRIM(fd.municipio)) = UPPER(TRIM(ms.municipio))
     AND UPPER(TRIM(fd.uf)) = UPPER(TRIM(ms.uf))
    WHERE fd.status = 'Reconhecido'
    GROUP BY
        ms.periodo,
        fd.uf,
        fd.municipio
),
emendas_depois AS (
    SELECT
        UPPER(TRIM(localidade)) AS municipio_ref,
        UPPER(TRIM(sigla_uf_gasto)) AS uf_ref,
        ano_emenda,
        SUM(COALESCE(valor_pago, 0)) AS total_pago
    FROM fato_emenda
    WHERE localidade IS NOT NULL
      AND sigla_uf_gasto IS NOT NULL
    GROUP BY
        UPPER(TRIM(localidade)),
        UPPER(TRIM(sigla_uf_gasto)),
        ano_emenda
)
SELECT
    d.periodo,
    d.uf,
    d.municipio,
    d.total_mortes,
    COALESCE(SUM(e.total_pago), 0) AS valor_pago_depois,
    CASE
        WHEN COALESCE(SUM(e.total_pago), 0) > 0 THEN 'SIM'
        ELSE 'NAO'
    END AS houve_emenda_depois
FROM desastres_base d
LEFT JOIN emendas_depois e
       ON e.municipio_ref = UPPER(TRIM(d.municipio))
      AND e.uf_ref = UPPER(TRIM(d.uf))
      AND e.ano_emenda > d.ultimo_ano_desastre
GROUP BY
    d.periodo,
    d.uf,
    d.municipio,
    d.total_mortes
ORDER BY
    d.periodo,
    valor_pago_depois DESC,
    d.total_mortes DESC;