SELECT
    cobrade,
    uf,
    municipio,
    SUM(dh_mortos) AS total_mortes
FROM fato_desastre fd
WHERE status = 'Reconhecido'
GROUP BY cobrade, uf, municipio
ORDER BY total_mortes DESC
LIMIT 5;