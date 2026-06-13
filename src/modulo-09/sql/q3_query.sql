SELECT
    cobrade,
    uf,
    municipio,
    SUM(dh_mortos) AS total_mortes
FROM fato_desastre
WHERE
    status = 'Reconhecido'
    AND ano BETWEEN 2014 and 2019 -- pré COVID
GROUP BY cobrade, uf, municipio
ORDER BY total_mortes DESC
LIMIT 5;