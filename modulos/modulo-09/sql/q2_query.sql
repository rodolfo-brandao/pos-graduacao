SELECT
    uf,
    municipio,
    COUNT(*) AS ocorrencias
FROM fato_desastre
WHERE status = 'Reconhecido'
GROUP BY uf, municipio
ORDER BY ocorrencias DESC;