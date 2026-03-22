SELECT
    cobrade AS desastre,
    COUNT(*) AS ocorrencias
FROM fato_desastre
WHERE status = 'Reconhecido'
GROUP BY cobrade
ORDER BY ocorrencias DESC
LIMIT 10;