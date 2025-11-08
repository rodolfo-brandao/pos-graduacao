-- List all table names from a specific schema:
SELECT table_name
FROM all_tables
WHERE owner = 'ACADEMICO'
ORDER BY table_name;