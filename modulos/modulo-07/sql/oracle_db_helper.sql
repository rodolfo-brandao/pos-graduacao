-- [Oracle DB] List all table names from a specific schema:
SELECT table_name
FROM all_tables
WHERE owner = 'ACADEMICO'  -- Schema name
ORDER BY table_name;