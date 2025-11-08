-- 1) Listar todos os alunos do curso 44 exibindo matrícula,
-- nome e data de nascimento do mais velho ao mais novo.
SELECT
    a.MAT_ALU,
    a.NOM_ALU,
    a.DAT_NASC
FROM ACADEMICO.ALUNOS a
WHERE a.COD_CURSO = 44
ORDER BY a.DAT_NASC DESC;


-- 2) Listar todos os alunos dos cursos 44 e 13 que possuem
-- média superior a 7,00, exibindo código do curso, matrícula
-- e nome do aluno, ordenados por código do curso e nome.
SELECT
    a.COD_CURSO,
    a.MAT_ALU,
    a.NOM_ALU
FROM ACADEMICO.ALUNOS a
WHERE a.COD_CURSO IN (44, 13) AND a.MGP > 7
ORDER BY a.COD_CURSO, a.NOM_ALU;


-- 3)Listar todos alunos que possuem o nome “Maria” na
-- formação de seu nome exibindo matrícula e nome, ordenados pelo nome.
SELECT
    a.MAT_ALU,
    a.NOM_ALU
FROM ACADEMICO.ALUNOS a
WHERE UPPER(a.NOM_ALU) LIKE '%MARIA%';