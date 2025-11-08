-- 1) Obter a quantidade de alunos cadastrados por curso, exibindo
-- código do curso e quantidade de alunos ordenados pelo código do curso.
SELECT
    c.COD_CURSO,
    COUNT(*) AS "TOTAL_ALUNOS"
FROM ACADEMICO.ALUNOS a
JOIN ACADEMICO.CURSOS c
    ON a.COD_CURSO = c.COD_CURSO
GROUP BY c.COD_CURSO
ORDER BY c.COD_CURSO;


-- 2) Obter a quantidade de disciplinas que possuem mais do que 10 alunos
-- matriculados, exibindo código da disciplina e quantidade de alunos ordenado
-- pela quantidade de alunos.
SELECT * FROM ACADEMICO.DISCIPLINAS;
SELECT * FROM ACADEMICO.CURSOS;
SELECT * FROM ACADEMICO.TURMAS;


-- 3) Exibir a maior e menor mgp dos alunos da universidade.


-- 4) Obter a quantidade de alunos matriculados por turma.