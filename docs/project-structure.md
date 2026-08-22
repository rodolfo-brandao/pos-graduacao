# Project Structure

```
.
├── .claude/          # Claude Code config: CLAUDE.md, agents, rules
├── assets/           # Images used by the README
├── data/             # Shared datasets (see data/README.md)
├── docs/             # The files imported by .claude/CLAUDE.md
└── src/modulo-XX/    # One self-contained folder per module
```

Each module lives under `src/modulo-XX/` and is self-contained. Every
module folder has a `README.md` describing the subject and the
professor who taught it.

| Module | Topic | Main artifacts |
|--------|-------|---------------|
| 01 | Intro & Definitions | `clustering.ipynb` |
| 02 | R for Data Analysis | `.R` scripts + `plots/` |
| 03 | Python for Data Analysis | `microdados_enem_2023.ipynb` |
| 04 | Statistics with R | `.R` scripts (linear/logistic regression) + `plots/` |
| 05 | Statistics with Python | `microdados_enem_2023.ipynb` |
| 06 | Linear Algebra | `conceitos.ipynb`, `pca.ipynb`, `transformacao_linear.ipynb` |
| 07 | Databases (SQL + NoSQL) | `docker-compose.yml`, `sql/`, Neo4j/Redis case studies |
| 08 | Data Preprocessing | Streamlit dashboard + MySQL service + web scraper |
| 09 | Data Visualization | Streamlit dashboard + `sql/` queries + storytelling notebook |
| 10 | ML Theory | `atividade_01.ipynb`, `relatorio_final.ipynb` |
| 11 | ML Theory & Practice I | Q-learning, Deep Q-learning and MDP value iteration notebooks |
| 12 | ML Theory & Practice II | `docker-compose.yml` + `nginx.conf` (Ollama / Open WebUI stack) |
| 13 | Deep Learning — Theory | Report and presentation (PDF only) |
| 14 | Deep Learning — Theory & Practice | `iris.ipynb` |

Modules that were delivered as written work only (12, 13) keep their
final report or presentation as a PDF in the module folder.

## Datasets

Shared datasets are in `data/`. The ENEM microdata CSV is too large for
version control and is kept outside the repository — `data/README.md`
has the download link and the modules that use it.

## Módulo 08 Architecture

The module follows a layered pattern:
- `magalu_scraper.py` — scrapes product data and writes JSON files to
  `JSON/<date>/`
- `mysql_service.py` — `MySqlService` class wrapping all MySQL queries
  (reads `.env` for connection)
- `dashboard.py` — Streamlit entrypoint that calls `MySqlService` and
  renders charts
- `schema/data-warehouse-model.png` — the data warehouse model the
  service queries against

## Módulo 12 Architecture

An Nginx container acts as an API gateway on port `8080`, proxying `/`
to Open WebUI and `/v1/` to Ollama behind a bearer-token check.
