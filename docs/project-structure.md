# Project Structure

```
.
├── .claude/          # Claude Code config: CLAUDE.md, agents/, rules/
├── assets/           # Images used by the README
├── data/             # Shared datasets (see data/README.md)
├── docs/             # The files imported by .claude/CLAUDE.md
├── src/modulo-XX/    # One self-contained folder per module (01–18)
├── .env.example      # Template for the credentials in .env
├── .python-version   # Python 3.13
├── pyproject.toml    # Dependencies (uv); pylint in the `dev` group
└── uv.lock
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
| 06 | Linear Algebra | `conceitos.ipynb`, `pca.ipynb`, `transformacao_linear.ipynb`, `relatorio-final.md` |
| 07 | Databases (SQL + NoSQL) | `docker-compose.yml`, `sql/`, Neo4j/Redis case studies |
| 08 | Data Preprocessing | Streamlit dashboard + MySQL service + web scraper |
| 09 | Data Visualization | Streamlit dashboard + `sql/` queries + storytelling notebook + presentation PDF |
| 10 | ML Theory | `atividade_01.ipynb`, `relatorio_final.ipynb` |
| 11 | ML Theory & Practice I | Q-learning, Deep Q-learning and MDP value iteration notebooks + `logs/`, `models/`, `videos/` |
| 12 | ML Theory & Practice II | `docker-compose.yml` + `nginx.conf` (Ollama / Open WebUI stack), `relatorio-final.md` |
| 13 | ML Practice & Projects | Final project canvas (PDF only) |
| 14 | Deep Learning — Theory | Report and presentation (PDF only) |
| 15 | Deep Learning — Theory & Practice | `iris.ipynb`, `cifar10.ipynb`, `sentimento.ipynb`, `projeto_final.ipynb` + `models/`, report PDF |
| 16 | Ethics in AI | `atividade-lgpd.md` (LGPD assignment) |
| 17 | Final Project | `projeto-final.md` (link to the external Cinematica repository) |
| 18 | Capstone Paper | LaTeX article (`main.tex`, `references.bib`) on the SBC template |

Modules that were delivered as written work only (13, 14) keep their
final report, canvas or presentation as a PDF in the module folder;
16 and 17 contain only Markdown.

## Datasets

Shared datasets are in `data/`:
- `films.json` — read by the Módulo 01 clustering notebook
- `levantamento_planos_de_saude.csv` — health-plan survey used by the
  Módulo 04 regression scripts
- `marketplace.csv` — marketplace data (not read directly by any
  module; Módulo 08 queries MySQL instead)
- `mock_obras_publicas_se_2022_2026.csv` — read by the Módulo 09
  dashboard

The ENEM microdata CSV is too large for version control and is kept
outside the repository — `data/README.md` has the download link and
the modules that use it (02–05).

Módulo 15 notebooks download their datasets at runtime (CIFAR-10,
Fashion-MNIST, a Hugging Face sentiment dataset) instead of reading
from `data/`.

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

## Módulo 17 and 18 — Capstone

The capstone project, **Cinematica** (a graph-backed RAG system with AI
agents generating Cypher queries), lives in its own repository:
https://github.com/rodolfo-brandao/cinematica. This repository only
holds the link (Módulo 17) and the accompanying SBC-template paper
sources (Módulo 18), which are also synced to Overleaf.
