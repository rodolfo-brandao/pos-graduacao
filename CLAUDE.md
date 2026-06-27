# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## About This Repository

Academic repository for a Lato Sensu Post-Graduate program in Data Science and AI (Portuguese: Pós-graduação em Ciência de Dados e Inteligência Artificial). All code is written manually — Claude Code is used strictly for analysis, insights, and decision support, never to generate code committed to this repo.

## Environment Setup

Python version is managed by `.python-version` (3.14). Dependencies are managed with `uv`:

```bash
uv sync          # create .venv and install all dependencies
uv add <pkg>     # add a new dependency
uv remove <pkg>  # remove a dependency
```

## Running Streamlit Dashboards

Both dashboards must be run from the **repository root**:

```bash
# Módulo 08 – PlusMarket marketplace data (requires MySQL via .env)
streamlit run modulos/modulo-08/dashboard.py

# Módulo 09 – Obras públicas de Sergipe (reads from data/mock_obras_publicas_se_2022_2026.csv)
streamlit run modulos/modulo-09/obras_sergipe_dashboard.py
```

## Running Jupyter Notebooks

Launch from the repository root so relative dataset paths resolve correctly:

```bash
jupyter notebook
# or
jupyter lab
```

## Docker Services (Módulo 07)

```bash
# NoSQL bundle: Redis, MongoDB, Cassandra, Neo4j
docker compose -f docker/nosql-bundle-compose.yml up -d

# Oracle DB
docker compose -f docker/oracle-compose.yml up -d
```

## Environment Variables

Copy `.env.example` to `.env` and fill in credentials before running Módulo 08's dashboard or scraper. Required variables: `MYSQL_HOST`, `MYSQL_PORT`, `MYSQL_USER`, `MYSQL_PASS`, `MYSQL_DB`, `MYSQL_CONN_STR`.

## Repository Structure

Each module lives under `modulos/modulo-XX/` and is self-contained:

| Module | Topic | Main artifacts |
|--------|-------|---------------|
| 01 | Intro & Definitions | `clustering.ipynb` |
| 02 | Python for Data Analysis | `microdados_enem_2023.ipynb` |
| 03 | R for Data Analysis | `.R` scripts + `plots/` |
| 04 | Statistics with R | `.R` scripts (regression) |
| 05 | Statistics with Python | `microdados_enem_2023.ipynb` |
| 06 | Linear Algebra | `pca.ipynb`, `transformacao_linear.ipynb` |
| 07 | Databases (SQL + NoSQL) | `.sql`, `.cypher` scripts, Jupyter notebooks for Neo4j/Redis |
| 08 | Data Preprocessing | Streamlit dashboard + MySQL service + web scraper |
| 09 | Data Visualization | Streamlit dashboard + SQL queries + storytelling notebook |
| 10 | ML Theory | `atividade_01.ipynb`, `relatorio_final.ipynb` |
| 11 | ML Theory & Practice | Q-learning + MDP value iteration notebooks |

Shared datasets are in `data/`. The `docker/` folder holds Compose files for local database services.

## Módulo 08 Architecture

The module follows a layered pattern:
- `magalu_scraper.py` — scrapes product data and writes JSON files to `JSON/<date>/`
- `mysql_service.py` — `MySqlService` class wrapping all MySQL queries (reads `.env` for connection)
- `dashboard.py` — Streamlit entrypoint that calls `MySqlService` and renders charts

## Custom Agents

A `code-reviewer` agent is configured in `.claude/agents/code-reviewer.md`. It performs quality, security, and best-practices review of changed files, grouping findings by severity (critical / major / minor).

## Custom Rules

The file `.claude/rules/code-writing.md` contains rules, best practices, and conventions to be **strictly** followed when writing Python code. So, when prompted, use it.