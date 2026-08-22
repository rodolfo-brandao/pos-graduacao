# Instructions

This is an Academic repository for a Lato Sensu Post-Graduate program in
Data Science and AI and this file provides guidance to Claude Code
(claude.ai/code) when working in it.

The author and sole contributor is [@rodolfo-brandao](https://github.com/rodolfo-brandao).
LLM-generated code is always reviewed by him before being committed;
Claude Code is also used as an analysis and decision-support tool.

Also, consult these files for important details regarding this repo:
- `docs/project-structure.md` — module layout, datasets and architecture
- `docs/custom-agents.md` — agents available under `.claude/agents/`
- `docs/custom-rules.md` — mandatory code-writing conventions

## Environment Setup

Python version is managed by `.python-version` (3.13, pinned to `<3.14`
in `pyproject.toml` because of TensorFlow). Dependencies are managed
with `uv`:

```bash
uv sync          # create .venv and install all dependencies
uv add <pkg>     # add a new dependency
uv remove <pkg>  # remove a dependency
```

## Running Streamlit Dashboards

Both dashboards read their data using paths relative to the current
working directory, so they must be run from the **repository root**:

```bash
# Módulo 08 – PlusMarket marketplace data (requires MySQL via .env)
streamlit run src/modulo-08/dashboard.py

# Módulo 09 – Obras públicas de Sergipe
# (reads data/mock_obras_publicas_se_2022_2026.csv)
streamlit run src/modulo-09/obras_sergipe_dashboard.py
```

> Note: `src/modulo-08` requires `mysql-connector-python`, which is not
> yet declared in `pyproject.toml`. Run `uv add mysql-connector-python`
> before using that dashboard or its service layer.

## Running Jupyter Notebooks

```bash
jupyter notebook
# or
jupyter lab
```

Notebooks resolve datasets relative to their **own** folder — for
example `../../data/films.json` from `src/modulo-01/` — so they work
regardless of where the server was launched from.

## Docker Services

Compose files live inside the module that uses them:

```bash
# Módulo 07 – Oracle XE, CloudBeaver, Redis, MongoDB, Cassandra, Neo4j
docker compose -f src/modulo-07/docker-compose.yml up -d

# Módulo 12 – Ollama + Open WebUI behind an Nginx API gateway
docker compose -f src/modulo-12/docker-compose.yml up -d
```

The Módulo 12 stack reserves NVIDIA GPUs for the `ollama` service and
will not start as-is on a machine without them.

## Environment Variables

Copy `.env.example` to `.env` and fill in the credentials before running
the modules that depend on them:

```bash
cp .env.example .env
```

Módulo 08 (MySQL):
- `MYSQL_HOST`
- `MYSQL_PORT`
- `MYSQL_USER`
- `MYSQL_PASS`
- `MYSQL_DB`
- `MYSQL_CONN_STR`

Módulo 12 (MongoDB Atlas):
- `MONGODB_ATLAS_CONN_STRING`
