# CLAUDE.md

## Project Overview
Postgraduate coursework repository (Lato Sensu) in Data Science and AI at
Universidade Tiradentes – Unit (2025–2027). Contains Jupyter notebooks and
Python scripts organized by course module. Content is written in pt-BR;
code comments and variable names may also be in pt-BR — preserve that.

## Project Structure
```
pos-graduacao/
├── assets/         # Images and supporting visual materials
├── datasets/       # Input data files (CSV, Excel, etc.)
├── docker/         # Docker-related configuration files
├── modulos/        # One subfolder per course module (modulo-01, modulo-02, ...)
├── templates/      # Official scientific paper template references from SBC (Sociedade Brasileira de Computação)
├── .env.example    # Reference for required environment variables
├── requirements.txt
└── CLAUDE.md
```

## Modules (Python only — ignore R scripts)
Each `modulos/modulo-XX` folder maps to a course subject:
- `modulo-01` — Intro, Definitions and Examples
- `modulo-02` — Python for Data Analysis
- `modulo-05` — Statistics with Python (Descriptive, Probability, Inference)
- `modulo-06` — Linear Algebra for Data Science
- `modulo-07` — Databases (Relational, Non-Relational, Advanced Topics)
- `modulo-08` — Data Preprocessing
- `modulo-09` — Data Visualization
- `modulo-10` — Machine Learning (Theory)

Modules 03 and 04 are R-only — do not touch them.

## Environment
- Python 3.14, virtual environment managed with `.venv`
- Activate: `source .venv/bin/activate`
- Install deps: `pip install -r requirements.txt`
- Environment variables: copy `.env.example` to `.env` and fill in values
- Never commit `.env` or dataset files

## Key Libraries
- Data manipulation: `pandas`, `numpy`
- Visualization: `matplotlib`, `seaborn`, `plotly`, `altair`
- Machine Learning: `scikit-learn`, `scipy`
- Notebooks: `jupyter`, `ipykernel`
- Database: `mysql-connector-python`
- Apps/dashboards: `streamlit`
- Utilities: `python-dotenv`, `requests`, `beautifulsoup4`

## Code Style
- Follow PEP 8
- Use type hints on function signatures
- Prefer `pathlib.Path` over raw strings for file paths
- Use `logging` instead of `print()` in standalone scripts
- Favor vectorized operations over loops (`pandas`/`numpy`)
- Keep notebook cells focused — one idea per cell
- pt-BR is the natural language for comments, docstrings, and variable names; do not translate or "correct" them to English

## Conventions
- Datasets live in `datasets/` and are never modified in place — always load → transform → save to a new file
- Each module folder is self-contained; do not import across modules
- Reusable utilities should live in `templates/` if they could apply broadly
- Do not create files outside the relevant `modulos/modulo-XX` or `templates/` unless explicitly asked