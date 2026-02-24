# Pós-graduação Lato Sensu em Ciência de Dados e Inteligência Artificial

![Python version](https://img.shields.io/badge/Language-pt--BR-red)
![Python version](https://img.shields.io/badge/Python-3.14-blue)
![Last GitHub commit](https://img.shields.io/github/last-commit/rodolfo-brandao/pos-graduacao)

## Módulos

1. [Introdução, Definições e Exemplos](modulos/modulo-01) (2025)
2. [Linguagem Python com Foco em Análise de Dados](modulos/modulo-02) (2025)
3. [Linguagem R com Foco em Análise de Dados](modulos/modulo-03) (2025)
4. [Estatística - Análise Descritiva, Probabilidade e Inferência com R](modulos/modulo-04) (2025)
5. [Estatística - Análise Descritiva, Probabilidade e Inferência com Python](modulos/modulo-05) (2025)
6. [Álgebra Linear para Ciência de Dados](modulos/modulo-06) (2025)
7. [Banco de Dados - Relacionais, Não Relacionais e Tópicos Avançados](modulos/modulo-07) (2025)
8. [Pré-processamento de Dados](modulos/modulo-08) (2026)

## Ambiente Virtual Python

Para executar os códigos Python deste repositório, é necessário (e recomendado) criar e utilizar um ambiente virtual (`.venv`). Dessa forma, é possível centralizar todas as dependências em um único lugar, além de evitar conflito entre versões.

### Setup inicial

- Clonar o repositório:
```bash
git clone https://github.com/rodolfo-brandao/pos-graduacao.git
```

```bash
cd pos-graduacao
```

- Gerenciar o ambiente virtual
```bash
python3 -m venv .venv
```

```bash
source .venv/bin/activate
```

```bash
pip install -r requirements.txt
```

```bash
pip freeze > requirements.txt
```