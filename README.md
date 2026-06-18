# Vigilância Sindrômica de Síndromes Febris

Quarto Book metodológico e executável para demonstrar um pipeline de vigilância sindrômica de síndromes febris a partir de atendimentos de saúde com CID, queixa e texto clínico.

O projeto combina três camadas de classificação:

1. CID
2. Regex
3. LLM

A saída final é uma classificação sindrômica única por atendimento, mantendo rastreabilidade da decisão, comparação entre camadas e critérios de auditoria.

## Objetivo

Produzir uma entrega profissional, reprodutível e apresentável para gestores e equipe técnica, antes do recebimento da base real.

O material pode servir como:

- relatório metodológico;
- protótipo executável;
- especificação para rotina operacional;
- base para um futuro aplicativo Shiny;
- base para validação com especialistas.

## Estrutura do projeto

```text
quarto_sindromes_febris/
├── _quarto.yml
├── README.md
├── run_pipeline.R
├── .Renviron.example
├── index.qmd
├── 01-contexto.qmd
├── 02-definicao-sindromes.qmd
├── 03-arquitetura-classificacao.qmd
├── 04-preparacao-dados.qmd
├── 05-classificacao-cid.qmd
├── 06-classificacao-regex.qmd
├── 07-classificacao-llm.qmd
├── 08-comparacao-cid-regex-llm.qmd
├── 09-classificacao-final.qmd
├── 10-series-historicas.qmd
├── 11-auditoria-limitacoes.qmd
├── 12-proximos-passos-produto.qmd
├── R/
├── data/
├── outputs/
└── assets/
```

## Pré-requisitos

O projeto foi desenvolvido em R e Quarto.

Pacotes principais:

```r
install.packages(c(
  "dplyr",
  "tibble",
  "stringr",
  "stringi",
  "purrr",
  "readr",
  "lubridate",
  "ggplot2",
  "rlang",
  "tidyr",
  "DBI",
  "duckdb",
  "knitr",
  "httr2",
  "jsonlite",
  "quarto"
))
```

## Como executar

Na raiz do projeto, rode:

```r
source("run_pipeline.R")
```

Ou, diretamente:

```r
quarto::quarto_render()
```

## Saídas principais

O DuckDB local é salvo em:

```text
outputs/sindromes_febris.duckdb
```

Tabelas principais:

```text
tb_atendimentos
tb_classificacao_cid
tb_classificacao_regex
tb_classificacao_llm
tb_comparacao_camadas
tb_classificacao_final
tb_execucoes
```

## Configuração da LLM

O projeto pode rodar em modo mock, sem API e sem modelo local.

Copie `.Renviron.example` para `.Renviron` e ajuste as variáveis conforme necessário.

### Modo mock

```text
LLM_PROVIDER=mock
LLM_MODEL=mock-sindromico-v1
```

### Ollama local

```text
LLM_PROVIDER=ollama
LLM_MODEL=mistral:latest
LLM_BASE_URL=http://localhost:11434
```

### Gemini

```text
LLM_PROVIDER=gemini
LLM_MODEL=gemini-1.5-flash
LLM_API_KEY=sua_chave
```

### OpenAI-compatible

```text
LLM_PROVIDER=openai_compatible
LLM_MODEL=gpt-4o-mini
LLM_BASE_URL=https://api.openai.com/v1
LLM_API_KEY=sua_chave
```

## Observações sobre dados reais

Este repositório usa base sintética. Não publique bases reais com dados sensíveis no GitHub.

Para adaptar a dados reais, a base deve ser convertida para o schema interno mínimo:

```text
record_id
data_atendimento
unidade
idade
faixa_etaria
sexo
cid
cid_nome
queixa
anamnese
texto_clinico
texto_clinico_norm
```

## Próximos passos

- Validar com especialistas.
- Medir desempenho por síndrome.
- Adaptar ingestão para base real.
- Criar versão Shiny operacional.
- Automatizar execução e relatórios.
- Evoluir para vigilância pré-sindrômica com embeddings.
```

