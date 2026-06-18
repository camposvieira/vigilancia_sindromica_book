# Instalação, configuração, execução e uso com novos dados

Este documento descreve como instalar, configurar e executar o projeto do Quarto Book de vigilância sindrômica de síndromes febris.

O objetivo é permitir que uma pessoa que receba o repositório consiga:

1. instalar os requisitos necessários;
2. renderizar o Quarto Book com dados sintéticos;
3. entender o papel do DuckDB no projeto;
4. executar a camada LLM em modo simulado ou com modelo real;
5. incluir novos dados;
6. adaptar o pipeline para dados reais;
7. versionar e reproduzir a análise.

---

# 1. Visão geral do projeto

Este projeto implementa um pipeline reprodutível para classificação sindrômica de atendimentos clínicos, com foco em síndromes febris.

A proposta combina três camadas principais:

1. **CID**
   Classificação baseada em códigos diagnósticos estruturados.

2. **Regex**
   Classificação baseada em termos e padrões textuais explícitos no texto clínico.

3. **LLM**
   Classificação baseada em interpretação semântica do texto clínico por modelo de linguagem.

As três camadas são comparadas e consolidadas em uma classificação final auditável.

A versão atual usa dados sintéticos para demonstrar a metodologia. O projeto foi desenhado para ser posteriormente adaptado a dados reais.

---

# 2. Modos de execução

O projeto pode ser executado em dois modos principais.

## 2.1. Modo padrão reprodutível: `mock`

Este é o modo recomendado para a entrega principal.

Nesse modo, a camada LLM é simulada. Isso permite renderizar o Quarto Book sem depender de:

* internet;
* chave de API;
* Ollama;
* modelo local;
* GPU;
* conta em serviço externo.

Esse modo é ideal para reprodução, revisão metodológica e versionamento no GitHub.

## 2.2. Modo com LLM real

Opcionalmente, a camada LLM pode ser executada com um modelo real.

Os provedores previstos são:

* `ollama`;
* `gemini`;
* `openai_compatible`;
* `mock`.

O teste local realizado neste projeto utilizou Ollama/Qwen em ambiente com GPU NVIDIA GeForce RTX 4060 Laptop GPU.

---

# 3. Requisitos gerais

## 3.1. Obrigatórios

Para executar o projeto no modo padrão, são necessários:

* R;
* Quarto;
* pacotes R do projeto;
* Git, se a pessoa for versionar ou clonar o repositório.

## 3.2. Recomendados

* RStudio;
* navegador para abrir o HTML renderizado;
* GitHub Desktop ou Git CLI, se for usar GitHub.

## 3.3. O que não é obrigatório

Não é necessário instalar o DuckDB como programa externo para rodar este projeto.

O projeto usa DuckDB por meio do pacote R:

```r
duckdb
```

Portanto, basta instalar o pacote R `duckdb`. O arquivo de banco local é criado e acessado pelo próprio R.

A instalação do DuckDB CLI é opcional e só seria útil para inspecionar o banco fora do R.

---

# 4. Instalação do R, Quarto e pacotes

## 4.1. R

É necessário ter R instalado.

Para verificar:

```bash
R --version
```

Ou, dentro do R:

```r
R.version.string
```

## 4.2. Quarto

É necessário ter Quarto instalado.

Para verificar no terminal:

```bash
quarto --version
```

Ou no R:

```r
quarto::quarto_version()
```

## 4.3. Pacotes R

Os pacotes principais do projeto são carregados em:

```text
R/00_pacotes.R
```

A lista mínima esperada inclui:

```r
pacotes <- c(
  "DBI",
  "duckdb",
  "dplyr",
  "tidyr",
  "stringr",
  "stringi",
  "purrr",
  "tibble",
  "readr",
  "lubridate",
  "ggplot2",
  "scales",
  "jsonlite",
  "httr2",
  "glue",
  "knitr",
  "quarto"
)
```

Para instalar pacotes ausentes:

```r
pacotes <- c(
  "DBI",
  "duckdb",
  "dplyr",
  "tidyr",
  "stringr",
  "stringi",
  "purrr",
  "tibble",
  "readr",
  "lubridate",
  "ggplot2",
  "scales",
  "jsonlite",
  "httr2",
  "glue",
  "knitr",
  "quarto"
)

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]

if (length(instalar) > 0) {
  install.packages(instalar)
}
```

Depois, testar:

```r
source("R/00_pacotes.R")
```

Se não houver erro, o ambiente básico está funcionando.

---

# 5. Estrutura do projeto

A estrutura esperada do repositório é:

```text
quarto_sindromes_febris/
├── _quarto.yml
├── README.md
├── run_pipeline.R
├── .Renviron.example
├── .gitignore
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
│   ├── 00_pacotes.R
│   ├── 01_config_sindromes.R
│   ├── 02_input_normalization.R
│   ├── 03_generate_synthetic_data.R
│   ├── 04_data_quality.R
│   ├── 04_duckdb.R
│   ├── 05_cid_classification.R
│   ├── 06_regex_classification.R
│   ├── 07_llm_clients.R
│   ├── 08_llm_classification.R
│   ├── 09_comparacao_camadas.R
│   ├── 10_classificacao_final.R
│   ├── 11_series_historicas.R
│   ├── 12_auditoria_limitacoes.R
│   └── 13_produto_entrega.R
├── data/
│   └── atendimentos_sinteticos.csv
├── outputs/
│   └── sindromes_febris.duckdb
├── assets/
│   └── styles.css
├── scripts/
│   └── teste_llm_qwen_metricas.R
└── docs/
    └── INSTALACAO_E_EXECUCAO.md
```

---

# 6. Papel do DuckDB no projeto

O DuckDB é usado como banco local do pipeline.

O arquivo principal é:

```text
outputs/sindromes_febris.duckdb
```

Esse arquivo guarda as tabelas intermediárias e finais geradas pelo pipeline.

## 6.1. Preciso instalar DuckDB na máquina?

Não necessariamente.

Para este projeto, basta instalar o pacote R:

```r
install.packages("duckdb")
```

O pacote R já permite criar, ler e escrever o banco `.duckdb`.

## 6.2. Tabelas principais

As principais tabelas persistidas são:

```text
tb_atendimentos
tb_classificacao_cid
tb_classificacao_regex
tb_classificacao_llm
tb_comparacao_camadas
tb_classificacao_final
tb_execucoes
```

## 6.3. Verificar o conteúdo do banco

No R:

```r
source("R/00_pacotes.R")
source("R/04_duckdb.R")

con <- connect_duckdb()

duckdb_counts(con)

DBI::dbDisconnect(con, shutdown = TRUE)
```

## 6.4. Erro comum: lock do DuckDB

Se aparecer erro como:

```text
Could not set lock on file outputs/sindromes_febris.duckdb
```

isso significa que o banco está aberto em outra sessão do R/RStudio.

Para resolver:

```r
DBI::dbDisconnectAll()
gc()
```

Se persistir, reiniciar a sessão do RStudio:

```r
.rs.restartR()
```

Ou remover arquivos temporários:

```bash
rm -f outputs/*.duckdb.wal
rm -f outputs/*.duckdb.tmp
```

---

# 7. Execução padrão com dados sintéticos

Este é o fluxo recomendado para reproduzir a entrega principal.

## 7.1. Configurar LLM em modo `mock`

No R:

```r
Sys.setenv(
  LLM_PROVIDER = "mock",
  LLM_MODEL = "mock-sindromico-v1",
  LLM_BASE_URL = "",
  LLM_JSON_MODE = "true",
  LLM_TIMEOUT_SEC = "120",
  LLM_MAX_RECORDS = "200",
  RUN_REAL_LLM_IN_RENDER = "false"
)
```

## 7.2. Fechar conexões antigas

```r
DBI::dbDisconnectAll()
gc()
```

## 7.3. Renderizar o Quarto Book

```r
quarto::quarto_render()
```

Ou pelo terminal:

```bash
quarto render
```

## 7.4. Abrir o HTML

O resultado será criado em:

```text
_book/index.html
```

No Linux:

```bash
xdg-open _book/index.html
```

---

# 8. Execução por linha de comando

No terminal:

```bash
cd caminho/para/quarto_sindromes_febris
quarto render
```

Ou:

```bash
Rscript run_pipeline.R
```

A renderização do Quarto Book é a forma recomendada de gerar a documentação completa, pois executa os capítulos na ordem definida em `_quarto.yml`.

---

# 9. Camada LLM

A camada LLM é configurada por variáveis de ambiente.

| Variável                 | Descrição                                                                           |
| ------------------------ | ----------------------------------------------------------------------------------- |
| `LLM_PROVIDER`           | Provedor da camada LLM. Pode ser `mock`, `ollama`, `gemini` ou `openai_compatible`. |
| `LLM_MODEL`              | Nome do modelo.                                                                     |
| `LLM_BASE_URL`           | URL base do provedor, quando aplicável.                                             |
| `LLM_API_KEY`            | Chave de API, quando aplicável.                                                     |
| `LLM_JSON_MODE`          | Controla se a chamada força modo JSON no provedor.                                  |
| `LLM_TIMEOUT_SEC`        | Timeout da chamada em segundos.                                                     |
| `LLM_MAX_RECORDS`        | Número máximo de registros processados pela camada LLM no render.                   |
| `RUN_REAL_LLM_IN_RENDER` | Controla se o Quarto Book pode chamar LLM real durante a renderização.              |

---

# 10. Modo `mock`

Este é o modo padrão e recomendado para reprodução.

```r
Sys.setenv(
  LLM_PROVIDER = "mock",
  LLM_MODEL = "mock-sindromico-v1",
  LLM_BASE_URL = "",
  LLM_JSON_MODE = "true",
  LLM_TIMEOUT_SEC = "120",
  LLM_MAX_RECORDS = "200",
  RUN_REAL_LLM_IN_RENDER = "false"
)
```

Características:

* não usa internet;
* não exige API;
* não exige GPU;
* não exige Ollama;
* é mais rápido;
* é reprodutível;
* é adequado para entrega no GitHub.

---

# 11. Uso opcional com Ollama/Qwen

## 11.1. Requisitos adicionais

Para usar LLM real local com Ollama, é necessário:

* Ollama instalado;
* modelo local baixado;
* serviço do Ollama ativo;
* memória suficiente para o modelo;
* GPU NVIDIA recomendada, mas não obrigatória dependendo do modelo.

## 11.2. Verificar Ollama

No terminal:

```bash
ollama list
```

Se o comando funcionar, o Ollama está instalado e acessível.

## 11.3. Baixar modelo

Exemplo:

```bash
ollama pull qwen3.5:latest
```

Depois, confirmar o nome exato:

```bash
ollama list
```

O nome usado em `LLM_MODEL` precisa ser exatamente igual ao nome listado pelo Ollama.

## 11.4. Configuração testada

```r
Sys.setenv(
  LLM_PROVIDER = "ollama",
  LLM_MODEL = "qwen3.5:latest",
  LLM_BASE_URL = "http://localhost:11434",
  LLM_JSON_MODE = "false",
  LLM_TIMEOUT_SEC = "180",
  LLM_MAX_RECORDS = "30",
  RUN_REAL_LLM_IN_RENDER = "true"
)
```

Para o Qwen testado, `LLM_JSON_MODE=false` foi mais estável. Nessa configuração, o modelo retorna o conteúdo principal no campo `response`, e o parser do pipeline extrai o JSON.

## 11.5. Teste mínimo

```r
source("R/00_pacotes.R")
source("R/07_llm_clients.R")

llm_config <- make_llm_config()

resp <- llm_generate(
  prompt = 'Responda exclusivamente este JSON: {"ok": true}',
  config = llm_config
)

resp
```

Resposta esperada:

```json
{"ok": true}
```

---

# 12. Validação local com Qwen/Ollama

Foi realizado um teste local com Ollama/Qwen em ambiente com GPU NVIDIA GeForce RTX 4060 Laptop GPU, com aproximadamente 8 GB de VRAM.

A amostra testada teve 30 registros sintéticos.

Principais métricas observadas:

| Métrica                   |                              Valor |
| ------------------------- | ---------------------------------: |
| Provider                  |                             ollama |
| Modelo                    |                     qwen3.5:latest |
| Registros processados     |                                 30 |
| Respostas com JSON válido |                                 30 |
| Falhas de parse           |                                  0 |
| Proporção de `parse_ok`   |                               100% |
| Tempo total de execução   |                             1596 s |
| Tempo médio por registro  |                             53,2 s |
| GPU                       | NVIDIA GeForce RTX 4060 Laptop GPU |
| VRAM total                |                            8188 MB |
| VRAM usada antes          |                              72 MB |
| VRAM usada depois         |                            5338 MB |
| Utilização da GPU antes   |                                 0% |
| Utilização da GPU depois  |                                95% |
| Temperatura da GPU antes  |                              37 °C |
| Temperatura da GPU depois |                              61 °C |

Esse teste demonstra que a arquitetura permite substituir o modo simulado por um modelo real local.

Esse resultado não deve ser interpretado como validação final do classificador. A validação final requer base real, revisão por especialistas e avaliação sistemática de desempenho.

---

# 13. Incluir novos dados

O projeto foi desenvolvido com dados sintéticos. Para incluir novos dados, é necessário garantir que a base de entrada tenha os campos mínimos esperados pelo pipeline.

## 13.1. Campos mínimos recomendados

A base de entrada deve conter, idealmente:

| Campo              | Tipo             | Descrição                                     |
| ------------------ | ---------------- | --------------------------------------------- |
| `record_id`        | texto            | Identificador único do atendimento.           |
| `data_atendimento` | data ou datetime | Data do atendimento.                          |
| `cid`              | texto            | Código CID-10, quando disponível.             |
| `cid_nome`         | texto            | Descrição do CID, quando disponível.          |
| `texto_clinico`    | texto            | Texto clínico consolidado para classificação. |
| `idade`            | numérico         | Idade do paciente, se disponível.             |
| `sexo`             | texto            | Sexo, se disponível.                          |
| `unidade`          | texto            | Unidade de atendimento, se disponível.        |
| `municipio`        | texto            | Município, se disponível.                     |
| `bairro`           | texto            | Bairro, se disponível.                        |

O campo mais importante para a camada textual é:

```text
texto_clinico
```

Ele pode ser construído pela concatenação de campos como:

```text
queixa
anamnese
evolucao
motivo_atendimento
asv_queixa_principal
soap_subjetivo_motivo
```

## 13.2. Formato recomendado para teste inicial

Para testar novos dados, o formato mais simples é CSV.

Exemplo:

```text
data/atendimentos_reais_teste.csv
```

Com colunas:

```text
record_id,data_atendimento,cid,cid_nome,texto_clinico,idade,sexo,unidade,municipio,bairro
```

## 13.3. Exemplo de leitura de novos dados

No R:

```r
dados_novos <- readr::read_csv(
  "data/atendimentos_reais_teste.csv",
  show_col_types = FALSE
)
```

Checar colunas:

```r
names(dados_novos)
```

Checar amostra:

```r
dplyr::glimpse(dados_novos)
```

## 13.4. Padronização mínima dos novos dados

Exemplo:

```r
dados_novos_padronizados <- dados_novos |>
  dplyr::mutate(
    record_id = as.character(record_id),
    data_atendimento = as.Date(data_atendimento),
    cid = as.character(cid),
    cid_nome = as.character(cid_nome),
    texto_clinico = as.character(texto_clinico),
    idade = suppressWarnings(as.numeric(idade)),
    sexo = as.character(sexo),
    unidade = as.character(unidade),
    municipio = as.character(municipio),
    bairro = as.character(bairro)
  )
```

Se algum campo não existir, pode ser criado como `NA`:

```r
dados_novos_padronizados <- dados_novos_padronizados |>
  dplyr::mutate(
    cid = if ("cid" %in% names(dados_novos_padronizados)) cid else NA_character_,
    cid_nome = if ("cid_nome" %in% names(dados_novos_padronizados)) cid_nome else NA_character_,
    idade = if ("idade" %in% names(dados_novos_padronizados)) idade else NA_real_,
    sexo = if ("sexo" %in% names(dados_novos_padronizados)) sexo else NA_character_,
    unidade = if ("unidade" %in% names(dados_novos_padronizados)) unidade else NA_character_,
    municipio = if ("municipio" %in% names(dados_novos_padronizados)) municipio else NA_character_,
    bairro = if ("bairro" %in% names(dados_novos_padronizados)) bairro else NA_character_
  )
```

---

# 14. Rodar o pipeline com novos dados

A forma mais segura de testar dados novos é criar um script separado, sem alterar imediatamente os capítulos do Quarto Book.

Sugestão de arquivo:

```text
scripts/testar_novos_dados.R
```

Conteúdo sugerido:

```r
# scripts/testar_novos_dados.R ---------------------------------------------

source("R/00_pacotes.R")
source("R/04_duckdb.R")
source("R/05_cid_classification.R")
source("R/06_regex_classification.R")
source("R/07_llm_clients.R")
source("R/08_llm_classification.R")
source("R/09_comparacao_camadas.R")
source("R/10_classificacao_final.R")

# Caminho do arquivo novo
arquivo_entrada <- "data/atendimentos_reais_teste.csv"

# Leitura
dados <- readr::read_csv(
  arquivo_entrada,
  show_col_types = FALSE
)

# Padronização mínima
dados <- dados |>
  dplyr::mutate(
    record_id = as.character(record_id),
    data_atendimento = as.Date(data_atendimento),
    cid = if ("cid" %in% names(dados)) as.character(cid) else NA_character_,
    cid_nome = if ("cid_nome" %in% names(dados)) as.character(cid_nome) else NA_character_,
    texto_clinico = as.character(texto_clinico),
    idade = if ("idade" %in% names(dados)) suppressWarnings(as.numeric(idade)) else NA_real_,
    sexo = if ("sexo" %in% names(dados)) as.character(sexo) else NA_character_,
    unidade = if ("unidade" %in% names(dados)) as.character(unidade) else NA_character_,
    municipio = if ("municipio" %in% names(dados)) as.character(municipio) else NA_character_,
    bairro = if ("bairro" %in% names(dados)) as.character(bairro) else NA_character_
  )

# Configuração segura da LLM em mock
Sys.setenv(
  LLM_PROVIDER = "mock",
  LLM_MODEL = "mock-sindromico-v1",
  LLM_BASE_URL = "",
  LLM_JSON_MODE = "true",
  LLM_TIMEOUT_SEC = "120",
  LLM_MAX_RECORDS = "200",
  RUN_REAL_LLM_IN_RENDER = "false"
)

llm_config <- make_llm_config()

# Classificações
classificacao_cid <- classify_by_cid(dados)
classificacao_regex <- classify_by_regex(dados)

classificacao_llm <- classify_by_llm(
  dados = dados,
  config = llm_config,
  max_records = as.integer(Sys.getenv("LLM_MAX_RECORDS", unset = "200"))
)

comparacao <- build_comparacao_camadas(
  atendimentos = dados,
  classificacao_cid = classificacao_cid,
  classificacao_regex = classificacao_regex,
  classificacao_llm = classificacao_llm
)

classificacao_final <- build_classificacao_final(comparacao)

# Salvar resultados em CSV para inspeção
readr::write_csv(
  classificacao_final,
  "outputs/classificacao_final_novos_dados.csv"
)

readr::write_csv(
  comparacao,
  "outputs/comparacao_camadas_novos_dados.csv"
)

# Resumos rápidos
print(
  classificacao_final |>
    dplyr::count(sindrome_final, sort = TRUE)
)

print(
  classificacao_final |>
    dplyr::count(fonte_classificacao_final, sort = TRUE)
)
```

Executar:

```bash
Rscript scripts/testar_novos_dados.R
```

---

# 15. Persistir novos dados no DuckDB do projeto

Depois de testar os dados novos em CSV, é possível persistir a base no DuckDB.

No R:

```r
source("R/00_pacotes.R")
source("R/04_duckdb.R")

con <- connect_duckdb()

write_atendimentos(
  con = con,
  dados = dados_novos_padronizados,
  overwrite = TRUE
)

duckdb_counts(con)

DBI::dbDisconnect(con, shutdown = TRUE)
```

Atenção: usar `overwrite = TRUE` substitui a tabela anterior `tb_atendimentos`.

Para evitar perda dos dados sintéticos, recomenda-se fazer cópia do banco antes:

```bash
cp outputs/sindromes_febris.duckdb outputs/sindromes_febris_backup.duckdb
```

---

# 16. Usar dados reais no Quarto Book

Há duas estratégias.

## 16.1. Estratégia segura: criar um banco separado

Criar um banco específico para teste real, por exemplo:

```text
outputs/sindromes_febris_real_teste.duckdb
```

Isso evita sobrescrever o banco sintético da entrega.

Para isso, o projeto precisa permitir configurar o caminho do banco via variável de ambiente, por exemplo:

```r
Sys.setenv(
  DUCKDB_PATH = "outputs/sindromes_febris_real_teste.duckdb"
)
```

Se a função `connect_duckdb()` já aceitar caminho customizado, usar:

```r
con <- connect_duckdb(
  db_path = Sys.getenv("DUCKDB_PATH", unset = "outputs/sindromes_febris.duckdb")
)
```

Se ainda não aceitar, recomenda-se adaptar `R/04_duckdb.R` para ler `DUCKDB_PATH`.

## 16.2. Estratégia simples: substituir a tabela de atendimentos

Essa opção é mais simples, mas menos segura.

1. Fazer backup:

```bash
cp outputs/sindromes_febris.duckdb outputs/sindromes_febris_sintetico_backup.duckdb
```

2. Gravar dados reais em `tb_atendimentos`.

3. Renderizar novamente.

4. Restaurar o banco sintético se necessário.

---

# 17. Adaptação para bases reais

Bases reais raramente chegam com os nomes de colunas exatamente iguais aos esperados pelo pipeline.

Por isso, recomenda-se criar um script de preparação específico para cada fonte.

Exemplo:

```text
scripts/preparar_dados_reais.R
```

Esse script deve:

1. ler a base original;
2. selecionar os campos necessários;
3. renomear colunas;
4. criar `record_id`;
5. criar `texto_clinico`;
6. normalizar `cid`;
7. converter datas;
8. remover registros sem texto e sem CID;
9. salvar arquivo padronizado;
10. opcionalmente gravar no DuckDB.

## 17.1. Exemplo genérico

```r
# scripts/preparar_dados_reais.R -------------------------------------------

source("R/00_pacotes.R")
source("R/04_duckdb.R")

arquivo_original <- "data/base_real_original.csv"
arquivo_padronizado <- "data/atendimentos_reais_teste.csv"

dados_raw <- readr::read_csv(
  arquivo_original,
  show_col_types = FALSE
)

dados_padronizados <- dados_raw |>
  dplyr::transmute(
    record_id = as.character(id_atendimento),
    data_atendimento = as.Date(data_atendimento),
    cid = as.character(cid10),
    cid_nome = as.character(descricao_cid),
    texto_clinico = stringr::str_squish(paste(
      queixa_principal,
      anamnese,
      evolucao,
      sep = " "
    )),
    idade = suppressWarnings(as.numeric(idade)),
    sexo = as.character(sexo),
    unidade = as.character(nome_unidade),
    municipio = as.character(municipio),
    bairro = as.character(bairro)
  ) |>
  dplyr::filter(
    !is.na(record_id),
    !is.na(data_atendimento),
    !(is.na(texto_clinico) | texto_clinico == "")
  )

readr::write_csv(
  dados_padronizados,
  arquivo_padronizado
)

con <- connect_duckdb()

write_atendimentos(
  con = con,
  dados = dados_padronizados,
  overwrite = TRUE
)

DBI::dbDisconnect(con, shutdown = TRUE)
```

Os nomes `id_atendimento`, `cid10`, `descricao_cid`, `queixa_principal`, `anamnese` e `evolucao` são exemplos. Eles devem ser trocados pelos nomes reais da base recebida.

---

# 18. Cuidados com dados reais

Antes de usar dados reais, observar:

* não versionar dados identificáveis no GitHub;
* não subir prontuários, textos clínicos reais ou identificadores pessoais;
* usar amostras anonimizadas para teste;
* avaliar necessidade de autorização institucional;
* remover CPF, CNS, nome, endereço, telefone e outros identificadores diretos;
* quando possível, usar identificadores pseudonimizados;
* evitar publicar exemplos textuais reais no Quarto Book;
* revisar a política de segurança da instituição.

Para repositórios públicos, recomenda-se manter apenas:

* dados sintéticos;
* scripts;
* documentação;
* exemplos anonimizados;
* estrutura de entrada esperada.

---

# 19. Campos reais recomendados por fonte

## 19.1. RUE / emergência

Campos úteis:

| Campo real possível          | Uso no pipeline        |
| ---------------------------- | ---------------------- |
| identificador do atendimento | `record_id`            |
| data/hora do atendimento     | `data_atendimento`     |
| CID                          | `cid`                  |
| descrição do CID             | `cid_nome`             |
| queixa principal             | compor `texto_clinico` |
| anamnese                     | compor `texto_clinico` |
| classificação de risco       | campo auxiliar         |
| unidade                      | `unidade`              |
| município                    | `municipio`            |
| bairro                       | `bairro`               |
| idade                        | `idade`                |
| sexo                         | `sexo`                 |

## 19.2. APS / atenção primária

Campos úteis:

| Campo real possível          | Uso no pipeline               |
| ---------------------------- | ----------------------------- |
| identificador do atendimento | `record_id`                   |
| data/hora do atendimento     | `data_atendimento`            |
| motivo subjetivo             | compor `texto_clinico`        |
| avaliação ou plano           | campo auxiliar, se disponível |
| CID ou CIAP                  | `cid` ou campo auxiliar       |
| unidade                      | `unidade`                     |
| município                    | `municipio`                   |
| bairro                       | `bairro`                      |
| idade                        | `idade`                       |
| sexo                         | `sexo`                        |

Atenção: em algumas bases APS, o CID pode representar condição ativa do paciente e não necessariamente o motivo do atendimento. Nesse caso, o uso do CID deve ser tratado com cautela na classificação.

---

# 20. Regras para construir `texto_clinico`

O campo `texto_clinico` deve concentrar a informação textual usada pela regex e pela LLM.

Exemplo:

```r
texto_clinico = stringr::str_squish(paste(
  queixa_principal,
  anamnese,
  evolucao,
  sep = " "
))
```

Boas práticas:

* manter o texto original tanto quanto possível;
* evitar limpar demais antes da LLM;
* remover apenas quebras excessivas, espaços duplicados e campos vazios;
* não remover negações como “nega”, “sem”, “não refere”;
* preservar sintomas negados, pois eles são relevantes para auditoria;
* criar também uma versão normalizada apenas para regex, se necessário.

---

# 21. Fluxo recomendado para testar dados reais

## Etapa 1 — preparar uma amostra pequena

Começar com 50 a 200 registros.

```text
data/atendimentos_reais_teste.csv
```

## Etapa 2 — rodar em modo `mock`

```r
Sys.setenv(
  LLM_PROVIDER = "mock",
  RUN_REAL_LLM_IN_RENDER = "false"
)
```

Executar:

```bash
Rscript scripts/testar_novos_dados.R
```

## Etapa 3 — revisar saídas

Verificar:

```text
outputs/classificacao_final_novos_dados.csv
outputs/comparacao_camadas_novos_dados.csv
```

Avaliar:

* quantos registros foram classificados;
* quantos ficaram `nao_classificado`;
* quais síndromes apareceram;
* quais registros foram marcados para revisão;
* se existem falsos positivos evidentes.

## Etapa 4 — testar LLM real em amostra pequena

Só depois de validar o fluxo em `mock`, ativar Ollama/Qwen para uma amostra pequena.

```r
Sys.setenv(
  LLM_PROVIDER = "ollama",
  LLM_MODEL = "qwen3.5:latest",
  LLM_BASE_URL = "http://localhost:11434",
  LLM_JSON_MODE = "false",
  LLM_TIMEOUT_SEC = "180",
  LLM_MAX_RECORDS = "30",
  RUN_REAL_LLM_IN_RENDER = "true"
)
```

## Etapa 5 — auditar manualmente

Revisar exemplos por síndrome:

* respiratória;
* gastrointestinal;
* exantemática;
* hemorrágica;
* neurológica;
* inespecífica;
* não classificada.

## Etapa 6 — só então ampliar escala

Após a auditoria, ampliar para mais registros.

---

# 22. Recomendações para base real

Antes de rodar em escala:

* revisar CIDs específicos e inespecíficos;
* revisar regex de negação;
* validar exemplos com especialistas;
* criar base padrão-ouro;
* registrar métricas de execução da LLM;
* separar ambiente de desenvolvimento e produção;
* evitar renderizar o Quarto Book inteiro com LLM real em grande volume;
* preferir scripts batch para execução de LLM real;
* persistir logs de execução.

---

# 23. Git e GitHub

## 23.1. Inicializar Git

```bash
cd caminho/para/quarto_sindromes_febris

git init
git status
git add .
git commit -m "Versao inicial do Quarto Book de vigilancia sindromica"
```

## 23.2. Configurar usuário, se necessário

```bash
git config --global user.name "Gabriel Vieira"
git config --global user.email "camposvieiragabriel@gmail.com"
```

## 23.3. Criar repositório no GitHub

Criar um repositório vazio no GitHub.

Depois conectar:

```bash
git branch -M main
git remote add origin https://github.com/SEU_USUARIO/NOME_DO_REPOSITORIO.git
git push -u origin main
```

---

# 24. `.gitignore` recomendado

O `.gitignore` deve evitar versionar arquivos temporários, cache, locks e saídas locais sensíveis.

Sugestão:

```gitignore
.Rhistory
.RData
.Rproj.user/
.quarto/
*_files/

*.duckdb.wal
*.duckdb.tmp

outputs/teste_llm_local_qwen_30_registros.csv
outputs/teste_llm_local_qwen_30_metricas.csv
outputs/teste_llm_local_qwen_30_distribuicao_sindromes.csv

data/base_real_original.csv
data/atendimentos_reais_teste.csv
data/*real*.csv

.Renviron
```

Opcionalmente, se o HTML renderizado não for versionado:

```gitignore
_book/
```

Se o objetivo for permitir que alguém abra o produto diretamente sem renderizar, `_book/` pode ser incluído no repositório. Para repositórios de código reprodutível, normalmente `_book/` fica fora do Git.

---

# 25. Checagem de segredos antes do GitHub

Antes de subir:

```bash
grep -R "LLM_API_KEY\|api_key\|AIza\|sk-" . \
  --exclude-dir=.git \
  --exclude-dir=_book
```

Não versionar:

* `.Renviron` real;
* chaves de API;
* tokens;
* dados clínicos reais;
* arquivos identificáveis.

Usar apenas:

```text
.Renviron.example
```

sem segredos verdadeiros.

---

# 26. `.Renviron.example`

Sugestão de conteúdo:

```text
# Modo padrão reprodutível
LLM_PROVIDER=mock
LLM_MODEL=mock-sindromico-v1
LLM_BASE_URL=
LLM_API_KEY=
LLM_JSON_MODE=true
LLM_TIMEOUT_SEC=120
LLM_MAX_RECORDS=200
RUN_REAL_LLM_IN_RENDER=false

# Exemplo para Ollama/Qwen
# LLM_PROVIDER=ollama
# LLM_MODEL=qwen3.5:latest
# LLM_BASE_URL=http://localhost:11434
# LLM_JSON_MODE=false
# LLM_TIMEOUT_SEC=180
# LLM_MAX_RECORDS=30
# RUN_REAL_LLM_IN_RENDER=true

# Caminho opcional do DuckDB
# DUCKDB_PATH=outputs/sindromes_febris.duckdb
```

---

# 27. Reprodução em outro computador

Fluxo sugerido para outro usuário:

```bash
git clone https://github.com/SEU_USUARIO/NOME_DO_REPOSITORIO.git
cd NOME_DO_REPOSITORIO
```

No R:

```r
install.packages(c(
  "DBI",
  "duckdb",
  "dplyr",
  "tidyr",
  "stringr",
  "stringi",
  "purrr",
  "tibble",
  "readr",
  "lubridate",
  "ggplot2",
  "scales",
  "jsonlite",
  "httr2",
  "glue",
  "knitr",
  "quarto"
))

Sys.setenv(
  LLM_PROVIDER = "mock",
  LLM_MODEL = "mock-sindromico-v1",
  LLM_BASE_URL = "",
  LLM_JSON_MODE = "true",
  LLM_TIMEOUT_SEC = "120",
  LLM_MAX_RECORDS = "200",
  RUN_REAL_LLM_IN_RENDER = "false"
)

quarto::quarto_render()
```

Abrir:

```text
_book/index.html
```

---

# 28. Limitações conhecidas

A versão atual é metodológica e demonstrativa.

Limitações:

* usa dados sintéticos;
* não deve ser interpretada como classificador validado;
* regex pode gerar falso positivo quando há negação de sintomas;
* LLM real foi testada em amostra pequena;
* ainda não há base padrão-ouro;
* a classificação final não é diagnóstico clínico;
* adaptação para base real exige revisão de campos, CIDs, deduplicação e regras;
* dados reais exigem cuidados de privacidade e segurança.

---

# 29. Próximos passos técnicos

Possíveis evoluções:

* adaptar entrada para base real;
* criar scripts específicos por fonte de dados;
* revisar regex para negações;
* criar base padrão-ouro;
* validar desempenho por síndrome;
* testar múltiplos modelos LLM;
* registrar métricas de execução da LLM em rotina;
* implementar execução batch para LLM real;
* desenvolver painel Shiny;
* incorporar embeddings para vigilância pré-sindrômica;
* documentar implantação operacional.
