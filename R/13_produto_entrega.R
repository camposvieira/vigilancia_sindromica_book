# R/13_produto_entrega.R -----------------------------------------------------
# Funções auxiliares para o capítulo final de entrega do produto.
#
# Este script não altera o pipeline principal. Ele apenas organiza resumos
# executivos sobre o estado do projeto, tabelas persistidas e próximos passos.

summarise_tabelas_produto <- function(con) {
  stopifnot(DBI::dbIsValid(con))

  if (!exists("duckdb_counts", mode = "function")) {
    rlang::abort("A função duckdb_counts() não foi encontrada. Rode source('R/04_duckdb.R').")
  }

  duckdb_counts(con) |>
    dplyr::arrange(.data$tabela)
}

summarise_entregaveis_produto <- function() {
  tibble::tibble(
    componente = c(
      "Quarto Book",
      "Base sintética demonstrativa",
      "DuckDB local",
      "Camada CID",
      "Camada regex",
      "Camada LLM",
      "Comparação entre camadas",
      "Classificação final",
      "Séries históricas",
      "Auditoria e limitações",
      "Documentação de execução"
    ),
    status = c(
      "implementado",
      "implementado",
      "implementado",
      "implementado",
      "implementado",
      "implementado",
      "implementado",
      "implementado",
      "implementado",
      "implementado",
      "implementado"
    ),
    finalidade = c(
      "Relatório metodológico executável e apresentável.",
      "Permitir demonstração antes do recebimento da base real.",
      "Persistir resultados intermediários e finais para auditoria.",
      "Classificar atendimentos por códigos estruturados.",
      "Classificar textos por padrões explícitos de sintomas.",
      "Classificar textos por interpretação estruturada de modelo de linguagem.",
      "Auditar convergências e divergências entre CID, regex e LLM.",
      "Produzir uma única síndrome final por atendimento.",
      "Descrever evolução temporal dos atendimentos classificados.",
      "Explicitar riscos, limites e necessidade de validação.",
      "Facilitar reexecução, manutenção e versionamento em GitHub."
    )
  )
}

summarise_campos_minimos_base_real <- function() {
  tibble::tibble(
    campo = c(
      "record_id",
      "data_atendimento",
      "unidade",
      "idade",
      "sexo",
      "cid",
      "cid_nome",
      "queixa",
      "anamnese",
      "texto_clinico"
    ),
    obrigatoriedade = c(
      "obrigatório",
      "obrigatório",
      "recomendado",
      "recomendado",
      "recomendado",
      "recomendado",
      "opcional",
      "recomendado",
      "recomendado",
      "obrigatório após padronização"
    ),
    observacao = c(
      "Identificador único do atendimento.",
      "Data ou data/hora do atendimento; usada nas séries históricas.",
      "Nome ou código da unidade de atendimento.",
      "Idade do paciente; permite recortes etários.",
      "Sexo registrado; permite estratificações.",
      "CID principal ou hipótese codificada, quando disponível.",
      "Descrição textual do CID, quando disponível.",
      "Queixa principal ou motivo do atendimento.",
      "Texto clínico, anamnese, evolução ou campo livre equivalente.",
      "Campo consolidado usado pelas camadas regex e LLM. Pode ser criado a partir de queixa + anamnese."
    )
  )
}

summarise_modulos_shiny <- function() {
  tibble::tibble(
    modulo = c(
      "Importação de dados",
      "Configuração do pipeline",
      "Execução da classificação",
      "Monitoramento temporal",
      "Auditoria de registros",
      "Exportação",
      "Administração"
    ),
    objetivo = c(
      "Carregar CSV/Parquet ou conectar a banco institucional.",
      "Selecionar síndromes, provedor LLM, modelo e parâmetros de execução.",
      "Executar CID, regex, LLM, comparação e classificação final.",
      "Visualizar séries diárias/semanais por síndrome e unidade.",
      "Revisar divergências, casos de baixa sustentação textual e registros não classificados.",
      "Gerar CSV, Excel, DuckDB ou relatório Quarto renderizado.",
      "Gerenciar usuários, logs, credenciais e parâmetros operacionais."
    ),
    prioridade = c("alta", "alta", "alta", "alta", "alta", "média", "média")
  )
}
