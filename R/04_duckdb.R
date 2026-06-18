# R/04_duckdb.R --------------------------------------------------------------
# Funções de persistência local em DuckDB.
#
# Este arquivo concentra a camada de banco local do Quarto Book.
# A ideia é que cada etapa metodológica grave seus resultados em tabelas
# próprias, permitindo auditoria, reexecução, comparação entre camadas
# e uso posterior em capítulos analíticos.
#
# Responsabilidades deste arquivo:
# - Definir caminho e conexão DuckDB
# - Inicializar schema mínimo
# - Migrar schemas antigos quando novas colunas forem incorporadas
# - Ler e escrever tabelas do pipeline
# - Registrar logs de execução
# - Resumir contagens das tabelas persistidas
#
# Importante:
# A lógica metodológica deve ficar nos scripts específicos:
# - R/05_cid_classification.R
# - R/06_regex_classification.R
# - R/08_llm_classification.R
# - R/09_comparacao_camadas.R
# - R/10_classificacao_final.R
#
# Este arquivo deve cuidar apenas da persistência.

# -------------------------------------------------------------------------
# Operador auxiliar
# -------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }

  if (length(x) == 1 && is.na(x)) {
    return(y)
  }

  x
}

# -------------------------------------------------------------------------
# Caminho e conexão
# -------------------------------------------------------------------------

get_duckdb_path <- function(path = "outputs/sindromes_febris.duckdb") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, mustWork = FALSE)
}

connect_duckdb <- function(path = get_duckdb_path(), read_only = FALSE) {
  DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = path,
    read_only = read_only
  )
}

# -------------------------------------------------------------------------
# Migração de schema
# -------------------------------------------------------------------------
# CREATE TABLE IF NOT EXISTS não altera tabelas existentes.
# Por isso, quando novas colunas são incorporadas ao projeto, usamos
# ensure_column() para manter compatibilidade com bancos DuckDB antigos.

ensure_column <- function(con, table, column, type) {
  stopifnot(DBI::dbIsValid(con))

  if (!DBI::dbExistsTable(con, table)) {
    return(invisible(FALSE))
  }

  campos <- DBI::dbListFields(con, table)

  if (!column %in% campos) {
    DBI::dbExecute(
      con,
      sprintf(
        "ALTER TABLE %s ADD COLUMN %s %s",
        DBI::dbQuoteIdentifier(con, table),
        DBI::dbQuoteIdentifier(con, column),
        type
      )
    )
  }

  invisible(TRUE)
}

migrate_duckdb_schema <- function(con) {
  stopifnot(DBI::dbIsValid(con))

  # -----------------------------------------------------------------------
  # tb_execucoes
  # -----------------------------------------------------------------------

  ensure_column(con, "tb_execucoes", "run_id", "VARCHAR")
  ensure_column(con, "tb_execucoes", "etapa", "VARCHAR")
  ensure_column(con, "tb_execucoes", "data_execucao", "TIMESTAMP")
  ensure_column(con, "tb_execucoes", "n_registros", "INTEGER")
  ensure_column(con, "tb_execucoes", "observacao", "VARCHAR")

  # -----------------------------------------------------------------------
  # tb_atendimentos
  # -----------------------------------------------------------------------

  ensure_column(con, "tb_atendimentos", "record_id", "VARCHAR")
  ensure_column(con, "tb_atendimentos", "data_atendimento", "DATE")
  ensure_column(con, "tb_atendimentos", "unidade", "VARCHAR")
  ensure_column(con, "tb_atendimentos", "idade", "DOUBLE")
  ensure_column(con, "tb_atendimentos", "faixa_etaria", "VARCHAR")
  ensure_column(con, "tb_atendimentos", "sexo", "VARCHAR")
  ensure_column(con, "tb_atendimentos", "cid", "VARCHAR")
  ensure_column(con, "tb_atendimentos", "cid_nome", "VARCHAR")
  ensure_column(con, "tb_atendimentos", "queixa", "VARCHAR")
  ensure_column(con, "tb_atendimentos", "anamnese", "VARCHAR")
  ensure_column(con, "tb_atendimentos", "texto_clinico", "VARCHAR")
  ensure_column(con, "tb_atendimentos", "texto_clinico_norm", "VARCHAR")

  # -----------------------------------------------------------------------
  # tb_classificacao_cid
  # -----------------------------------------------------------------------

  ensure_column(con, "tb_classificacao_cid", "record_id", "VARCHAR")
  ensure_column(con, "tb_classificacao_cid", "classificacao_cid", "VARCHAR")
  ensure_column(con, "tb_classificacao_cid", "cid_original", "VARCHAR")
  ensure_column(con, "tb_classificacao_cid", "cid_norm", "VARCHAR")
  ensure_column(con, "tb_classificacao_cid", "cid_prefix_match", "VARCHAR")
  ensure_column(con, "tb_classificacao_cid", "grupo_cid", "VARCHAR")
  ensure_column(con, "tb_classificacao_cid", "descricao_grupo", "VARCHAR")
  ensure_column(con, "tb_classificacao_cid", "cid_informativo", "BOOLEAN")
  ensure_column(con, "tb_classificacao_cid", "fonte_classificacao_cid", "VARCHAR")

  # -----------------------------------------------------------------------
  # tb_classificacao_regex
  # -----------------------------------------------------------------------

  ensure_column(con, "tb_classificacao_regex", "record_id", "VARCHAR")
  ensure_column(con, "tb_classificacao_regex", "flag_febre", "BOOLEAN")
  ensure_column(con, "tb_classificacao_regex", "flag_febre_negada", "BOOLEAN")
  ensure_column(con, "tb_classificacao_regex", "febre_valida_regex", "BOOLEAN")
  ensure_column(con, "tb_classificacao_regex", "flag_respiratorio", "BOOLEAN")
  ensure_column(con, "tb_classificacao_regex", "flag_exantematico", "BOOLEAN")
  ensure_column(con, "tb_classificacao_regex", "flag_gastrointestinal", "BOOLEAN")
  ensure_column(con, "tb_classificacao_regex", "flag_hemorragico", "BOOLEAN")
  ensure_column(con, "tb_classificacao_regex", "flag_icterico", "BOOLEAN")
  ensure_column(con, "tb_classificacao_regex", "flag_neurologico_meningeo", "BOOLEAN")
  ensure_column(con, "tb_classificacao_regex", "flag_inespecifico", "BOOLEAN")
  ensure_column(con, "tb_classificacao_regex", "sindrome_principal_regex", "VARCHAR")
  ensure_column(con, "tb_classificacao_regex", "regex_classificado", "BOOLEAN")
  ensure_column(con, "tb_classificacao_regex", "fonte_classificacao_regex", "VARCHAR")
  ensure_column(con, "tb_classificacao_regex", "sintomas_regex", "VARCHAR")

  # -----------------------------------------------------------------------
  # tb_classificacao_llm
  # -----------------------------------------------------------------------

  ensure_column(con, "tb_classificacao_llm", "record_id", "VARCHAR")
  ensure_column(con, "tb_classificacao_llm", "sindrome_principal_llm", "VARCHAR")
  ensure_column(con, "tb_classificacao_llm", "febre_presente", "BOOLEAN")
  ensure_column(con, "tb_classificacao_llm", "febre_negada", "BOOLEAN")
  ensure_column(con, "tb_classificacao_llm", "sintomas_identificados", "VARCHAR")
  ensure_column(con, "tb_classificacao_llm", "justificativa_llm", "VARCHAR")
  ensure_column(con, "tb_classificacao_llm", "llm_classificado", "BOOLEAN")
  ensure_column(con, "tb_classificacao_llm", "provider_llm", "VARCHAR")
  ensure_column(con, "tb_classificacao_llm", "modelo_llm", "VARCHAR")
  ensure_column(con, "tb_classificacao_llm", "prompt_version", "VARCHAR")
  ensure_column(con, "tb_classificacao_llm", "parse_ok", "BOOLEAN")
  ensure_column(con, "tb_classificacao_llm", "parse_error", "VARCHAR")
  ensure_column(con, "tb_classificacao_llm", "raw_response", "VARCHAR")
  ensure_column(con, "tb_classificacao_llm", "data_classificacao", "TIMESTAMP")

  # -----------------------------------------------------------------------
  # tb_comparacao_camadas
  # -----------------------------------------------------------------------

  ensure_column(con, "tb_comparacao_camadas", "record_id", "VARCHAR")
  ensure_column(con, "tb_comparacao_camadas", "sindrome_cid", "VARCHAR")
  ensure_column(con, "tb_comparacao_camadas", "sindrome_regex", "VARCHAR")
  ensure_column(con, "tb_comparacao_camadas", "sindrome_llm", "VARCHAR")
  ensure_column(con, "tb_comparacao_camadas", "cid_classificado", "BOOLEAN")
  ensure_column(con, "tb_comparacao_camadas", "regex_classificado", "BOOLEAN")
  ensure_column(con, "tb_comparacao_camadas", "llm_classificado", "BOOLEAN")
  ensure_column(con, "tb_comparacao_camadas", "cid_informativo", "BOOLEAN")
  ensure_column(con, "tb_comparacao_camadas", "n_camadas_classificaram", "INTEGER")
  ensure_column(con, "tb_comparacao_camadas", "cid_regex_concordam", "BOOLEAN")
  ensure_column(con, "tb_comparacao_camadas", "cid_llm_concordam", "BOOLEAN")
  ensure_column(con, "tb_comparacao_camadas", "regex_llm_concordam", "BOOLEAN")
  ensure_column(con, "tb_comparacao_camadas", "tres_camadas_concordam", "BOOLEAN")
  ensure_column(con, "tb_comparacao_camadas", "padrao_comparacao", "VARCHAR")
  ensure_column(con, "tb_comparacao_camadas", "divergencia_com_cid_informativo", "BOOLEAN")
  ensure_column(con, "tb_comparacao_camadas", "divergencia_textual_regex_llm", "BOOLEAN")
  ensure_column(con, "tb_comparacao_camadas", "camadas_disponiveis", "VARCHAR")
  ensure_column(con, "tb_comparacao_camadas", "tripla_classificacao", "VARCHAR")

  # -----------------------------------------------------------------------
  # tb_classificacao_final
  # -----------------------------------------------------------------------

  ensure_column(con, "tb_classificacao_final", "record_id", "VARCHAR")
  ensure_column(con, "tb_classificacao_final", "sindrome_final", "VARCHAR")
  ensure_column(con, "tb_classificacao_final", "fonte_classificacao_final", "VARCHAR")
  ensure_column(con, "tb_classificacao_final", "regra_classificacao_final", "VARCHAR")
  ensure_column(con, "tb_classificacao_final", "revisao_manual_recomendada", "BOOLEAN")
  ensure_column(con, "tb_classificacao_final", "motivo_revisao", "VARCHAR")
  ensure_column(con, "tb_classificacao_final", "classificado_final", "BOOLEAN")
  ensure_column(con, "tb_classificacao_final", "tipo_resultado_final", "VARCHAR")
  ensure_column(con, "tb_classificacao_final", "data_classificacao_final", "TIMESTAMP")

  invisible(TRUE)
}

# -------------------------------------------------------------------------
# Inicialização do schema
# -------------------------------------------------------------------------

init_duckdb_schema <- function(con) {
  stopifnot(DBI::dbIsValid(con))

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS tb_execucoes (
      run_id VARCHAR,
      etapa VARCHAR,
      data_execucao TIMESTAMP,
      n_registros INTEGER,
      observacao VARCHAR
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS tb_atendimentos (
      record_id VARCHAR,
      data_atendimento DATE,
      unidade VARCHAR,
      idade DOUBLE,
      faixa_etaria VARCHAR,
      sexo VARCHAR,
      cid VARCHAR,
      cid_nome VARCHAR,
      queixa VARCHAR,
      anamnese VARCHAR,
      texto_clinico VARCHAR,
      texto_clinico_norm VARCHAR
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS tb_classificacao_cid (
      record_id VARCHAR,
      classificacao_cid VARCHAR,
      cid_original VARCHAR,
      cid_norm VARCHAR,
      cid_prefix_match VARCHAR,
      grupo_cid VARCHAR,
      descricao_grupo VARCHAR,
      cid_informativo BOOLEAN,
      fonte_classificacao_cid VARCHAR
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS tb_classificacao_regex (
      record_id VARCHAR,
      flag_febre BOOLEAN,
      flag_febre_negada BOOLEAN,
      febre_valida_regex BOOLEAN,
      flag_respiratorio BOOLEAN,
      flag_exantematico BOOLEAN,
      flag_gastrointestinal BOOLEAN,
      flag_hemorragico BOOLEAN,
      flag_icterico BOOLEAN,
      flag_neurologico_meningeo BOOLEAN,
      flag_inespecifico BOOLEAN,
      sindrome_principal_regex VARCHAR,
      regex_classificado BOOLEAN,
      fonte_classificacao_regex VARCHAR,
      sintomas_regex VARCHAR
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS tb_classificacao_llm (
      record_id VARCHAR,
      sindrome_principal_llm VARCHAR,
      febre_presente BOOLEAN,
      febre_negada BOOLEAN,
      sintomas_identificados VARCHAR,
      justificativa_llm VARCHAR,
      llm_classificado BOOLEAN,
      provider_llm VARCHAR,
      modelo_llm VARCHAR,
      prompt_version VARCHAR,
      parse_ok BOOLEAN,
      parse_error VARCHAR,
      raw_response VARCHAR,
      data_classificacao TIMESTAMP
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS tb_comparacao_camadas (
      record_id VARCHAR,
      sindrome_cid VARCHAR,
      sindrome_regex VARCHAR,
      sindrome_llm VARCHAR,
      cid_classificado BOOLEAN,
      regex_classificado BOOLEAN,
      llm_classificado BOOLEAN,
      cid_informativo BOOLEAN,
      n_camadas_classificaram INTEGER,
      cid_regex_concordam BOOLEAN,
      cid_llm_concordam BOOLEAN,
      regex_llm_concordam BOOLEAN,
      tres_camadas_concordam BOOLEAN,
      padrao_comparacao VARCHAR,
      divergencia_com_cid_informativo BOOLEAN,
      divergencia_textual_regex_llm BOOLEAN,
      camadas_disponiveis VARCHAR,
      tripla_classificacao VARCHAR
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS tb_classificacao_final (
      record_id VARCHAR,
      sindrome_final VARCHAR,
      fonte_classificacao_final VARCHAR,
      regra_classificacao_final VARCHAR,
      revisao_manual_recomendada BOOLEAN,
      motivo_revisao VARCHAR,
      classificado_final BOOLEAN,
      tipo_resultado_final VARCHAR,
      data_classificacao_final TIMESTAMP
    )
  ")

  migrate_duckdb_schema(con)

  invisible(TRUE)
}

# -------------------------------------------------------------------------
# Log de execução
# -------------------------------------------------------------------------

new_run_id <- function(prefix = "run") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
}

write_execution_log <- function(
    con,
    etapa,
    n_registros,
    observacao = NA_character_,
    run_id = new_run_id(etapa)
) {
  stopifnot(DBI::dbIsValid(con))

  if (!DBI::dbExistsTable(con, "tb_execucoes")) {
    init_duckdb_schema(con)
  } else {
    migrate_duckdb_schema(con)
  }

  log_tbl <- tibble::tibble(
    run_id = run_id,
    etapa = etapa,
    data_execucao = Sys.time(),
    n_registros = as.integer(n_registros),
    observacao = observacao
  )

  # Compatibilidade com schemas antigos que eventualmente tenham colunas
  # extras ou ordem diferente.
  campos_destino <- DBI::dbListFields(con, "tb_execucoes")

  for (campo in setdiff(campos_destino, names(log_tbl))) {
    log_tbl[[campo]] <- NA
  }

  log_tbl <- log_tbl[, campos_destino, drop = FALSE]

  DBI::dbWriteTable(
    conn = con,
    name = "tb_execucoes",
    value = log_tbl,
    append = TRUE
  )

  invisible(log_tbl)
}

# -------------------------------------------------------------------------
# Utilitários de escrita
# -------------------------------------------------------------------------

write_table_safe <- function(con, table, dados, overwrite = TRUE) {
  stopifnot(DBI::dbIsValid(con))
  stopifnot(is.data.frame(dados))

  # Quando overwrite = TRUE, removemos a tabela antes para evitar conflitos
  # com schemas antigos. Em seguida, dbWriteTable recria a tabela com o schema
  # real do objeto salvo.
  if (overwrite && DBI::dbExistsTable(con, table)) {
    DBI::dbExecute(
      con,
      paste("DROP TABLE IF EXISTS", DBI::dbQuoteIdentifier(con, table))
    )
  }

  DBI::dbWriteTable(
    conn = con,
    name = table,
    value = dados,
    overwrite = overwrite,
    append = !overwrite
  )

  invisible(dados)
}

read_table_safe <- function(con, table) {
  stopifnot(DBI::dbIsValid(con))

  if (!DBI::dbExistsTable(con, table)) {
    return(tibble::tibble())
  }

  DBI::dbReadTable(con, table) |>
    tibble::as_tibble()
}

# -------------------------------------------------------------------------
# Escrita das tabelas do pipeline
# -------------------------------------------------------------------------

write_atendimentos <- function(con, dados, overwrite = TRUE) {
  write_table_safe(con, "tb_atendimentos", dados, overwrite = overwrite)
}

write_classificacao_cid <- function(
    con,
    dados = NULL,
    dados_cid = NULL,
    overwrite = TRUE
) {
  # Retrocompatibilidade:
  # capítulos anteriores chamavam esta função com dados_cid = ...
  # versões mais novas podem chamar com dados = ...
  if (is.null(dados)) dados <- dados_cid

  if (is.null(dados)) {
    rlang::abort("Informe os dados em `dados` ou `dados_cid`.")
  }

  write_table_safe(con, "tb_classificacao_cid", dados, overwrite = overwrite)
}

write_classificacao_regex <- function(
    con,
    dados = NULL,
    dados_regex = NULL,
    overwrite = TRUE
) {
  # Retrocompatibilidade com chamadas antigas: dados_regex = ...
  if (is.null(dados)) dados <- dados_regex

  if (is.null(dados)) {
    rlang::abort("Informe os dados em `dados` ou `dados_regex`.")
  }

  write_table_safe(con, "tb_classificacao_regex", dados, overwrite = overwrite)
}

write_classificacao_llm <- function(
    con,
    dados = NULL,
    dados_llm = NULL,
    overwrite = TRUE
) {
  # Retrocompatibilidade com chamadas explícitas: dados_llm = ...
  if (is.null(dados)) dados <- dados_llm

  if (is.null(dados)) {
    rlang::abort("Informe os dados em `dados` ou `dados_llm`.")
  }

  dados_db <- dados

  # Campos de lista não são gravados diretamente de forma estável no DuckDB.
  # Convertemos para texto separado por "; ".
  dados_db <- dados_db |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.list),
        ~ purrr::map_chr(.x, ~ paste(.x %||% character(0), collapse = "; "))
      )
    )

  if ("data_classificacao" %in% names(dados_db)) {
    dados_db <- dados_db |>
      dplyr::mutate(
        data_classificacao = dplyr::coalesce(.data$data_classificacao, Sys.time())
      )
  } else {
    dados_db$data_classificacao <- Sys.time()
  }

  write_table_safe(con, "tb_classificacao_llm", dados_db, overwrite = overwrite)

  invisible(dados)
}

write_comparacao_camadas <- function(
    con,
    dados = NULL,
    dados_comparacao = NULL,
    overwrite = TRUE
) {
  # Retrocompatibilidade: aceita dados = ... ou dados_comparacao = ...
  if (is.null(dados)) dados <- dados_comparacao

  if (is.null(dados)) {
    rlang::abort("Informe os dados em `dados` ou `dados_comparacao`.")
  }

  dados_db <- dados |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.list),
        ~ purrr::map_chr(.x, ~ paste(.x %||% character(0), collapse = "; "))
      )
    )

  write_table_safe(con, "tb_comparacao_camadas", dados_db, overwrite = overwrite)

  invisible(dados)
}

write_classificacao_final <- function(
    con,
    dados = NULL,
    dados_final = NULL,
    overwrite = TRUE
) {
  # Retrocompatibilidade: aceita dados = ... ou dados_final = ...
  if (is.null(dados)) dados <- dados_final

  if (is.null(dados)) {
    rlang::abort("Informe os dados em `dados` ou `dados_final`.")
  }

  dados_db <- dados |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.list),
        ~ purrr::map_chr(.x, ~ paste(.x %||% character(0), collapse = "; "))
      )
    )

  if ("data_classificacao_final" %in% names(dados_db)) {
    dados_db <- dados_db |>
      dplyr::mutate(
        data_classificacao_final = dplyr::coalesce(
          .data$data_classificacao_final,
          Sys.time()
        )
      )
  } else {
    dados_db$data_classificacao_final <- Sys.time()
  }

  write_table_safe(con, "tb_classificacao_final", dados_db, overwrite = overwrite)

  invisible(dados)
}

# -------------------------------------------------------------------------
# Leitura das tabelas do pipeline
# -------------------------------------------------------------------------

read_atendimentos <- function(con) {
  read_table_safe(con, "tb_atendimentos")
}

read_classificacao_cid <- function(con) {
  read_table_safe(con, "tb_classificacao_cid")
}

read_classificacao_regex <- function(con) {
  read_table_safe(con, "tb_classificacao_regex")
}

read_classificacao_llm <- function(con) {
  read_table_safe(con, "tb_classificacao_llm")
}

read_comparacao_camadas <- function(con) {
  read_table_safe(con, "tb_comparacao_camadas")
}

read_classificacao_final <- function(con) {
  read_table_safe(con, "tb_classificacao_final")
}

# -------------------------------------------------------------------------
# Resumos do banco
# -------------------------------------------------------------------------

duckdb_counts <- function(con) {
  stopifnot(DBI::dbIsValid(con))

  tabelas <- DBI::dbListTables(con)

  if (length(tabelas) == 0) {
    return(tibble::tibble(tabela = character(), n = integer()))
  }

  purrr::map_dfr(tabelas, function(tbl) {
    query <- paste0(
      "SELECT COUNT(*) AS n FROM ",
      DBI::dbQuoteIdentifier(con, tbl)
    )

    n <- DBI::dbGetQuery(con, query)$n[[1]]

    tibble::tibble(
      tabela = tbl,
      n = as.integer(n)
    )
  }) |>
    dplyr::arrange(.data$tabela)
}

duckdb_list_tables <- function(con) {
  stopifnot(DBI::dbIsValid(con))

  DBI::dbListTables(con) |>
    sort()
}

duckdb_table_schema <- function(con, table) {
  stopifnot(DBI::dbIsValid(con))

  if (!DBI::dbExistsTable(con, table)) {
    return(tibble::tibble())
  }

  DBI::dbGetQuery(
    con,
    paste0("DESCRIBE ", DBI::dbQuoteIdentifier(con, table))
  ) |>
    tibble::as_tibble()
}
