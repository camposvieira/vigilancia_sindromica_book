# R/11_series_historicas.R ----------------------------------------------------
# Funções para análise temporal da classificação final.
#
# Este script usa como entrada principal a base produzida no capítulo de
# classificação final (`tb_classificacao_final`). A etapa tem finalidade
# descritiva e operacional: transformar a classificação final em séries diárias,
# semanais e resumos por unidade/síndrome.
#
# Importante:
# - Este script não altera a regra de classificação final.
# - A persistência principal continua centralizada no R/04_duckdb.R.
# - As funções abaixo operam sobre data.frames/tibbles já lidos do DuckDB.

# -------------------------------------------------------------------------
# Validação e preparação
# -------------------------------------------------------------------------

validar_base_series <- function(dados) {
  if (!is.data.frame(dados)) {
    rlang::abort("`dados` precisa ser um data.frame ou tibble.")
  }

  colunas_obrigatorias <- c("record_id", "data_atendimento", "sindrome_final")
  ausentes <- setdiff(colunas_obrigatorias, names(dados))

  if (length(ausentes) > 0) {
    rlang::abort(
      paste0(
        "A base de classificação final não contém as colunas obrigatórias: ",
        paste(ausentes, collapse = ", "),
        ". Verifique se a tabela `tb_classificacao_final` foi gerada após a reorganização do DuckDB."
      )
    )
  }

  invisible(TRUE)
}

preparar_base_series <- function(dados) {
  validar_base_series(dados)

  dados |>
    dplyr::mutate(
      data_atendimento = as.Date(.data$data_atendimento),
      sindrome_final = dplyr::coalesce(as.character(.data$sindrome_final), "nao_classificado"),
      classificado_final = dplyr::case_when(
        "classificado_final" %in% names(dados) ~ as.logical(.data$classificado_final),
        TRUE ~ .data$sindrome_final != "nao_classificado"
      ),
      semana_inicio = lubridate::floor_date(.data$data_atendimento, unit = "week", week_start = 1),
      mes_inicio = lubridate::floor_date(.data$data_atendimento, unit = "month")
    )
}

# -------------------------------------------------------------------------
# Séries históricas
# -------------------------------------------------------------------------

build_serie_diaria <- function(dados, incluir_nao_classificado = FALSE) {
  base <- preparar_base_series(dados)

  if (!incluir_nao_classificado) {
    base <- base |>
      dplyr::filter(.data$sindrome_final != "nao_classificado")
  }

  base |>
    dplyr::count(.data$data_atendimento, .data$sindrome_final, name = "n") |>
    dplyr::arrange(.data$data_atendimento, .data$sindrome_final)
}

build_serie_diaria_total <- function(dados, incluir_nao_classificado = FALSE) {
  base <- preparar_base_series(dados)

  if (!incluir_nao_classificado) {
    base <- base |>
      dplyr::filter(.data$sindrome_final != "nao_classificado")
  }

  base |>
    dplyr::count(.data$data_atendimento, name = "n") |>
    dplyr::arrange(.data$data_atendimento)
}

build_serie_semanal <- function(dados, incluir_nao_classificado = FALSE) {
  base <- preparar_base_series(dados)

  if (!incluir_nao_classificado) {
    base <- base |>
      dplyr::filter(.data$sindrome_final != "nao_classificado")
  }

  base |>
    dplyr::count(.data$semana_inicio, .data$sindrome_final, name = "n") |>
    dplyr::arrange(.data$semana_inicio, .data$sindrome_final)
}

build_serie_semanal_total <- function(dados, incluir_nao_classificado = FALSE) {
  base <- preparar_base_series(dados)

  if (!incluir_nao_classificado) {
    base <- base |>
      dplyr::filter(.data$sindrome_final != "nao_classificado")
  }

  base |>
    dplyr::count(.data$semana_inicio, name = "n") |>
    dplyr::arrange(.data$semana_inicio)
}

build_serie_mensal <- function(dados, incluir_nao_classificado = FALSE) {
  base <- preparar_base_series(dados)

  if (!incluir_nao_classificado) {
    base <- base |>
      dplyr::filter(.data$sindrome_final != "nao_classificado")
  }

  base |>
    dplyr::count(.data$mes_inicio, .data$sindrome_final, name = "n") |>
    dplyr::arrange(.data$mes_inicio, .data$sindrome_final)
}

# -------------------------------------------------------------------------
# Resumos operacionais
# -------------------------------------------------------------------------

summarise_series_periodo <- function(dados) {
  base <- preparar_base_series(dados)

  tibble::tibble(
    indicador = c(
      "Primeira data",
      "Última data",
      "Número de dias no período",
      "Atendimentos totais",
      "Atendimentos classificados",
      "Atendimentos não classificados"
    ),
    valor = c(
      as.character(min(base$data_atendimento, na.rm = TRUE)),
      as.character(max(base$data_atendimento, na.rm = TRUE)),
      as.character(dplyr::n_distinct(base$data_atendimento)),
      as.character(nrow(base)),
      as.character(sum(base$sindrome_final != "nao_classificado", na.rm = TRUE)),
      as.character(sum(base$sindrome_final == "nao_classificado", na.rm = TRUE))
    )
  )
}

summarise_sindromes_final <- function(dados) {
  base <- preparar_base_series(dados)

  base |>
    dplyr::count(.data$sindrome_final, name = "n") |>
    dplyr::mutate(
      percentual = round(100 * .data$n / sum(.data$n), 1)
    ) |>
    dplyr::arrange(dplyr::desc(.data$n))
}

summarise_semanas_maior_volume <- function(dados, n = 10) {
  build_serie_semanal_total(dados) |>
    dplyr::arrange(dplyr::desc(.data$n), .data$semana_inicio) |>
    dplyr::slice_head(n = n)
}

summarise_unidades_maior_volume <- function(dados, n = 15) {
  base <- preparar_base_series(dados)

  if (!"unidade" %in% names(base)) {
    return(tibble::tibble(
      unidade = character(),
      n = integer(),
      percentual = numeric()
    ))
  }

  base |>
    dplyr::filter(.data$sindrome_final != "nao_classificado") |>
    dplyr::count(.data$unidade, name = "n") |>
    dplyr::mutate(
      percentual = round(100 * .data$n / sum(.data$n), 1)
    ) |>
    dplyr::arrange(dplyr::desc(.data$n)) |>
    dplyr::slice_head(n = n)
}

summarise_unidade_sindrome <- function(dados, n_unidades = 10) {
  base <- preparar_base_series(dados)

  if (!"unidade" %in% names(base)) {
    return(tibble::tibble(
      unidade = character(),
      sindrome_final = character(),
      n = integer(),
      percentual_unidade = numeric()
    ))
  }

  top_unidades <- base |>
    dplyr::filter(.data$sindrome_final != "nao_classificado") |>
    dplyr::count(.data$unidade, name = "n_total") |>
    dplyr::arrange(dplyr::desc(.data$n_total)) |>
    dplyr::slice_head(n = n_unidades) |>
    dplyr::pull(.data$unidade)

  base |>
    dplyr::filter(
      .data$unidade %in% top_unidades,
      .data$sindrome_final != "nao_classificado"
    ) |>
    dplyr::count(.data$unidade, .data$sindrome_final, name = "n") |>
    dplyr::group_by(.data$unidade) |>
    dplyr::mutate(
      percentual_unidade = round(100 * .data$n / sum(.data$n), 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$unidade, dplyr::desc(.data$n))
}

# -------------------------------------------------------------------------
# Gráficos
# -------------------------------------------------------------------------

plot_serie_diaria_total <- function(dados) {
  serie <- build_serie_diaria_total(dados)

  ggplot2::ggplot(
    serie,
    ggplot2::aes(x = .data$data_atendimento, y = .data$n)
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = NULL,
      y = "Número de atendimentos",
      title = "Série diária de atendimentos classificados"
    ) +
    ggplot2::theme_minimal()
}

plot_serie_semanal_total <- function(dados) {
  serie <- build_serie_semanal_total(dados)

  ggplot2::ggplot(
    serie,
    ggplot2::aes(x = .data$semana_inicio, y = .data$n)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = NULL,
      y = "Número de atendimentos",
      title = "Série semanal de atendimentos classificados"
    ) +
    ggplot2::theme_minimal()
}

plot_serie_semanal_por_sindrome <- function(dados) {
  serie <- build_serie_semanal(dados)

  ggplot2::ggplot(
    serie,
    ggplot2::aes(
      x = .data$semana_inicio,
      y = .data$n,
      group = .data$sindrome_final
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~sindrome_final, scales = "free_y") +
    ggplot2::labs(
      x = NULL,
      y = "Número de atendimentos",
      title = "Série semanal por síndrome final"
    ) +
    ggplot2::theme_minimal()
}

plot_distribuicao_sindromes_series <- function(dados) {
  resumo <- summarise_sindromes_final(dados)

  ggplot2::ggplot(
    resumo,
    ggplot2::aes(
      x = stats::reorder(.data$sindrome_final, .data$n),
      y = .data$n
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Número de atendimentos",
      title = "Distribuição dos atendimentos por síndrome final"
    ) +
    ggplot2::theme_minimal()
}

plot_unidades_maior_volume <- function(dados, n = 15) {
  resumo <- summarise_unidades_maior_volume(dados, n = n)

  ggplot2::ggplot(
    resumo,
    ggplot2::aes(
      x = stats::reorder(.data$unidade, .data$n),
      y = .data$n
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Número de atendimentos",
      title = "Unidades com maior volume de atendimentos classificados"
    ) +
    ggplot2::theme_minimal()
}
