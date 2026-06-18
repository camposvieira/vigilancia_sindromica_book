# ============================================================================
# R/04_data_quality.R
#
# Funções auxiliares para resumir qualidade e perfil da base de atendimentos.
# ============================================================================

summarise_data_quality <- function(df) {

  tibble::tibble(
    indicador = c(
      "Registros",
      "Unidades",
      "Territórios",
      "Bairros",
      "Período inicial",
      "Período final",
      "Com CID informado",
      "Sem CID informado",
      "CID coerente na simulação",
      "CID conflitante na simulação",
      "Com texto clínico",
      "Texto clínico com 100+ caracteres",
      "Textos com negação explícita de febre"
    ),
    valor = c(
      nrow(df),
      dplyr::n_distinct(df$unidade),
      dplyr::n_distinct(df$territorio),
      dplyr::n_distinct(df$bairro),
      as.character(min(df$data_atendimento, na.rm = TRUE)),
      as.character(max(df$data_atendimento, na.rm = TRUE)),
      sum(!is.na(df$cid) & df$cid != ""),
      sum(is.na(df$cid) | df$cid == ""),
      sum(df$cid_status_sintetico == "coerente", na.rm = TRUE),
      sum(df$cid_status_sintetico == "conflitante", na.rm = TRUE),
      sum(!is.na(df$texto_clinico) & df$texto_clinico != ""),
      sum(nchar(df$texto_clinico) >= 100, na.rm = TRUE),
      sum(stringr::str_detect(df$texto_clinico_norm, "\\b(nega febre|sem febre|afebril|sem relato de febre|nao apresenta febre)\\b"), na.rm = TRUE)
    )
  )

}

plot_temporal_distribution <- function(df) {

  df |>
    dplyr::mutate(mes = lubridate::floor_date(data_atendimento, "month")) |>
    dplyr::count(mes) |>
    ggplot2::ggplot(ggplot2::aes(x = mes, y = n)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      x = NULL,
      y = "Atendimentos",
      title = "Distribuição temporal dos atendimentos sintéticos"
    ) +
    ggplot2::theme_minimal(base_size = 12)

}

plot_bar_distribution <- function(df, var, title) {

  var <- rlang::ensym(var)

  df |>
    dplyr::count(!!var, sort = TRUE) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(as.character(!!var), n), y = n)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Atendimentos",
      title = title
    ) +
    ggplot2::theme_minimal(base_size = 12)

}
