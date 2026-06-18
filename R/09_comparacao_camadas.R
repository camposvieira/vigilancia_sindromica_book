# R/09_comparacao_camadas.R --------------------------------------------------
# Comparação entre as três camadas de classificação:
# - CID
# - regex
# - LLM
#
# Esta etapa ainda não define a classificação final.
# O objetivo é construir uma base auditável de convergência, divergência
# e lacunas entre as camadas.

# -------------------------------------------------------------------------
# Auxiliares
# -------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

normalizar_nao_classificado <- function(x) {
  x <- as.character(x)
  
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x == "" ~ NA_character_,
    x %in% c("nao_classificado", "não_classificado", "sem_classificacao", "sem_classificação") ~ NA_character_,
    TRUE ~ x
  )
}

pegar_primeira_coluna <- function(dados, candidatos, novo_nome) {
  candidatos <- candidatos[candidatos %in% names(dados)]
  
  if (length(candidatos) == 0) {
    return(tibble::tibble(!!novo_nome := NA_character_))
  }
  
  tibble::tibble(
    !!novo_nome := dados[[candidatos[[1]]]]
  )
}

garantir_coluna_logica <- function(dados, nome) {
  if (!nome %in% names(dados)) {
    dados[[nome]] <- FALSE
  }
  
  dados[[nome]] <- as.logical(dados[[nome]])
  dados
}

# -------------------------------------------------------------------------
# Construção da comparação
# -------------------------------------------------------------------------

build_comparacao_camadas <- function(
    atendimentos,
    classificacao_cid,
    classificacao_regex,
    classificacao_llm
) {
  stopifnot(is.data.frame(atendimentos))
  
  base <- atendimentos |>
    dplyr::select(
      record_id,
      dplyr::any_of(c(
        "data_atendimento",
        "unidade",
        "idade",
        "faixa_etaria",
        "sexo",
        "cid",
        "cid_nome",
        "queixa",
        "anamnese",
        "texto_clinico"
      ))
    )
  
  cid <- classificacao_cid |>
    dplyr::select(record_id, dplyr::everything())
  
  sindrome_cid_tbl <- pegar_primeira_coluna(
    cid,
    candidatos = c(
      "sindrome_cid",
      "classificacao_cid",
      "grupo_cid",
      "sindrome_principal_cid"
    ),
    novo_nome = "sindrome_cid"
  )
  
  cid_informativo_tbl <- pegar_primeira_coluna(
    cid,
    candidatos = c("cid_informativo"),
    novo_nome = "cid_informativo"
  )
  
  cid_padronizado <- tibble::tibble(record_id = cid$record_id) |>
    dplyr::bind_cols(sindrome_cid_tbl, cid_informativo_tbl) |>
    dplyr::mutate(
      sindrome_cid = normalizar_nao_classificado(.data$sindrome_cid),
      cid_informativo = dplyr::case_when(
        is.logical(.data$cid_informativo) ~ .data$cid_informativo,
        is.na(.data$cid_informativo) ~ !is.na(.data$sindrome_cid),
        TRUE ~ as.logical(.data$cid_informativo)
      ),
      cid_classificado = !is.na(.data$sindrome_cid)
    )
  
  regex <- classificacao_regex |>
    dplyr::select(record_id, dplyr::everything())
  
  sindrome_regex_tbl <- pegar_primeira_coluna(
    regex,
    candidatos = c(
      "sindrome_regex",
      "sindrome_principal_regex",
      "classificacao_regex"
    ),
    novo_nome = "sindrome_regex"
  )
  
  sintomas_regex_tbl <- pegar_primeira_coluna(
    regex,
    candidatos = c("sintomas_regex"),
    novo_nome = "sintomas_regex"
  )
  
  regex_padronizado <- tibble::tibble(record_id = regex$record_id) |>
    dplyr::bind_cols(sindrome_regex_tbl, sintomas_regex_tbl) |>
    dplyr::mutate(
      sindrome_regex = normalizar_nao_classificado(.data$sindrome_regex),
      regex_classificado = !is.na(.data$sindrome_regex)
    )
  
  llm <- classificacao_llm |>
    dplyr::select(record_id, dplyr::everything())
  
  sindrome_llm_tbl <- pegar_primeira_coluna(
    llm,
    candidatos = c(
      "sindrome_llm",
      "sindrome_principal_llm",
      "classificacao_llm"
    ),
    novo_nome = "sindrome_llm"
  )
  
  justificativa_llm_tbl <- pegar_primeira_coluna(
    llm,
    candidatos = c("justificativa_llm"),
    novo_nome = "justificativa_llm"
  )
  
  sintomas_llm_tbl <- pegar_primeira_coluna(
    llm,
    candidatos = c("sintomas_identificados", "sintomas_llm"),
    novo_nome = "sintomas_llm"
  )
  
  parse_ok_tbl <- pegar_primeira_coluna(
    llm,
    candidatos = c("parse_ok"),
    novo_nome = "parse_ok"
  )
  
  llm_padronizado <- tibble::tibble(record_id = llm$record_id) |>
    dplyr::bind_cols(
      sindrome_llm_tbl,
      justificativa_llm_tbl,
      sintomas_llm_tbl,
      parse_ok_tbl
    ) |>
    dplyr::mutate(
      sindrome_llm = normalizar_nao_classificado(.data$sindrome_llm),
      llm_classificado = !is.na(.data$sindrome_llm)
    )
  
  comparacao <- base |>
    dplyr::left_join(cid_padronizado, by = "record_id") |>
    dplyr::left_join(regex_padronizado, by = "record_id") |>
    dplyr::left_join(llm_padronizado, by = "record_id") |>
    dplyr::mutate(
      cid_classificado = dplyr::coalesce(.data$cid_classificado, FALSE),
      regex_classificado = dplyr::coalesce(.data$regex_classificado, FALSE),
      llm_classificado = dplyr::coalesce(.data$llm_classificado, FALSE),
      cid_informativo = dplyr::coalesce(.data$cid_informativo, FALSE),
      
      n_camadas_classificaram =
        as.integer(.data$cid_classificado) +
        as.integer(.data$regex_classificado) +
        as.integer(.data$llm_classificado),
      
      cid_regex_concordam =
        .data$cid_classificado &
        .data$regex_classificado &
        .data$sindrome_cid == .data$sindrome_regex,
      
      cid_llm_concordam =
        .data$cid_classificado &
        .data$llm_classificado &
        .data$sindrome_cid == .data$sindrome_llm,
      
      regex_llm_concordam =
        .data$regex_classificado &
        .data$llm_classificado &
        .data$sindrome_regex == .data$sindrome_llm,
      
      tres_camadas_concordam =
        .data$cid_classificado &
        .data$regex_classificado &
        .data$llm_classificado &
        .data$sindrome_cid == .data$sindrome_regex &
        .data$sindrome_regex == .data$sindrome_llm,
      
      divergencia_com_cid_informativo =
        .data$cid_informativo &
        (
          (.data$regex_classificado & .data$sindrome_regex != .data$sindrome_cid) |
            (.data$llm_classificado & .data$sindrome_llm != .data$sindrome_cid)
        ),
      
      divergencia_textual_regex_llm =
        .data$regex_classificado &
        .data$llm_classificado &
        .data$sindrome_regex != .data$sindrome_llm,
      
      padrao_comparacao = dplyr::case_when(
        .data$n_camadas_classificaram == 0 ~ "sem_classificacao",
        .data$tres_camadas_concordam ~ "tres_camadas_concordantes",
        .data$n_camadas_classificaram == 3 &
          !.data$cid_regex_concordam &
          !.data$cid_llm_concordam &
          !.data$regex_llm_concordam ~ "tres_camadas_divergentes",
        .data$cid_regex_concordam & !.data$llm_classificado ~ "cid_regex_concordam_sem_llm",
        .data$cid_llm_concordam & !.data$regex_classificado ~ "cid_llm_concordam_sem_regex",
        .data$regex_llm_concordam & !.data$cid_classificado ~ "regex_llm_concordam_sem_cid",
        .data$cid_regex_concordam & .data$llm_classificado ~ "cid_regex_concordam_llm_diverge",
        .data$cid_llm_concordam & .data$regex_classificado ~ "cid_llm_concordam_regex_diverge",
        .data$regex_llm_concordam & .data$cid_classificado ~ "regex_llm_concordam_cid_diverge",
        .data$cid_classificado & !.data$regex_classificado & !.data$llm_classificado ~ "apenas_cid",
        !.data$cid_classificado & .data$regex_classificado & !.data$llm_classificado ~ "apenas_regex",
        !.data$cid_classificado & !.data$regex_classificado & .data$llm_classificado ~ "apenas_llm",
        TRUE ~ "divergencia_parcial"
      )
    )
  
  comparacao
}

# Nome usado no QMD.
comparar_camadas_classificacao <- build_comparacao_camadas

# -------------------------------------------------------------------------
# Resumos
# -------------------------------------------------------------------------

summarise_comparacao_camadas <- function(comparacao) {
  stopifnot(is.data.frame(comparacao))
  
  comparacao |>
    dplyr::count(.data$padrao_comparacao, name = "n") |>
    dplyr::mutate(
      percentual = round(100 * .data$n / sum(.data$n), 1)
    ) |>
    dplyr::arrange(dplyr::desc(.data$n))
}

summarise_camadas_disponiveis <- function(comparacao) {
  stopifnot(is.data.frame(comparacao))
  
  tibble::tibble(
    camada = c("CID", "regex", "LLM"),
    n_classificados = c(
      sum(comparacao$cid_classificado, na.rm = TRUE),
      sum(comparacao$regex_classificado, na.rm = TRUE),
      sum(comparacao$llm_classificado, na.rm = TRUE)
    ),
    total = nrow(comparacao)
  ) |>
    dplyr::mutate(
      percentual = round(100 * .data$n_classificados / .data$total, 1)
    )
}

calcular_concordancia <- function(x, y) {
  comparavel <- !is.na(x) & !is.na(y)
  n_comparavel <- sum(comparavel)
  
  if (n_comparavel == 0) {
    return(tibble::tibble(
      n_comparavel = 0L,
      n_concordante = 0L,
      percentual_concordancia = NA_real_
    ))
  }
  
  n_concordante <- sum(x[comparavel] == y[comparavel])
  
  tibble::tibble(
    n_comparavel = n_comparavel,
    n_concordante = n_concordante,
    percentual_concordancia = round(100 * n_concordante / n_comparavel, 1)
  )
}

summarise_concordancia_par_a_par <- function(comparacao) {
  stopifnot(is.data.frame(comparacao))
  
  cid_regex <- calcular_concordancia(
    comparacao$sindrome_cid,
    comparacao$sindrome_regex
  ) |>
    dplyr::mutate(par = "CID × regex")
  
  cid_llm <- calcular_concordancia(
    comparacao$sindrome_cid,
    comparacao$sindrome_llm
  ) |>
    dplyr::mutate(par = "CID × LLM")
  
  regex_llm <- calcular_concordancia(
    comparacao$sindrome_regex,
    comparacao$sindrome_llm
  ) |>
    dplyr::mutate(par = "regex × LLM")
  
  dplyr::bind_rows(cid_regex, cid_llm, regex_llm) |>
    dplyr::select(
      par,
      n_comparavel,
      n_concordante,
      percentual_concordancia
    )
}

summarise_matriz_cid_llm <- function(comparacao) {
  stopifnot(is.data.frame(comparacao))
  
  comparacao |>
    dplyr::filter(!is.na(.data$sindrome_cid), !is.na(.data$sindrome_llm)) |>
    dplyr::count(.data$sindrome_cid, .data$sindrome_llm, name = "n") |>
    dplyr::group_by(.data$sindrome_cid) |>
    dplyr::mutate(
      percentual_linha = round(100 * .data$n / sum(.data$n), 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$sindrome_cid, dplyr::desc(.data$n))
}

summarise_matriz_regex_llm <- function(comparacao) {
  stopifnot(is.data.frame(comparacao))
  
  comparacao |>
    dplyr::filter(!is.na(.data$sindrome_regex), !is.na(.data$sindrome_llm)) |>
    dplyr::count(.data$sindrome_regex, .data$sindrome_llm, name = "n") |>
    dplyr::group_by(.data$sindrome_regex) |>
    dplyr::mutate(
      percentual_linha = round(100 * .data$n / sum(.data$n), 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$sindrome_regex, dplyr::desc(.data$n))
}

selecionar_amostra_auditoria <- function(comparacao, n = 20) {
  stopifnot(is.data.frame(comparacao))
  
  comparacao |>
    dplyr::mutate(
      prioridade_auditoria = dplyr::case_when(
        .data$divergencia_com_cid_informativo ~ 1L,
        .data$divergencia_textual_regex_llm ~ 2L,
        .data$padrao_comparacao == "tres_camadas_divergentes" ~ 3L,
        .data$padrao_comparacao == "divergencia_parcial" ~ 4L,
        TRUE ~ 9L
      )
    ) |>
    dplyr::arrange(.data$prioridade_auditoria, .data$record_id) |>
    dplyr::slice_head(n = n)
}

# -------------------------------------------------------------------------
# Gráficos
# -------------------------------------------------------------------------

plot_padroes_comparacao <- function(comparacao) {
  resumo <- summarise_comparacao_camadas(comparacao)
  
  ggplot2::ggplot(
    resumo,
    ggplot2::aes(
      x = stats::reorder(.data$padrao_comparacao, .data$n),
      y = .data$n
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Número de atendimentos",
      title = "Padrões de comparação entre CID, regex e LLM"
    ) +
    ggplot2::theme_minimal()
}

plot_concordancia_par_a_par <- function(comparacao) {
  resumo <- summarise_concordancia_par_a_par(comparacao)
  
  ggplot2::ggplot(
    resumo,
    ggplot2::aes(
      x = stats::reorder(.data$par, .data$percentual_concordancia),
      y = .data$percentual_concordancia
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Concordância (%)",
      title = "Concordância par a par entre camadas"
    ) +
    ggplot2::theme_minimal()
}