# R/10_classificacao_final.R --------------------------------------------------
# Classificação final do pipeline de vigilância sindrômica de síndromes febris.
#
# Esta etapa recebe a base comparativa CID x regex x LLM e produz uma única
# classificação final por atendimento.
#
# Ajuste metodológico importante:
# - O CID continua sendo soberano quando for específico/informativo para uma
#   síndrome operacional.
# - CIDs inespecíficos, como R50 e B34, não devem impedir que o texto clínico
#   detalhe melhor a síndrome quando regex e/ou LLM trouxerem informação mais
#   específica.
# - Assim, "CID soberano" não significa "qualquer CID vence"; significa que
#   CIDs específicos prevalecem. CIDs genéricos funcionam como evidência de
#   quadro febril/viral, mas podem ser refinados pela informação textual.

# -------------------------------------------------------------------------
# Auxiliares gerais
# -------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

normalizar_sindrome_final <- function(x) {
  x <- as.character(x)

  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x == "" ~ NA_character_,
    x %in% c("nao_classificado", "não_classificado", "sem_classificacao", "sem_classificação") ~ NA_character_,
    TRUE ~ x
  )
}

# Ordem operacional usada quando duas camadas textuais divergem e não há CID
# específico que resolva a classificação. A prioridade privilegia quadros menos
# frequentes, mais graves ou mais específicos para vigilância.
get_prioridade_sindromica_final <- function() {
  tibble::tibble(
    prioridade = c(1L, 2L, 3L, 4L, 5L, 6L, 7L),
    sindrome = c(
      "febril_ictero_hemorragica",
      "febril_hemorragica",
      "febril_neurologica_meningea",
      "febril_exantematica",
      "febril_gastrointestinal",
      "febril_respiratoria",
      "febril_inespecifica"
    ),
    criterio_operacional = c(
      "Combinação de icterícia e sangramento; síndrome menos frequente e mais específica.",
      "Presença de sangramento; potencial relevância para investigação e gravidade.",
      "Sinais neurológicos ou meníngeos; maior criticidade operacional.",
      "Quadro febril com exantema; relevante para doenças exantemáticas de interesse.",
      "Quadro febril com sintomas gastrointestinais.",
      "Quadro febril com sintomas respiratórios.",
      "Febre sem outro eixo sindrômico predominante."
    )
  )
}



resolver_por_prioridade_operacional <- function(sindromes) {
  # Remove ausentes, vazios e nao_classificado
  sindromes <- sindromes |>
    as.character()
  
  sindromes <- sindromes[
    !is.na(sindromes) &
      sindromes != "" &
      !sindromes %in% c(
        "nao_classificado",
        "não_classificado",
        "sem_classificacao",
        "sem_classificação"
      )
  ]
  
  if (length(sindromes) == 0) {
    return(NA_character_)
  }
  
  prioridade <- get_prioridade_sindromica_final()
  
  prioridade |>
    dplyr::filter(.data$sindrome %in% sindromes) |>
    dplyr::arrange(.data$prioridade) |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(.data$sindrome)
}

# -------------------------------------------------------------------------
# CID específico versus CID inespecífico
# -------------------------------------------------------------------------
# Alguns CIDs informam que há febre, virose ou quadro inespecífico, mas não
# definem uma síndrome operacional detalhada. Nesses casos, a informação textual
# pode refinar a classificação.
#
# Exemplos:
# - R50: febre de origem desconhecida
# - B34: infecção viral de localização não especificada
# - A90/A91 podem ser específicos para dengue, mas neste pipeline amplo podem
#   estar mapeados para síndrome exantemática/hemorrágica conforme configuração.
#
# A lista abaixo deve ser mantida conservadora. Ela identifica CIDs que não
# devem bloquear um refinamento textual mais específico.

get_cids_genericos_refinaveis <- function() {
  c(
    "R50",   # febre de origem desconhecida
    "R500",
    "R501",
    "R509",
    "B34",   # infecção viral de localização não especificada
    "B340",
    "B341",
    "B342",
    "B343",
    "B344",
    "B348",
    "B349",
    "B33",   # outras doenças virais não classificadas em outra parte
    "B330",
    "B331",
    "B332",
    "B333",
    "B334",
    "B338"
  )
}

normalizar_cid_final <- function(cid) {
  cid |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    toupper() |>
    stringr::str_replace_all("[^A-Z0-9]", "")
}

cid_eh_generico_refinavel <- function(cid, sindrome_cid = NA_character_) {
  cid_norm <- normalizar_cid_final(cid)
  cids_genericos <- get_cids_genericos_refinaveis()

  # Também tratamos a própria síndrome CID inespecífica como refinável, porque
  # o objetivo é não perder detalhe clínico quando regex/LLM concordam em uma
  # síndrome mais específica.
  cid_norm %in% cids_genericos | sindrome_cid %in% c("febril_inespecifica")
}

cid_eh_especifico_soberano <- function(cid, sindrome_cid, cid_informativo = TRUE) {
  sindrome_cid <- normalizar_sindrome_final(sindrome_cid)

  isTRUE(cid_informativo) &&
    !is.na(sindrome_cid) &&
    !cid_eh_generico_refinavel(cid = cid, sindrome_cid = sindrome_cid)
}

# -------------------------------------------------------------------------
# Classificação final de um registro
# -------------------------------------------------------------------------

classificar_registro_final <- function(
    cid,
    sindrome_cid,
    sindrome_regex,
    sindrome_llm,
    cid_informativo = FALSE,
    cid_classificado = FALSE,
    regex_classificado = FALSE,
    llm_classificado = FALSE
) {
  sindrome_cid <- normalizar_sindrome_final(sindrome_cid)
  sindrome_regex <- normalizar_sindrome_final(sindrome_regex)
  sindrome_llm <- normalizar_sindrome_final(sindrome_llm)

  cid_classificado <- isTRUE(cid_classificado) && !is.na(sindrome_cid)
  regex_classificado <- isTRUE(regex_classificado) && !is.na(sindrome_regex)
  llm_classificado <- isTRUE(llm_classificado) && !is.na(sindrome_llm)
  cid_informativo <- isTRUE(cid_informativo) && cid_classificado

  cid_generico_refinavel <- cid_classificado && cid_eh_generico_refinavel(
    cid = cid,
    sindrome_cid = sindrome_cid
  )

  cid_especifico_soberano <- cid_eh_especifico_soberano(
    cid = cid,
    sindrome_cid = sindrome_cid,
    cid_informativo = cid_informativo
  )

  textuais <- c(sindrome_regex, sindrome_llm)
  textuais_validas <- unique(stats::na.omit(textuais))
  ha_texto_mais_especifico <- any(textuais_validas != "febril_inespecifica")

  regex_llm_concordam <- regex_classificado &&
    llm_classificado &&
    identical(sindrome_regex, sindrome_llm)

  # 1. CID específico permanece soberano.
  if (cid_especifico_soberano) {
    divergencia_textual <-
      (regex_classificado && !identical(sindrome_regex, sindrome_cid)) ||
      (llm_classificado && !identical(sindrome_llm, sindrome_cid))

    return(tibble::tibble(
      sindrome_final = sindrome_cid,
      fonte_classificacao_final = "cid_especifico_soberano",
      regra_classificacao_final = "cid_especifico_prevalece",
      revisao_manual_recomendada = isTRUE(divergencia_textual),
      motivo_revisao = dplyr::case_when(
        divergencia_textual ~ "CID específico diverge de ao menos uma camada textual; revisar para auditoria.",
        TRUE ~ NA_character_
      )
    ))
  }

  # 2. CID genérico/inespecífico + regex e LLM concordantes em síndrome mais
  # específica: aceitar refinamento textual. Esse é o caso dos exemplos R50/B34
  # com regex e LLM apontando febril_hemorragica.
  if (cid_generico_refinavel && regex_llm_concordam && ha_texto_mais_especifico) {
    return(tibble::tibble(
      sindrome_final = sindrome_regex,
      fonte_classificacao_final = "texto_refina_cid_inespecifico",
      regra_classificacao_final = "cid_inespecifico_regex_llm_concordantes",
      revisao_manual_recomendada = FALSE,
      motivo_revisao = NA_character_
    ))
  }

  # 3. CID genérico/inespecífico + apenas uma camada textual mais específica:
  # aceitar refinamento, mas recomendar revisão.
  if (cid_generico_refinavel && ha_texto_mais_especifico && length(textuais_validas) == 1) {
    return(tibble::tibble(
      sindrome_final = textuais_validas[[1]],
      fonte_classificacao_final = dplyr::case_when(
        regex_classificado && !llm_classificado ~ "regex_refina_cid_inespecifico",
        !regex_classificado && llm_classificado ~ "llm_refina_cid_inespecifico",
        TRUE ~ "texto_refina_cid_inespecifico"
      ),
      regra_classificacao_final = "cid_inespecifico_uma_camada_textual_especifica",
      revisao_manual_recomendada = TRUE,
      motivo_revisao = "CID inespecífico refinado por apenas uma camada textual; recomenda-se revisão."
    ))
  }

  # 4. CID genérico/inespecífico + regex e LLM divergem: resolver por prioridade
  # operacional e recomendar revisão.
  if (cid_generico_refinavel && ha_texto_mais_especifico && length(textuais_validas) > 1) {
    sindrome_prioritaria <- resolver_por_prioridade_operacional(textuais_validas)

    return(tibble::tibble(
      sindrome_final = sindrome_prioritaria,
      fonte_classificacao_final = "prioridade_operacional_textual",
      regra_classificacao_final = "cid_inespecifico_regex_llm_divergentes",
      revisao_manual_recomendada = TRUE,
      motivo_revisao = "CID inespecífico com divergência entre regex e LLM; aplicada prioridade operacional e revisão recomendada."
    ))
  }

  # 5. CID genérico classificado, mas texto não trouxe informação mais específica.
  if (cid_generico_refinavel && cid_classificado) {
    return(tibble::tibble(
      sindrome_final = sindrome_cid,
      fonte_classificacao_final = "cid_inespecifico_sem_refinamento_textual",
      regra_classificacao_final = "cid_inespecifico_mantido",
      revisao_manual_recomendada = FALSE,
      motivo_revisao = NA_character_
    ))
  }

  # 6. Sem CID específico: regex e LLM concordantes.
  if (!cid_classificado && regex_llm_concordam) {
    return(tibble::tibble(
      sindrome_final = sindrome_regex,
      fonte_classificacao_final = "regex_llm_concordantes",
      regra_classificacao_final = "sem_cid_regex_llm_concordantes",
      revisao_manual_recomendada = FALSE,
      motivo_revisao = NA_character_
    ))
  }

  # 7. Sem CID específico: apenas regex classifica.
  if (!cid_classificado && regex_classificado && !llm_classificado) {
    return(tibble::tibble(
      sindrome_final = sindrome_regex,
      fonte_classificacao_final = "apenas_regex",
      regra_classificacao_final = "sem_cid_apenas_regex",
      revisao_manual_recomendada = TRUE,
      motivo_revisao = "Classificação final apoiada apenas na regex; recomenda-se revisão."
    ))
  }

  # 8. Sem CID específico: apenas LLM classifica.
  if (!cid_classificado && !regex_classificado && llm_classificado) {
    return(tibble::tibble(
      sindrome_final = sindrome_llm,
      fonte_classificacao_final = "apenas_llm",
      regra_classificacao_final = "sem_cid_apenas_llm",
      revisao_manual_recomendada = TRUE,
      motivo_revisao = "Classificação final apoiada apenas na LLM; recomenda-se revisão."
    ))
  }

  # 9. Sem CID específico: regex e LLM divergem.
  if (!cid_classificado && regex_classificado && llm_classificado && !regex_llm_concordam) {
    sindrome_prioritaria <- resolver_por_prioridade_operacional(c(sindrome_regex, sindrome_llm))

    return(tibble::tibble(
      sindrome_final = sindrome_prioritaria,
      fonte_classificacao_final = "prioridade_operacional_textual",
      regra_classificacao_final = "sem_cid_regex_llm_divergentes",
      revisao_manual_recomendada = TRUE,
      motivo_revisao = "Regex e LLM divergem; aplicada prioridade operacional e revisão recomendada."
    ))
  }

  # 10. Nenhuma camada classificou.
  tibble::tibble(
    sindrome_final = "nao_classificado",
    fonte_classificacao_final = "sem_classificacao",
    regra_classificacao_final = "nenhuma_camada_classificou",
    revisao_manual_recomendada = FALSE,
    motivo_revisao = NA_character_
  )
}

# -------------------------------------------------------------------------
# Classificação final da base
# -------------------------------------------------------------------------

classificar_base_final <- function(comparacao_camadas) {
  stopifnot(is.data.frame(comparacao_camadas))

  dados <- comparacao_camadas |>
    dplyr::mutate(
      sindrome_cid = normalizar_sindrome_final(.data$sindrome_cid),
      sindrome_regex = normalizar_sindrome_final(.data$sindrome_regex),
      sindrome_llm = normalizar_sindrome_final(.data$sindrome_llm),
      cid_classificado = dplyr::coalesce(as.logical(.data$cid_classificado), FALSE),
      regex_classificado = dplyr::coalesce(as.logical(.data$regex_classificado), FALSE),
      llm_classificado = dplyr::coalesce(as.logical(.data$llm_classificado), FALSE),
      cid_informativo = dplyr::coalesce(as.logical(.data$cid_informativo), .data$cid_classificado),
      cid_generico_refinavel = cid_eh_generico_refinavel(
        cid = .data$cid,
        sindrome_cid = .data$sindrome_cid
      ),
      cid_especifico_soberano = purrr::pmap_lgl(
        list(.data$cid, .data$sindrome_cid, .data$cid_informativo),
        ~ cid_eh_especifico_soberano(cid = ..1, sindrome_cid = ..2, cid_informativo = ..3)
      )
    )

  decisoes <- purrr::pmap_dfr(
    list(
      dados$cid,
      dados$sindrome_cid,
      dados$sindrome_regex,
      dados$sindrome_llm,
      dados$cid_informativo,
      dados$cid_classificado,
      dados$regex_classificado,
      dados$llm_classificado
    ),
    ~ classificar_registro_final(
      cid = ..1,
      sindrome_cid = ..2,
      sindrome_regex = ..3,
      sindrome_llm = ..4,
      cid_informativo = ..5,
      cid_classificado = ..6,
      regex_classificado = ..7,
      llm_classificado = ..8
    )
  )

  dados |>
    dplyr::bind_cols(decisoes) |>
    dplyr::mutate(
      classificado_final = .data$sindrome_final != "nao_classificado",
      tipo_resultado_final = dplyr::case_when(
        .data$sindrome_final == "nao_classificado" ~ "nao_classificado",
        .data$sindrome_final == "febril_inespecifica" ~ "febril_inespecifica",
        TRUE ~ "sindrome_especifica"
      )
    )
}

# Alias para estabilidade caso o QMD use outro nome.
gerar_classificacao_final <- classificar_base_final
build_classificacao_final <- classificar_base_final

# -------------------------------------------------------------------------
# Resumos e gráficos
# -------------------------------------------------------------------------

summarise_classificacao_final <- function(classificacao_final) {
  stopifnot(is.data.frame(classificacao_final))

  classificacao_final |>
    dplyr::count(.data$sindrome_final, name = "n") |>
    dplyr::mutate(
      percentual = round(100 * .data$n / sum(.data$n), 1)
    ) |>
    dplyr::arrange(dplyr::desc(.data$n))
}

summarise_fontes_classificacao_final <- function(classificacao_final) {
  stopifnot(is.data.frame(classificacao_final))

  classificacao_final |>
    dplyr::count(.data$fonte_classificacao_final, name = "n") |>
    dplyr::mutate(
      percentual = round(100 * .data$n / sum(.data$n), 1)
    ) |>
    dplyr::arrange(dplyr::desc(.data$n))
}

summarise_revisao_manual <- function(classificacao_final) {
  stopifnot(is.data.frame(classificacao_final))

  classificacao_final |>
    dplyr::count(.data$revisao_manual_recomendada, name = "n") |>
    dplyr::mutate(
      percentual = round(100 * .data$n / sum(.data$n), 1)
    ) |>
    dplyr::arrange(dplyr::desc(.data$revisao_manual_recomendada))
}

plot_distribuicao_classificacao_final <- function(classificacao_final) {
  resumo <- summarise_classificacao_final(classificacao_final)

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
      title = "Distribuição da classificação final"
    ) +
    ggplot2::theme_minimal()
}

plot_fontes_classificacao_final <- function(classificacao_final) {
  resumo <- summarise_fontes_classificacao_final(classificacao_final)

  ggplot2::ggplot(
    resumo,
    ggplot2::aes(
      x = stats::reorder(.data$fonte_classificacao_final, .data$n),
      y = .data$n
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Número de atendimentos",
      title = "Fonte da classificação final"
    ) +
    ggplot2::theme_minimal()
}

plot_classificacao_final <- function(classificacao_final) {
  resumo <- summarise_classificacao_final(classificacao_final)
  
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
      title = "Distribuição da classificação final"
    ) +
    ggplot2::theme_minimal()
}


selecionar_auditoria_classificacao_final <- function(classificacao_final, n = 20) {
  stopifnot(is.data.frame(classificacao_final))

  classificacao_final |>
    dplyr::filter(.data$revisao_manual_recomendada) |>
    dplyr::arrange(.data$regra_classificacao_final, .data$record_id) |>
    dplyr::slice_head(n = n)
}

# # -------------------------------------------------------------------------
# # DuckDB
# # -------------------------------------------------------------------------
# 
# write_classificacao_final <- function(con, dados, overwrite = TRUE) {
#   stopifnot(DBI::dbIsValid(con))
#   stopifnot(is.data.frame(dados))
# 
#   DBI::dbWriteTable(
#     con,
#     "tb_classificacao_final",
#     dados,
#     overwrite = overwrite,
#     append = !overwrite
#   )
# 
#   invisible(TRUE)
# }
# 
# read_classificacao_final <- function(con) {
#   stopifnot(DBI::dbIsValid(con))
# 
#   if (!DBI::dbExistsTable(con, "tb_classificacao_final")) {
#     return(tibble::tibble())
#   }
# 
#   DBI::dbReadTable(con, "tb_classificacao_final") |>
#     tibble::as_tibble()
# }

read_comparacao_camadas <- function(con) {
  stopifnot(DBI::dbIsValid(con))

  if (!DBI::dbExistsTable(con, "tb_comparacao_camadas")) {
    stop(
      "A tabela tb_comparacao_camadas não existe. ",
      "Execute primeiro o capítulo 08-comparacao-cid-regex-llm.qmd."
    )
  }

  DBI::dbReadTable(con, "tb_comparacao_camadas") |>
    tibble::as_tibble()
}
