# ============================================================================
# R/05_cid_classification.R
#
# Classificação sindrômica por CID.
#
# Princípio:
# O CID é considerado camada soberana. Quando o CID principal estiver presente
# e mapeado para uma síndrome, a classificação por CID prevalecerá na etapa
# final do pipeline.
# ============================================================================

get_cid_syndrome_map <- function() {

  tibble::tribble(
    ~cid_prefix, ~classificacao_cid, ~grupo_cid, ~descricao_grupo,

    # Síndrome febril respiratória
    "J00", "febril_respiratoria", "respiratorio", "Nasofaringite aguda",
    "J01", "febril_respiratoria", "respiratorio", "Sinusite aguda",
    "J02", "febril_respiratoria", "respiratorio", "Faringite aguda",
    "J03", "febril_respiratoria", "respiratorio", "Amigdalite aguda",
    "J04", "febril_respiratoria", "respiratorio", "Laringite e traqueíte agudas",
    "J05", "febril_respiratoria", "respiratorio", "Laringite obstrutiva aguda e epiglotite",
    "J06", "febril_respiratoria", "respiratorio", "Infecções agudas das vias aéreas superiores",
    "J09", "febril_respiratoria", "respiratorio", "Influenza por vírus zoonótico ou pandêmico",
    "J10", "febril_respiratoria", "respiratorio", "Influenza por vírus identificado",
    "J11", "febril_respiratoria", "respiratorio", "Influenza por vírus não identificado",
    "J12", "febril_respiratoria", "respiratorio", "Pneumonia viral",
    "J13", "febril_respiratoria", "respiratorio", "Pneumonia por Streptococcus pneumoniae",
    "J14", "febril_respiratoria", "respiratorio", "Pneumonia por Haemophilus influenzae",
    "J15", "febril_respiratoria", "respiratorio", "Pneumonia bacteriana",
    "J16", "febril_respiratoria", "respiratorio", "Pneumonia por outros microrganismos",
    "J18", "febril_respiratoria", "respiratorio", "Pneumonia por microrganismo não especificado",
    "U07", "febril_respiratoria", "respiratorio", "COVID-19",

    # Síndrome febril gastrointestinal
    "A00", "febril_gastrointestinal", "gastrointestinal", "Cólera",
    "A01", "febril_gastrointestinal", "gastrointestinal", "Febres tifoide e paratifoide",
    "A02", "febril_gastrointestinal", "gastrointestinal", "Infecções por Salmonella",
    "A03", "febril_gastrointestinal", "gastrointestinal", "Shiguelose",
    "A04", "febril_gastrointestinal", "gastrointestinal", "Outras infecções intestinais bacterianas",
    "A05", "febril_gastrointestinal", "gastrointestinal", "Intoxicações alimentares bacterianas",
    "A06", "febril_gastrointestinal", "gastrointestinal", "Amebíase",
    "A07", "febril_gastrointestinal", "gastrointestinal", "Outras doenças intestinais por protozoários",
    "A08", "febril_gastrointestinal", "gastrointestinal", "Infecções intestinais virais",
    "A09", "febril_gastrointestinal", "gastrointestinal", "Diarreia e gastroenterite infecciosa presumível",

    # Síndrome febril exantemática
    "B05", "febril_exantematica", "exantematico", "Sarampo",
    "B06", "febril_exantematica", "exantematico", "Rubéola",
    "A38", "febril_exantematica", "exantematico", "Escarlatina",

    # Síndrome febril hemorrágica
    "A90", "febril_hemorragica", "hemorragico", "Dengue",
    "A91", "febril_hemorragica", "hemorragico", "Febre hemorrágica devida ao vírus da dengue",
    "A92", "febril_hemorragica", "hemorragico", "Outras febres virais transmitidas por mosquitos",
    "A96", "febril_hemorragica", "hemorragico", "Febre hemorrágica por arenavírus",
    "A98", "febril_hemorragica", "hemorragico", "Outras febres hemorrágicas virais",
    "A99", "febril_hemorragica", "hemorragico", "Febre hemorrágica viral não especificada",

    # Síndrome febril íctero-hemorrágica
    "A27", "febril_ictero_hemorragica", "ictero_hemorragico", "Leptospirose",
    "A95", "febril_ictero_hemorragica", "ictero_hemorragico", "Febre amarela",
    "B15", "febril_ictero_hemorragica", "ictero_hemorragico", "Hepatite aguda A",
    "B16", "febril_ictero_hemorragica", "ictero_hemorragico", "Hepatite aguda B",
    "B17", "febril_ictero_hemorragica", "ictero_hemorragico", "Outras hepatites virais agudas",
    "B19", "febril_ictero_hemorragica", "ictero_hemorragico", "Hepatite viral não especificada",
    "B50", "febril_ictero_hemorragica", "ictero_hemorragico", "Malária por Plasmodium falciparum",
    "B51", "febril_ictero_hemorragica", "ictero_hemorragico", "Malária por Plasmodium vivax",
    "B52", "febril_ictero_hemorragica", "ictero_hemorragico", "Malária por Plasmodium malariae",
    "B53", "febril_ictero_hemorragica", "ictero_hemorragico", "Outras formas de malária",
    "B54", "febril_ictero_hemorragica", "ictero_hemorragico", "Malária não especificada",

    # Síndrome febril neurológica/meníngea
    "A39", "febril_neurologica_meningea", "neurologico_meningeo", "Infecção meningocócica",
    "G00", "febril_neurologica_meningea", "neurologico_meningeo", "Meningite bacteriana",
    "G01", "febril_neurologica_meningea", "neurologico_meningeo", "Meningite em doenças bacterianas classificadas em outra parte",
    "G02", "febril_neurologica_meningea", "neurologico_meningeo", "Meningite em outras doenças infecciosas e parasitárias",
    "G03", "febril_neurologica_meningea", "neurologico_meningeo", "Meningite por outras causas e não especificada",
    "G04", "febril_neurologica_meningea", "neurologico_meningeo", "Encefalite, mielite e encefalomielite",
    "G05", "febril_neurologica_meningea", "neurologico_meningeo", "Encefalite, mielite e encefalomielite em doenças classificadas em outra parte",

    # Síndrome febril inespecífica
    "R50", "febril_inespecifica", "inespecifico", "Febre de origem desconhecida",
    "B34", "febril_inespecifica", "inespecifico", "Infecção viral de localização não especificada"
  )

}

normalize_cid <- function(cid) {

  cid |>
    as.character() |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9]", "") |>
    stringr::str_squish()

}

classify_by_cid <- function(df, cid_col = "cid") {

  cid_map <- get_cid_syndrome_map()

  df_cid <- df |>
    dplyr::mutate(
      cid_original = .data[[cid_col]],
      cid_norm = normalize_cid(.data[[cid_col]])
    )

  # Regra por prefixo: permite capturar tanto A09 quanto A09.0/A090.
  out <- df_cid |>
    dplyr::rowwise() |>
    dplyr::mutate(
      cid_prefix_match = {
        if (is.na(cid_norm) || cid_norm == "") {
          NA_character_
        } else {
          hits <- cid_map$cid_prefix[stringr::str_starts(cid_norm, cid_map$cid_prefix)]
          if (length(hits) == 0) NA_character_ else hits[[which.max(nchar(hits))]]
        }
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      cid_map,
      by = c("cid_prefix_match" = "cid_prefix")
    ) |>
    dplyr::mutate(
      cid_informativo = !is.na(classificacao_cid),
      fonte_classificacao_cid = dplyr::if_else(
        cid_informativo,
        "CID mapeado",
        "CID ausente ou não mapeado"
      )
    )

  out

}

summarise_cid_classification <- function(df_cid) {

  tibble::tibble(
    indicador = c(
      "Registros avaliados",
      "Com CID informado",
      "Sem CID informado",
      "CID mapeado para síndrome",
      "CID informado, mas não mapeado",
      "Classificados por CID"
    ),
    valor = c(
      nrow(df_cid),
      sum(!is.na(df_cid$cid_norm) & df_cid$cid_norm != ""),
      sum(is.na(df_cid$cid_norm) | df_cid$cid_norm == ""),
      sum(df_cid$cid_informativo, na.rm = TRUE),
      sum(!is.na(df_cid$cid_norm) & df_cid$cid_norm != "" & !df_cid$cid_informativo, na.rm = TRUE),
      sum(df_cid$cid_informativo, na.rm = TRUE)
    )
  )

}

plot_cid_syndrome_distribution <- function(df_cid) {

  df_cid |>
    dplyr::filter(cid_informativo) |>
    dplyr::count(classificacao_cid, sort = TRUE) |>
    ggplot2::ggplot(
      ggplot2::aes(x = reorder(classificacao_cid, n), y = n)
    ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Atendimentos",
      title = "Classificação sindrômica por CID"
    ) +
    ggplot2::theme_minimal(base_size = 12)

}
