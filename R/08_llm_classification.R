# R/08_llm_classification.R -------------------------------------------------
# Classificação sindrômica por LLM.
#
# Esta versão mantém compatibilidade com o capítulo 07 e com o schema DuckDB:
# febre_presente, febre_negada, sintomas_identificados, parse_ok, modelo_llm.
# Também força raw_response a ser sempre character, evitando erro em bind_rows().

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

get_llm_allowed_syndromes <- function() {
  c(
    "febril_respiratoria",
    "febril_exantematica",
    "febril_gastrointestinal",
    "febril_hemorragica",
    "febril_ictero_hemorragica",
    "febril_neurologica_meningea",
    "febril_inespecifica",
    "nao_classificado"
  )
}

as_single_character <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  if (inherits(x, "json")) return(as.character(x)[1])
  if (is.list(x) || is.data.frame(x)) {
    return(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null") |> as.character())
  }
  as.character(x)[1]
}

as_logical_scalar <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(default)
  if (is.logical(x)) return(isTRUE(x[1]))
  x_chr <- tolower(trimws(as.character(x[1])))
  if (x_chr %in% c("true", "t", "1", "sim", "s", "yes")) return(TRUE)
  if (x_chr %in% c("false", "f", "0", "nao", "não", "n", "no")) return(FALSE)
  default
}

normalise_sintomas <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  x <- as.character(x)
  x <- x[!is.na(x) & trimws(x) != ""]
  unique(trimws(x))
}

validate_llm_syndrome <- function(x) {
  x <- as_single_character(x) %||% "nao_classificado"
  if (!x %in% get_llm_allowed_syndromes()) return("nao_classificado")
  x
}

build_llm_syndrome_prompt <- function(texto_clinico) {
  glue::glue(
    '
Você é um classificador sindrômico para vigilância em saúde.

Sua tarefa é ler exclusivamente o texto clínico de um atendimento e classificar o registro em UMA síndrome febril principal.

Você NÃO deve usar CID, diagnóstico codificado, exames laboratoriais externos, dados epidemiológicos externos ou conhecimento sobre surtos em curso. Use apenas o texto clínico informado.

Síndromes permitidas:

1. febril_respiratoria
   - Febre associada a tosse, coriza, dor de garganta, dispneia, congestão nasal, sintomas gripais ou respiratórios.

2. febril_exantematica
   - Febre associada a exantema, rash, manchas vermelhas, lesões cutâneas difusas ou quadro febril com erupção de pele.

3. febril_gastrointestinal
   - Febre associada a diarreia, vômitos, náusea, dor abdominal ou sintomas gastrointestinais predominantes.

4. febril_hemorragica
   - Febre associada a sangramento, epistaxe, gengivorragia, petéquias, equimoses, manchas roxas, sangue em vômito, urina ou fezes.

5. febril_ictero_hemorragica
   - Febre associada simultaneamente a icterícia ou sinais como pele/olhos amarelos, colúria, urina escura, e sinais hemorrágicos.

6. febril_neurologica_meningea
   - Febre associada a rigidez de nuca, sinais meníngeos, convulsão, confusão mental, rebaixamento do nível de consciência, cefaleia intensa com sinais neurológicos.

7. febril_inespecifica
   - Febre presente, mas sem eixo respiratório, exantemático, gastrointestinal, hemorrágico, ictérico ou neurológico predominante.

8. nao_classificado
   - Use quando não houver febre descrita, quando a febre estiver negada, ou quando o texto não sustentar uma síndrome febril.

Regras obrigatórias:

- Se houver negação de febre, como "nega febre", "sem febre", "afebril", "não refere febre" ou equivalente, classifique como nao_classificado, exceto se o próprio texto trouxer febre atual clara em outro trecho.
- Não atribua síndrome febril apenas por sintomas isolados sem febre.
- Não invente sintomas que não aparecem no texto.
- Não use confiança numérica.
- Retorne exclusivamente JSON válido.
- Não use markdown.
- Não escreva texto antes ou depois do JSON.
- Não inclua comentários.
- Não inclua tags como <think> ou raciocínio passo a passo.
- Em caso de dúvida, prefira nao_classificado.

Formato obrigatório da resposta:

{{
  "sindrome_principal_llm": "uma_das_sindromes_permitidas",
  "febre_presente": true,
  "febre_negada": false,
  "sintomas_identificados": ["lista", "de", "sintomas"],
  "justificativa_llm": "justificativa curta, objetiva e auditável"
}}

Texto clínico para classificar:

\"\"\"{texto_clinico}\"\"\"
'
  )
}

extract_json_object <- function(x) {
  x <- as_single_character(x) %||% ""
  x <- gsub("^```json\\s*|^```\\s*|```$", "", x)
  x <- trimws(x)
  ini <- regexpr("\\{", x)[1]
  fim_all <- gregexpr("\\}", x)[[1]]
  if (ini > 0 && length(fim_all) > 0 && fim_all[1] > 0) {
    fim <- max(fim_all)
    return(substr(x, ini, fim))
  }
  x
}

parse_llm_json <- function(raw_response) {
  raw_chr <- extract_json_object(raw_response)

  out <- tryCatch(
    {
      list(
        parse_ok = TRUE,
        data = jsonlite::fromJSON(raw_chr, simplifyVector = FALSE),
        error = NA_character_
      )
    },
    error = function(e) {
      list(
        parse_ok = FALSE,
        data = list(
          sindrome_principal_llm = "nao_classificado",
          febre_presente = FALSE,
          febre_negada = FALSE,
          sintomas_identificados = character(0),
          justificativa_llm = paste("Falha ao interpretar JSON da LLM:", conditionMessage(e))
        ),
        error = conditionMessage(e)
      )
    }
  )

  out
}

safe_parse_llm_json <- parse_llm_json

mock_llm_response <- function(texto_clinico) {
  texto <- texto_clinico %||% ""
  texto_norm <- texto |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower()

  febre_negada <- stringr::str_detect(
    texto_norm,
    "\\b(nega|sem|nao apresenta|não apresenta|nao refere|não refere|afebril|sem relato de)\\s+(febre|episodios febris|episodios de febre)"
  )

  febre_presente <- !febre_negada && stringr::str_detect(
    texto_norm,
    "\\b(febre|febril|febricula|temperatura alta|pico febril|episodio febril|episodios febris)\\b"
  )

  sintomas <- character(0)
  sindrome <- "nao_classificado"

  tem_resp <- stringr::str_detect(texto_norm, "tosse|coriza|garganta|dispneia|falta de ar|sibil|congestao nasal")
  tem_exant <- stringr::str_detect(texto_norm, "exantema|rash|mancha|manchas|vermelh|prurido|coceira")
  tem_gi <- stringr::str_detect(texto_norm, "diarre|vomit|nausea|enjoo|dor abdominal|dor de barriga")
  tem_hemo <- stringr::str_detect(texto_norm, "sangramento|sangue|epistaxe|petequia|equimose|mancha roxa|gengivorragia")
  tem_ict <- stringr::str_detect(texto_norm, "icter|amarela|amarelao|olhos amarelos|coluria|urina escura")
  tem_neuro <- stringr::str_detect(texto_norm, "rigidez de nuca|meningite|convuls|confusao|meningismo|cefaleia intensa")
  tem_inesp <- stringr::str_detect(texto_norm, "mialgia|dor no corpo|cefaleia|mal estar|prostracao|cansaco")

  if (febre_presente) sintomas <- c(sintomas, "febre")
  if (tem_resp) sintomas <- c(sintomas, "sintomas respiratórios")
  if (tem_exant) sintomas <- c(sintomas, "exantema/manchas")
  if (tem_gi) sintomas <- c(sintomas, "sintomas gastrointestinais")
  if (tem_hemo) sintomas <- c(sintomas, "sangramento")
  if (tem_ict) sintomas <- c(sintomas, "icterícia")
  if (tem_neuro) sintomas <- c(sintomas, "sinais neurológicos/meníngeos")
  if (tem_inesp) sintomas <- c(sintomas, "sintomas inespecíficos")

  if (febre_presente) {
    sindrome <- dplyr::case_when(
      tem_ict && tem_hemo ~ "febril_ictero_hemorragica",
      tem_hemo ~ "febril_hemorragica",
      tem_neuro ~ "febril_neurologica_meningea",
      tem_exant ~ "febril_exantematica",
      tem_gi ~ "febril_gastrointestinal",
      tem_resp ~ "febril_respiratoria",
      TRUE ~ "febril_inespecifica"
    )
  }

  justificativa <- if (febre_negada) {
    "Texto contém negação de febre; não classificado como síndrome febril pela camada LLM."
  } else if (sindrome == "nao_classificado") {
    "Texto não contém febre válida suficiente para classificação sindrômica febril."
  } else {
    paste0("Texto menciona ", paste(unique(sintomas), collapse = ", "), ", compatível com ", sindrome, ".")
  }

  jsonlite::toJSON(
    list(
      sindrome_principal_llm = sindrome,
      febre_presente = febre_presente,
      febre_negada = febre_negada,
      sintomas_identificados = unique(sintomas),
      justificativa_llm = justificativa
    ),
    auto_unbox = TRUE,
    null = "null"
  ) |>
    as.character()
}

classify_one_by_llm <- function(record_id,
                                texto_clinico,
                                config = make_llm_config(provider = "mock"),
                                prompt_version = "llm_sindromes_febris_v1",
                                sleep_seconds = 0) {
  prompt <- build_llm_syndrome_prompt(texto_clinico)

  raw_response <- tryCatch(
    {
      if (!is.null(config$provider) && identical(config$provider, "mock")) {
        mock_llm_response(texto_clinico)
      } else {
        llm_generate(prompt = prompt, config = config)
      }
    },
    error = function(e) {
      jsonlite::toJSON(
        list(
          sindrome_principal_llm = "nao_classificado",
          febre_presente = FALSE,
          febre_negada = FALSE,
          sintomas_identificados = character(0),
          justificativa_llm = paste("Erro na chamada LLM:", conditionMessage(e))
        ),
        auto_unbox = TRUE,
        null = "null"
      ) |> as.character()
    }
  )

  raw_response_chr <- as_single_character(raw_response)
  parsed <- parse_llm_json(raw_response_chr)
  data <- parsed$data
  sintomas <- normalise_sintomas(data$sintomas_identificados %||% character(0))

  if (!is.null(sleep_seconds) && sleep_seconds > 0) Sys.sleep(sleep_seconds)

  tibble::tibble(
    record_id = as_single_character(record_id),
    sindrome_principal_llm = validate_llm_syndrome(data$sindrome_principal_llm %||% "nao_classificado"),
    febre_presente = as_logical_scalar(data$febre_presente %||% FALSE),
    febre_negada = as_logical_scalar(data$febre_negada %||% FALSE),
    sintomas_identificados = paste(sintomas, collapse = "; "),
    justificativa_llm = as_single_character(data$justificativa_llm %||% NA_character_),
    llm_classificado = validate_llm_syndrome(data$sindrome_principal_llm %||% "nao_classificado") != "nao_classificado",
    provider_llm = as_single_character(config$provider %||% NA_character_),
    modelo_llm = as_single_character(config$model %||% NA_character_),
    prompt_version = as_single_character(prompt_version),
    parse_ok = isTRUE(parsed$parse_ok),
    raw_response = raw_response_chr,
    data_classificacao = Sys.time()
  )
}

classify_by_llm <- function(dados,
                            config = make_llm_config(provider = "mock"),
                            max_records = Inf,
                            prompt_version = "llm_sindromes_febris_v1",
                            sleep_seconds = 0) {
  stopifnot(is.data.frame(dados))
  if (!"record_id" %in% names(dados)) stop("A base precisa conter a coluna `record_id`.", call. = FALSE)

  texto_col <- dplyr::case_when(
    "texto_clinico" %in% names(dados) ~ "texto_clinico",
    "texto_clinico_norm" %in% names(dados) ~ "texto_clinico_norm",
    TRUE ~ NA_character_
  )

  if (is.na(texto_col)) stop("A base precisa conter `texto_clinico` ou `texto_clinico_norm`.", call. = FALSE)

  dados_exec <- dados |>
    dplyr::select(record_id, texto_clinico = dplyr::all_of(texto_col)) |>
    dplyr::mutate(texto_clinico = dplyr::coalesce(.data$texto_clinico, ""))

  if (is.finite(max_records)) {
    dados_exec <- dados_exec |> dplyr::slice_head(n = max_records)
  }

  purrr::pmap_dfr(
    list(dados_exec$record_id, dados_exec$texto_clinico),
    function(record_id, texto_clinico) {
      classify_one_by_llm(
        record_id = record_id,
        texto_clinico = texto_clinico,
        config = config,
        prompt_version = prompt_version,
        sleep_seconds = sleep_seconds
      )
    }
  ) |>
    dplyr::mutate(
      raw_response = as.character(.data$raw_response),
      sintomas_identificados = as.character(.data$sintomas_identificados),
      justificativa_llm = as.character(.data$justificativa_llm)
    )
}

summarise_llm_classification <- function(dados_llm) {
  dados_llm |>
    dplyr::count(.data$sindrome_principal_llm, sort = TRUE, name = "n") |>
    dplyr::mutate(prop = .data$n / sum(.data$n))
}

plot_llm_syndrome_distribution <- function(dados_llm) {
  dados_llm |>
    dplyr::count(.data$sindrome_principal_llm, sort = TRUE) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = stats::reorder(.data$sindrome_principal_llm, .data$n),
        y = .data$n
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Número de atendimentos",
      title = "Distribuição da classificação sindrômica por LLM"
    ) +
    ggplot2::theme_minimal()
}

sample_llm_audit <- function(dados, classificacao_llm, n = 10) {
  dados |>
    dplyr::select(dplyr::any_of(c("record_id", "cid", "queixa", "anamnese", "texto_clinico"))) |>
    dplyr::left_join(classificacao_llm, by = "record_id") |>
    dplyr::slice_head(n = n)
}
