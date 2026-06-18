# R/12_auditoria_limitacoes.R -----------------------------------------------
# Funções para auditoria metodológica e discussão de limitações do pipeline.
#
# Este módulo não altera a classificação final. Ele cria indicadores de
# rastreabilidade, prioriza registros para revisão manual e resume pontos
# críticos para uso operacional.

# -------------------------------------------------------------------------
# Auxiliares
# -------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
  x
}

garantir_colunas_auditoria <- function(dados) {
  stopifnot(is.data.frame(dados))

  defaults <- list(
    record_id = NA_character_,
    cid = NA_character_,
    cid_nome = NA_character_,
    texto_clinico = NA_character_,
    sindrome_cid = NA_character_,
    sindrome_regex = NA_character_,
    sindrome_llm = NA_character_,
    sindrome_final = NA_character_,
    fonte_classificacao_final = NA_character_,
    regra_classificacao_final = NA_character_,
    revisao_manual_recomendada = FALSE,
    motivo_revisao = NA_character_,
    justificativa_llm = NA_character_,
    parse_ok = NA,
    cid_informativo = NA,
    regex_classificado = NA,
    llm_classificado = NA,
    divergencia_com_cid_informativo = NA,
    divergencia_textual_regex_llm = NA,
    padrao_comparacao = NA_character_
  )

  for (nm in names(defaults)) {
    if (!nm %in% names(dados)) {
      dados[[nm]] <- defaults[[nm]]
    }
  }

  dados
}

normalizar_classificacao_vazia <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x == "" ~ NA_character_,
    x %in% c("nao_classificado", "não_classificado", "sem_classificacao", "sem_classificação") ~ NA_character_,
    TRUE ~ x
  )
}

# -------------------------------------------------------------------------
# Base de auditoria
# -------------------------------------------------------------------------

build_base_auditoria <- function(classificacao_final) {
  dados <- classificacao_final |>
    garantir_colunas_auditoria() |>
    dplyr::mutate(
      sindrome_cid_norm = normalizar_classificacao_vazia(.data$sindrome_cid),
      sindrome_regex_norm = normalizar_classificacao_vazia(.data$sindrome_regex),
      sindrome_llm_norm = normalizar_classificacao_vazia(.data$sindrome_llm),
      sindrome_final_norm = normalizar_classificacao_vazia(.data$sindrome_final),

      cid_classificou = !is.na(.data$sindrome_cid_norm),
      regex_classificou = !is.na(.data$sindrome_regex_norm),
      llm_classificou = !is.na(.data$sindrome_llm_norm),
      final_classificou = !is.na(.data$sindrome_final_norm),

      llm_parse_falhou = !is.na(.data$parse_ok) & !isTRUE(.data$parse_ok),
      sem_texto_clinico = is.na(.data$texto_clinico) | stringr::str_squish(.data$texto_clinico) == "",

      regex_llm_concordam = .data$regex_classificou &
        .data$llm_classificou &
        .data$sindrome_regex_norm == .data$sindrome_llm_norm,

      regex_llm_divergem = .data$regex_classificou &
        .data$llm_classificou &
        .data$sindrome_regex_norm != .data$sindrome_llm_norm,

      cid_texto_diverge = .data$cid_classificou &
        (
          (.data$regex_classificou & .data$sindrome_regex_norm != .data$sindrome_cid_norm) |
            (.data$llm_classificou & .data$sindrome_llm_norm != .data$sindrome_cid_norm)
        ),

      classificacao_por_apenas_uma_camada_textual =
        (.data$regex_classificou & !.data$llm_classificou & !.data$cid_classificou) |
        (!.data$regex_classificou & .data$llm_classificou & !.data$cid_classificou),

      cid_generico_refinado_por_texto = stringr::str_detect(
        .data$regra_classificacao_final %||% "",
        "cid_inespecifico|refina|texto_refina"
      ),

      prioridade_auditoria = dplyr::case_when(
        .data$llm_parse_falhou ~ 1L,
        .data$revisao_manual_recomendada ~ 2L,
        .data$regex_llm_divergem ~ 3L,
        .data$cid_texto_diverge ~ 4L,
        .data$classificacao_por_apenas_uma_camada_textual ~ 5L,
        .data$cid_generico_refinado_por_texto ~ 6L,
        !.data$final_classificou ~ 7L,
        TRUE ~ 9L
      ),

      grupo_auditoria = dplyr::case_when(
        .data$llm_parse_falhou ~ "falha_parse_llm",
        .data$revisao_manual_recomendada ~ "revisao_manual_recomendada",
        .data$regex_llm_divergem ~ "divergencia_regex_llm",
        .data$cid_texto_diverge ~ "divergencia_cid_texto",
        .data$classificacao_por_apenas_uma_camada_textual ~ "apenas_uma_camada_textual",
        .data$cid_generico_refinado_por_texto ~ "cid_generico_refinado_por_texto",
        !.data$final_classificou ~ "nao_classificado_final",
        TRUE ~ "sem_alerta_auditoria"
      )
    )

  dados
}

summarise_grupos_auditoria <- function(classificacao_final) {
  classificacao_final |>
    dplyr::mutate(
      grupo_auditoria = dplyr::case_when(
        .data$revisao_manual_recomendada ~ "Revisão manual recomendada",
        .data$fonte_classificacao_final %in% c(
          "texto_refina_cid_inespecifico",
          "regex_llm_concordantes"
        ) ~ "Classificação textual aceita",
        .data$fonte_classificacao_final %in% c(
          "cid_especifico_soberano",
          "cid_soberano"
        ) ~ "CID específico predominante",
        .data$sindrome_final == "nao_classificado" |
          is.na(.data$sindrome_final) ~ "Não classificado",
        TRUE ~ "Outros"
      ),
      prioridade = dplyr::case_when(
        .data$grupo_auditoria == "Revisão manual recomendada" ~ 1L,
        .data$grupo_auditoria == "Classificação textual aceita" ~ 2L,
        .data$grupo_auditoria == "CID específico predominante" ~ 3L,
        .data$grupo_auditoria == "Não classificado" ~ 4L,
        TRUE ~ 9L
      )
    ) |>
    dplyr::count(.data$grupo_auditoria, .data$prioridade, name = "n") |>
    dplyr::mutate(
      percentual = round(100 * .data$n / sum(.data$n), 1)
    ) |>
    dplyr::arrange(.data$prioridade) |>
    dplyr::select(-.data$prioridade)
}

summarise_indicadores_auditoria <- function(base_auditoria) {
  total <- nrow(base_auditoria)

  tibble::tibble(
    indicador = c(
      "Registros com classificação final",
      "Registros sem classificação final",
      "Revisão manual recomendada",
      "Divergência regex × LLM",
      "Divergência CID × texto",
      "CID genérico refinado pelo texto",
      "Falha de parse LLM",
      "Texto clínico ausente ou vazio"
    ),
    n = c(
      sum(base_auditoria$final_classificou, na.rm = TRUE),
      sum(!base_auditoria$final_classificou, na.rm = TRUE),
      sum(base_auditoria$revisao_manual_recomendada, na.rm = TRUE),
      sum(base_auditoria$regex_llm_divergem, na.rm = TRUE),
      sum(base_auditoria$cid_texto_diverge, na.rm = TRUE),
      sum(base_auditoria$cid_generico_refinado_por_texto, na.rm = TRUE),
      sum(base_auditoria$llm_parse_falhou, na.rm = TRUE),
      sum(base_auditoria$sem_texto_clinico, na.rm = TRUE)
    )
  ) |>
    dplyr::mutate(percentual = round(100 * .data$n / total, 1))
}

selecionar_amostra_auditoria_metodologica <- function(base_auditoria, n = 30) {
  base_auditoria |>
    dplyr::arrange(.data$prioridade_auditoria, .data$record_id) |>
    dplyr::select(
      dplyr::any_of(c(
        "record_id",
        "cid",
        "cid_nome",
        "sindrome_cid",
        "sindrome_regex",
        "sindrome_llm",
        "sindrome_final",
        "fonte_classificacao_final",
        "regra_classificacao_final",
        "grupo_auditoria",
        "motivo_revisao",
        "justificativa_llm",
        "texto_clinico"
      ))
    ) |>
    dplyr::slice_head(n = n)
}

summarise_fontes_para_auditoria <- function(base_auditoria) {
  base_auditoria |>
    dplyr::count(.data$fonte_classificacao_final, .data$grupo_auditoria, name = "n") |>
    dplyr::group_by(.data$fonte_classificacao_final) |>
    dplyr::mutate(percentual_na_fonte = round(100 * .data$n / sum(.data$n), 1)) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$fonte_classificacao_final, dplyr::desc(.data$n))
}

# -------------------------------------------------------------------------
# Tabelas narrativas de limitações e recomendações
# -------------------------------------------------------------------------

listar_limitacoes_pipeline <- function() {
  tibble::tibble(
    dimensao = c(
      "Base sintética",
      "CID",
      "Regex",
      "LLM",
      "Texto clínico",
      "Classificação final",
      "Validação",
      "Uso operacional"
    ),
    limitacao = c(
      "Os resultados demonstrativos foram produzidos sobre base sintética, ainda não sobre a base real do cliente.",
      "CIDs podem ser ausentes, genéricos, conflitantes ou representar hipótese diagnóstica não descrita no texto.",
      "Regex captura padrões explícitos, mas pode perder variações linguísticas não previstas ou contextos ambíguos.",
      "LLMs podem falhar no JSON, interpretar ambiguidades de forma instável ou inferir além do texto disponível.",
      "Queixa e anamnese podem ser curtas, incompletas, negadas, abreviadas ou conter erros de digitação.",
      "A classificação final é uma regra operacional, não diagnóstico etiológico individual.",
      "O desempenho real depende de validação manual com amostra rotulada por especialistas.",
      "O pipeline deve apoiar vigilância e priorização, não substituir investigação epidemiológica ou revisão clínica."
    ),
    mitigacao = c(
      "Reexecutar o pipeline com dados reais e comparar distribuição, qualidade textual e padrões de divergência.",
      "Separar CIDs específicos de CIDs genéricos e permitir refinamento textual quando apropriado.",
      "Manter dicionário versionado e revisar falsos negativos/falsos positivos periodicamente.",
      "Usar modo estruturado, prompt versionado, validação de schema, logs e fallback seguro.",
      "Medir completude textual e criar indicadores de suficiência do texto para classificação.",
      "Manter rastreabilidade da fonte decisória e sinalizar revisão manual quando necessário.",
      "Construir padrão-ouro com dupla revisão, consenso e métricas de sensibilidade, especificidade e F1.",
      "Definir protocolo de uso, rotina de auditoria, responsáveis e critérios de escalonamento."
    )
  )
}

listar_recomendacoes_validacao <- function() {
  tibble::tibble(
    etapa = c(
      "Amostragem",
      "Rotulagem manual",
      "Métricas",
      "Auditoria de divergências",
      "Calibração",
      "Monitoramento contínuo"
    ),
    recomendacao = c(
      "Selecionar amostra estratificada por síndrome final, fonte decisória, unidade, semana e grupo de auditoria.",
      "Utilizar ao menos dois revisores independentes e etapa de consenso para casos discordantes.",
      "Calcular sensibilidade, especificidade, valor preditivo positivo, F1 e matriz de confusão por síndrome.",
      "Priorizar registros com CID/texto divergente, regex/LLM divergente, uso de apenas uma camada textual e falhas de parse.",
      "Ajustar dicionários, prompt e regras de decisão com base nos erros observados, mantendo versionamento.",
      "Acompanhar semanalmente volume, proporção de revisão manual, falhas LLM e mudanças bruscas no perfil de classificação."
    )
  )
}

# -------------------------------------------------------------------------
# Gráficos
# -------------------------------------------------------------------------

plot_grupos_auditoria <- function(base_auditoria) {
  resumo <- summarise_grupos_auditoria(base_auditoria)

  ggplot2::ggplot(
    resumo,
    ggplot2::aes(
      x = stats::reorder(.data$grupo_auditoria, .data$n),
      y = .data$n
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Número de atendimentos",
      title = "Grupos de auditoria metodológica"
    ) +
    ggplot2::theme_minimal()
}

plot_indicadores_auditoria <- function(base_auditoria) {
  resumo <- summarise_indicadores_auditoria(base_auditoria)

  ggplot2::ggplot(
    resumo,
    ggplot2::aes(
      x = stats::reorder(.data$indicador, .data$n),
      y = .data$n
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Número de atendimentos",
      title = "Indicadores principais de auditoria"
    ) +
    ggplot2::theme_minimal()
}
