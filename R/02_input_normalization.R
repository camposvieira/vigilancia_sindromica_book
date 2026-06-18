# ============================================================================
# R/02_input_normalization.R
#
# Funções de padronização e normalização da entrada de dados.
#
# Objetivo:
# Permitir que diferentes bases sejam convertidas para um schema interno único,
# independente da origem dos dados (CSV, Parquet, DuckDB, banco SQL etc.).
# ============================================================================

# ----------------------------------------------------------------------------
# Faixa etária padronizada
# ----------------------------------------------------------------------------

create_age_group <- function(age) {
  
  dplyr::case_when(
    is.na(age) ~ NA_character_,
    age < 5 ~ "0-4",
    age < 15 ~ "5-14",
    age < 40 ~ "15-39",
    age < 60 ~ "40-59",
    TRUE ~ "60+"
  )
  
}

# ----------------------------------------------------------------------------
# Normalização básica de texto
# ----------------------------------------------------------------------------

normalize_text <- function(text) {
  
  text |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^[:alnum:] ]", " ") |>
    stringr::str_squish()
  
}

# ----------------------------------------------------------------------------
# Schema interno do projeto
# ----------------------------------------------------------------------------
#
# record_id
# data_atendimento
# unidade
# idade
# faixa_etaria
# sexo
# cid
# cid_nome
# queixa
# anamnese
# texto_clinico
#
# ----------------------------------------------------------------------------

normalize_input_data <- function(df, mapping) {
  
  required_mapping <- c(
    "record_id",
    "data_atendimento",
    "unidade",
    "idade",
    "sexo",
    "cid",
    "queixa",
    "anamnese"
  )
  
  missing_mapping <- setdiff(
    required_mapping,
    names(mapping)
  )
  
  if(length(missing_mapping) > 0) {
    
    stop(
      "Campos ausentes no mapping: ",
      paste(missing_mapping, collapse = ", ")
    )
    
  }
  
  out <- tibble::tibble(
    
    record_id = df[[ mapping$record_id ]],
    
    data_atendimento =
      as.Date(df[[ mapping$data_atendimento ]]),
    
    unidade =
      as.character(df[[ mapping$unidade ]]),
    
    idade =
      suppressWarnings(
        as.numeric(df[[ mapping$idade ]])
      ),
    
    sexo =
      as.character(df[[ mapping$sexo ]]),
    
    cid =
      as.character(df[[ mapping$cid ]]),
    
    queixa =
      as.character(df[[ mapping$queixa ]]),
    
    anamnese =
      as.character(df[[ mapping$anamnese ]])
    
  ) |>
    
    dplyr::mutate(
      
      faixa_etaria =
        create_age_group(idade),
      
      texto_clinico =
        paste(
          queixa,
          anamnese,
          sep = " "
        ),
      
      texto_clinico_norm =
        normalize_text(texto_clinico)
      
    )
  
  out
  
}