# R/07_llm_clients.R ---------------------------------------------------------
# Clientes LLM desacoplados do provedor.
#
# Objetivo:
# - Permitir que o pipeline use LLM local, API gratuita/paga ou modo mock.
# - Manter a classificação sindrômica independente do fornecedor.
# - Garantir que o Quarto Book renderize sem internet, sem chave e sem modelo local.
#
# Provedores suportados:
# - mock
# - ollama
# - gemini
# - openai_compatible
#
# Observação:
# A camada de classificação não deve saber qual provedor está sendo usado.
# Ela deve chamar apenas llm_generate(prompt, config).
#
# Configuração recomendada por variáveis de ambiente:
#
# Modo padrão demonstrativo:
# LLM_PROVIDER=mock
# LLM_MODEL=mock-sindromico-v1
#
# Exemplo Ollama local:
# LLM_PROVIDER=ollama
# LLM_MODEL=qwen3.5:latest
# LLM_BASE_URL=http://localhost:11434
# LLM_JSON_MODE=true
# LLM_TIMEOUT_SEC=180
# LLM_MAX_TOKENS=1000
#
# Exemplo Gemini:
# LLM_PROVIDER=gemini
# LLM_MODEL=gemini-1.5-flash
# LLM_API_KEY=sua_chave_aqui
#
# Exemplo OpenAI-compatible:
# LLM_PROVIDER=openai_compatible
# LLM_MODEL=nome_do_modelo
# LLM_BASE_URL=https://api.exemplo.com/v1
# LLM_API_KEY=sua_chave_aqui

# -------------------------------------------------------------------------
# Operadores e auxiliares gerais
# -------------------------------------------------------------------------

# Operador de valor padrão.
# Retorna `y` quando `x` é NULL, vazio ou NA escalar.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  
  if (length(x) == 1 && is.na(x)) {
    return(y)
  }
  
  x
}

# Converte strings vindas de Sys.getenv em lógico.
# Aceita formas comuns em português/inglês.
env_to_logical <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || identical(x, "")) {
    return(default)
  }
  
  tolower(as.character(x[[1]])) %in% c("true", "1", "yes", "y", "sim", "s")
}

# Converte strings de ambiente em número, usando padrão se vier vazio/NA/inválido.
env_to_numeric <- function(x, default) {
  if (is.null(x) || length(x) == 0 || identical(x, "")) {
    return(default)
  }
  
  out <- suppressWarnings(as.numeric(x[[1]]))
  
  if (is.na(out)) {
    return(default)
  }
  
  out
}

# Remove barra final de uma URL, quando existir.
normalizar_base_url <- function(base_url) {
  base_url <- base_url %||% ""
  
  if (identical(base_url, "")) {
    return("")
  }
  
  stringr::str_remove(base_url, "/$")
}

# -------------------------------------------------------------------------
# Configuração
# -------------------------------------------------------------------------

make_llm_config <- function(
    provider = Sys.getenv("LLM_PROVIDER", unset = "mock"),
    model = Sys.getenv("LLM_MODEL", unset = "mock-sindromico-v1"),
    api_key = Sys.getenv("LLM_API_KEY", unset = ""),
    base_url = Sys.getenv("LLM_BASE_URL", unset = ""),
    temperature = env_to_numeric(Sys.getenv("LLM_TEMPERATURE", unset = "0"), default = 0),
    max_tokens = env_to_numeric(Sys.getenv("LLM_MAX_TOKENS", unset = "800"), default = 800),
    timeout_sec = env_to_numeric(Sys.getenv("LLM_TIMEOUT_SEC", unset = "120"), default = 120),
    json_mode = env_to_logical(Sys.getenv("LLM_JSON_MODE", unset = "true"), default = TRUE)
) {
  provider <- tolower(provider %||% "mock")
  
  providers_validos <- c(
    "mock",
    "ollama",
    "gemini",
    "openai_compatible"
  )
  
  if (!provider %in% providers_validos) {
    stop(
      "Provider LLM inválido: ", provider,
      ". Use: mock, ollama, gemini ou openai_compatible."
    )
  }
  
  # Garantias defensivas para evitar NULL em chamadas como httr2::req_timeout().
  temperature <- temperature %||% 0
  max_tokens <- max_tokens %||% 800
  timeout_sec <- timeout_sec %||% 120
  json_mode <- isTRUE(json_mode)
  
  list(
    provider = provider,
    model = model %||% "",
    api_key = api_key %||% "",
    base_url = base_url %||% "",
    temperature = temperature,
    max_tokens = max_tokens,
    timeout_sec = timeout_sec,
    json_mode = json_mode
  )
}

# Alias para compatibilidade com exemplos anteriores.
create_llm_config <- make_llm_config

# -------------------------------------------------------------------------
# Função principal
# -------------------------------------------------------------------------

llm_generate <- function(prompt, config = make_llm_config()) {
  if (is.null(config$provider)) {
    stop("A configuração LLM precisa conter o campo `provider`.")
  }
  
  provider <- tolower(config$provider)
  
  switch(
    provider,
    mock = llm_generate_mock(prompt = prompt, config = config),
    ollama = llm_generate_ollama(prompt = prompt, config = config),
    gemini = llm_generate_gemini(prompt = prompt, config = config),
    openai_compatible = llm_generate_openai_compatible(prompt = prompt, config = config),
    stop("Provider LLM não reconhecido: ", provider)
  )
}

# -------------------------------------------------------------------------
# Modo mock
# -------------------------------------------------------------------------
# O modo mock é obrigatório para que o Quarto Book renderize mesmo sem:
# - internet
# - chave de API
# - Ollama instalado
# - modelo local carregado
#
# Ele simula uma resposta JSON determinística a partir do texto contido no prompt.
# Isso preserva a reprodutibilidade da entrega e permite demonstrar a lógica
# metodológica sem depender de infraestrutura externa.

llm_generate_mock <- function(prompt, config = make_llm_config()) {
  texto <- stringi::stri_trans_general(prompt, "Latin-ASCII") |>
    tolower()
  
  febre_negada <- stringr::str_detect(
    texto,
    paste(
      c(
        "nega febre",
        "sem febre",
        "afebril",
        "nao apresenta febre",
        "nao refere febre",
        "sem relato de febre",
        "nega episodios febris"
      ),
      collapse = "|"
    )
  )
  
  febre_presente <- stringr::str_detect(
    texto,
    paste(
      c(
        "febre",
        "febril",
        "febricula",
        "temperatura alta",
        "fbre",
        "febree"
      ),
      collapse = "|"
    )
  ) && !febre_negada
  
  sintomas <- character(0)
  
  detecta <- function(padrao) {
    stringr::str_detect(texto, padrao)
  }
  
  if (detecta("tosse|coriza|nariz escorrendo|dor de garganta|falta de ar|dispneia")) {
    sintomas <- c(sintomas, "sintomas respiratórios")
  }
  
  if (detecta("exantema|rash|manchas|manchinhas|coceira")) {
    sintomas <- c(sintomas, "exantema")
  }
  
  if (detecta("diarreia|diarreiaa|vomito|vomitos|nausea|enjoo|dor de barriga")) {
    sintomas <- c(sintomas, "sintomas gastrointestinais")
  }
  
  if (detecta("sangramento|sangue|epistaxe|gengivorragia|petequias|manchas roxas")) {
    sintomas <- c(sintomas, "sangramento")
  }
  
  if (detecta("ictericia|amarelao|pele amarela|olhos amarelos|coluria|urina escura")) {
    sintomas <- c(sintomas, "icterícia")
  }
  
  if (detecta("rigidez de nuca|meningite|cefaleia intensa|convulsao|confusao mental")) {
    sintomas <- c(sintomas, "sinais neurológicos/meníngeos")
  }
  
  if (detecta("mialgia|dor no corpo|cefaleia|mal estar|prostracao|cansaco")) {
    sintomas <- c(sintomas, "sintomas inespecíficos")
  }
  
  sindrome <- dplyr::case_when(
    !febre_presente ~ "nao_classificado",
    "icterícia" %in% sintomas && "sangramento" %in% sintomas ~ "febril_ictero_hemorragica",
    "sangramento" %in% sintomas ~ "febril_hemorragica",
    "sinais neurológicos/meníngeos" %in% sintomas ~ "febril_neurologica_meningea",
    "exantema" %in% sintomas ~ "febril_exantematica",
    "sintomas gastrointestinais" %in% sintomas ~ "febril_gastrointestinal",
    "sintomas respiratórios" %in% sintomas ~ "febril_respiratoria",
    TRUE ~ "febril_inespecifica"
  )
  
  justificativa <- dplyr::case_when(
    febre_negada ~ "O texto contém negação de febre; por isso não foi classificado como síndrome febril.",
    sindrome == "nao_classificado" ~ "O texto não apresenta febre válida suficiente para classificação sindrômica.",
    length(sintomas) == 0 ~ "Classificação simulada em modo mock com base em febre válida, sem outros achados específicos.",
    TRUE ~ paste0(
      "Classificação simulada em modo mock com base em febre válida e nos achados textuais: ",
      paste(unique(sintomas), collapse = ", "),
      "."
    )
  )
  
  resposta <- list(
    sindrome_principal_llm = sindrome,
    febre_presente = isTRUE(febre_presente),
    febre_negada = isTRUE(febre_negada),
    sintomas_identificados = unique(sintomas),
    justificativa_llm = justificativa
  )
  
  jsonlite::toJSON(
    resposta,
    auto_unbox = TRUE,
    null = "null"
  )
}

# -------------------------------------------------------------------------
# Ollama local
# -------------------------------------------------------------------------
# Usa endpoint:
# POST /api/generate
#
# Configuração típica:
#
# Sys.setenv(
#   LLM_PROVIDER = "ollama",
#   LLM_MODEL = "qwen3.5:latest",
#   LLM_BASE_URL = "http://localhost:11434",
#   LLM_JSON_MODE = "true",
#   LLM_TIMEOUT_SEC = "180"
# )
#
# Observação:
# - `format = "json"` é enviado quando config$json_mode = TRUE.
# - Nem todos os modelos respeitam perfeitamente JSON; por isso o parsing
#   posterior continua tolerante a texto antes/depois do objeto JSON.

llm_generate_ollama <- function(prompt, config = make_llm_config()) {
  base_url <- config$base_url %||% ""
  
  if (identical(base_url, "")) {
    base_url <- "http://localhost:11434"
  }
  
  base_url <- stringr::str_remove(base_url, "/$")
  
  model <- config$model %||% "mistral:latest"
  temperature <- config$temperature %||% 0
  timeout_sec <- config$timeout_sec %||% 120
  json_mode <- isTRUE(config$json_mode)
  
  req_body <- list(
    model = model,
    prompt = prompt,
    stream = FALSE,
    options = list(
      temperature = temperature
    )
  )
  
  # Alguns modelos funcionam bem com format = "json";
  # outros, como alguns Qwen locais, podem responder melhor sem esse parâmetro.
  # Por isso o uso é controlado por LLM_JSON_MODE.
  if (json_mode) {
    req_body$format <- "json"
  }
  
  resp <- httr2::request(paste0(base_url, "/api/generate")) |>
    httr2::req_method("POST") |>
    httr2::req_timeout(seconds = timeout_sec) |>
    httr2::req_body_json(req_body) |>
    httr2::req_perform()
  
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  
  # O campo correto da API /api/generate do Ollama é `response`.
  # Alguns modelos também retornam `thinking`; esse campo é ignorado.
  resposta <- body$response %||% ""
  
  if (!identical(resposta, "")) {
    return(as.character(resposta))
  }
  
  # Diagnóstico explícito quando não houver resposta útil.
  jsonlite::toJSON(
    list(
      erro = "Resposta do Ollama sem campo `response` ou com `response` vazio.",
      model = model,
      endpoint = paste0(base_url, "/api/generate"),
      json_mode = json_mode,
      campos_recebidos = names(body),
      body = body
    ),
    auto_unbox = TRUE,
    null = "null"
  )
}

# -------------------------------------------------------------------------
# Gemini
# -------------------------------------------------------------------------
# Usa a API do Google Gemini.
#
# Exemplo:
#
# Sys.setenv(
#   LLM_PROVIDER = "gemini",
#   LLM_MODEL = "gemini-1.5-flash",
#   LLM_API_KEY = "SUA_CHAVE"
# )
#
# Observação:
# - `response_mime_type = "application/json"` solicita retorno em JSON.
# - A chave deve vir de variável de ambiente, nunca hardcoded no script.

llm_generate_gemini <- function(prompt, config = make_llm_config()) {
  api_key <- config$api_key %||% ""
  
  if (identical(api_key, "")) {
    stop("LLM_API_KEY não configurada para provider = 'gemini'.")
  }
  
  model <- config$model %||% ""
  
  if (identical(model, "") || identical(model, "mock-sindromico-v1")) {
    model <- "gemini-1.5-flash"
  }
  
  temperature <- config$temperature %||% 0
  timeout_sec <- config$timeout_sec %||% 120
  max_tokens <- config$max_tokens %||% 800
  
  endpoint <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model,
    ":generateContent?key=",
    api_key
  )
  
  req <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_timeout(seconds = timeout_sec) |>
    httr2::req_body_json(list(
      contents = list(
        list(
          role = "user",
          parts = list(
            list(text = prompt)
          )
        )
      ),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_tokens,
        response_mime_type = "application/json"
      )
    ))
  
  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp)
  
  body$candidates[[1]]$content$parts[[1]]$text %||% ""
}

# -------------------------------------------------------------------------
# OpenAI-compatible
# -------------------------------------------------------------------------
# Funciona para APIs com interface compatível com:
# POST /chat/completions
#
# Pode ser usado para:
# - OpenAI
# - LiteLLM
# - LM Studio em modo OpenAI-compatible
# - vLLM
# - outros gateways compatíveis
#
# Exemplo:
#
# Sys.setenv(
#   LLM_PROVIDER = "openai_compatible",
#   LLM_MODEL = "gpt-4o-mini",
#   LLM_BASE_URL = "https://api.openai.com/v1",
#   LLM_API_KEY = "SUA_CHAVE"
# )

llm_generate_openai_compatible <- function(prompt, config = make_llm_config()) {
  api_key <- config$api_key %||% ""
  
  if (identical(api_key, "")) {
    stop("LLM_API_KEY não configurada para provider = 'openai_compatible'.")
  }
  
  base_url <- normalizar_base_url(config$base_url)
  
  if (identical(base_url, "")) {
    base_url <- "https://api.openai.com/v1"
  }
  
  model <- config$model %||% "gpt-4o-mini"
  temperature <- config$temperature %||% 0
  timeout_sec <- config$timeout_sec %||% 120
  max_tokens <- config$max_tokens %||% 800
  
  body <- list(
    model = model,
    temperature = temperature,
    max_tokens = max_tokens,
    messages = list(
      list(
        role = "system",
        content = paste(
          "Você é um classificador sindrômico.",
          "Responda exclusivamente em JSON válido.",
          "Não inclua markdown, comentários ou texto fora do JSON."
        )
      ),
      list(
        role = "user",
        content = prompt
      )
    )
  )
  
  # Algumas APIs OpenAI-compatible aceitam response_format.
  # Outras não aceitam. Por isso deixamos ativado apenas quando json_mode = TRUE.
  if (isTRUE(config$json_mode)) {
    body$response_format <- list(type = "json_object")
  }
  
  req <- httr2::request(paste0(base_url, "/chat/completions")) |>
    httr2::req_method("POST") |>
    httr2::req_timeout(seconds = timeout_sec) |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(body)
  
  resp <- httr2::req_perform(req)
  body_resp <- httr2::resp_body_json(resp)
  
  body_resp$choices[[1]]$message$content %||% ""
}

# -------------------------------------------------------------------------
# Extração e parsing seguro de JSON
# -------------------------------------------------------------------------
# Algumas LLMs podem responder com texto antes/depois do JSON.
# Estas funções extraem o primeiro objeto JSON aparente e tentam fazer parse.
#
# A validação de schema final fica no R/08_llm_classification.R.

extract_json_object <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  
  if (length(x) == 1 && is.na(x)) {
    return(NA_character_)
  }
  
  x <- as.character(x)
  
  # Remove blocos markdown frequentes, se o modelo retornar ```json ... ```.
  x <- stringr::str_replace_all(x, "```json", "")
  x <- stringr::str_replace_all(x, "```", "")
  
  # Remove tags de raciocínio que alguns modelos locais podem emitir.
  x <- stringr::str_replace_all(x, "<think>.*?</think>", "")
  
  inicio <- regexpr("\\{", x)[[1]]
  fim_matches <- gregexpr("\\}", x)[[1]]
  
  if (inicio < 0 || all(fim_matches < 0)) {
    return(NA_character_)
  }
  
  fim <- max(fim_matches)
  
  substr(x, inicio, fim)
}

safe_parse_llm_json <- function(x) {
  json_txt <- extract_json_object(x)
  
  if (is.na(json_txt)) {
    return(list(
      parse_ok = FALSE,
      data = NULL,
      error = "Nenhum objeto JSON encontrado."
    ))
  }
  
  parsed <- tryCatch(
    jsonlite::fromJSON(json_txt, simplifyVector = FALSE),
    error = function(e) e
  )
  
  if (inherits(parsed, "error")) {
    return(list(
      parse_ok = FALSE,
      data = NULL,
      error = parsed$message
    ))
  }
  
  list(
    parse_ok = TRUE,
    data = parsed,
    error = NA_character_
  )
}
