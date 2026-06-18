# scripts/teste_llm_qwen_metricas.R -----------------------------------------
# Teste local da camada LLM com Ollama/Qwen e registro de métricas.
#
# Objetivo:
# - Executar a camada LLM real em uma amostra controlada.
# - Medir tempo total e tempo médio por registro.
# - Registrar parse_ok e distribuição das síndromes.
# - Registrar informações básicas da GPU NVIDIA, quando disponível.
#
# Ambiente esperado:
# - Ollama ativo em http://localhost:11434
# - Modelo Qwen instalado no Ollama
# - GPU NVIDIA disponível, se o ambiente tiver nvidia-smi
#
# Observação:
# Este script é uma validação técnica local.
# A entrega principal do Quarto Book pode continuar em modo mock.

# -------------------------------------------------------------------------
# 1. Configuração do teste
# -------------------------------------------------------------------------

Sys.setenv(
  LLM_PROVIDER = "ollama",
  LLM_MODEL = "qwen3.5:latest",
  LLM_BASE_URL = "http://localhost:11434",
  LLM_JSON_MODE = "false",
  LLM_TIMEOUT_SEC = "180",
  LLM_MAX_RECORDS = "30"
)

N_AMOSTRA <- as.integer(Sys.getenv("LLM_MAX_RECORDS", unset = "30"))

DIR_OUTPUTS <- "outputs"
if (!dir.exists(DIR_OUTPUTS)) {
  dir.create(DIR_OUTPUTS, recursive = TRUE)
}

arquivo_resultados <- file.path(
  DIR_OUTPUTS,
  paste0("teste_llm_local_qwen_", N_AMOSTRA, "_registros.csv")
)

arquivo_metricas <- file.path(
  DIR_OUTPUTS,
  paste0("teste_llm_local_qwen_", N_AMOSTRA, "_metricas.csv")
)

arquivo_distribuicao <- file.path(
  DIR_OUTPUTS,
  paste0("teste_llm_local_qwen_", N_AMOSTRA, "_distribuicao_sindromes.csv")
)

# -------------------------------------------------------------------------
# 2. Pacotes e funções do projeto
# -------------------------------------------------------------------------

source("R/00_pacotes.R")
source("R/04_duckdb.R")
source("R/07_llm_clients.R")
source("R/08_llm_classification.R")

# -------------------------------------------------------------------------
# 3. Funções auxiliares para GPU
# -------------------------------------------------------------------------

tem_nvidia_smi <- function() {
  nzchar(Sys.which("nvidia-smi"))
}

ler_gpu_nvidia <- function() {
  if (!tem_nvidia_smi()) {
    return(tibble::tibble(
      gpu_disponivel = FALSE,
      gpu_name = NA_character_,
      memory_total_mb = NA_real_,
      memory_used_mb = NA_real_,
      utilization_gpu_percent = NA_real_,
      temperature_gpu_c = NA_real_
    ))
  }
  
  cmd <- paste(
    "nvidia-smi",
    "--query-gpu=name,memory.total,memory.used,utilization.gpu,temperature.gpu",
    "--format=csv,noheader,nounits"
  )
  
  out <- tryCatch(
    system(cmd, intern = TRUE),
    error = function(e) NA_character_
  )
  
  if (length(out) == 0 || is.na(out[1])) {
    return(tibble::tibble(
      gpu_disponivel = TRUE,
      gpu_name = NA_character_,
      memory_total_mb = NA_real_,
      memory_used_mb = NA_real_,
      utilization_gpu_percent = NA_real_,
      temperature_gpu_c = NA_real_
    ))
  }
  
  partes <- strsplit(out[1], ",")[[1]]
  partes <- trimws(partes)
  
  tibble::tibble(
    gpu_disponivel = TRUE,
    gpu_name = partes[1],
    memory_total_mb = as.numeric(partes[2]),
    memory_used_mb = as.numeric(partes[3]),
    utilization_gpu_percent = as.numeric(partes[4]),
    temperature_gpu_c = as.numeric(partes[5])
  )
}

# -------------------------------------------------------------------------
# 4. Leitura dos dados
# -------------------------------------------------------------------------

con <- connect_duckdb()

atendimentos <- read_atendimentos(con)

set.seed(123)

amostra <- atendimentos |>
  dplyr::slice_sample(n = min(N_AMOSTRA, nrow(atendimentos)))

llm_config <- make_llm_config()

cat("\nConfiguração LLM ativa:\n")
print(llm_config)

cat("\nGPU antes da execução:\n")
gpu_antes <- ler_gpu_nvidia()
print(gpu_antes)

# -------------------------------------------------------------------------
# 5. Execução com medição de tempo
# -------------------------------------------------------------------------

inicio <- Sys.time()

teste_qwen <- classify_by_llm(
  dados = amostra,
  config = llm_config,
  max_records = N_AMOSTRA
)

fim <- Sys.time()

tempo_total_seg <- as.numeric(
  difftime(fim, inicio, units = "secs")
)

tempo_medio_seg <- tempo_total_seg / nrow(teste_qwen)

cat("\nGPU depois da execução:\n")
gpu_depois <- ler_gpu_nvidia()
print(gpu_depois)

# -------------------------------------------------------------------------
# 6. Métricas do teste
# -------------------------------------------------------------------------

n_registros <- nrow(teste_qwen)

n_parse_ok <- sum(teste_qwen$parse_ok %in% TRUE, na.rm = TRUE)
n_parse_falha <- sum(!(teste_qwen$parse_ok %in% TRUE), na.rm = TRUE)

prop_parse_ok <- ifelse(
  n_registros > 0,
  n_parse_ok / n_registros,
  NA_real_
)

distribuicao_sindromes <- teste_qwen |>
  dplyr::count(sindrome_principal_llm, sort = TRUE) |>
  dplyr::mutate(
    percentual = round(100 * n / sum(n), 1)
  )

metricas <- tibble::tibble(
  data_execucao = as.character(Sys.time()),
  provider = llm_config$provider,
  modelo = llm_config$model,
  base_url = llm_config$base_url,
  json_mode = llm_config$json_mode,
  timeout_sec = llm_config$timeout_sec,
  n_registros = n_registros,
  n_parse_ok = n_parse_ok,
  n_parse_falha = n_parse_falha,
  prop_parse_ok = prop_parse_ok,
  tempo_total_seg = round(tempo_total_seg, 2),
  tempo_medio_por_registro_seg = round(tempo_medio_seg, 2),
  gpu_name = gpu_antes$gpu_name[1],
  gpu_memory_total_mb = gpu_antes$memory_total_mb[1],
  gpu_memory_used_mb_antes = gpu_antes$memory_used_mb[1],
  gpu_memory_used_mb_depois = gpu_depois$memory_used_mb[1],
  gpu_utilization_percent_antes = gpu_antes$utilization_gpu_percent[1],
  gpu_utilization_percent_depois = gpu_depois$utilization_gpu_percent[1],
  gpu_temperature_c_antes = gpu_antes$temperature_gpu_c[1],
  gpu_temperature_c_depois = gpu_depois$temperature_gpu_c[1]
)

# -------------------------------------------------------------------------
# 7. Salvamento dos resultados
# -------------------------------------------------------------------------

readr::write_csv(
  teste_qwen,
  arquivo_resultados
)

readr::write_csv(
  metricas,
  arquivo_metricas
)

readr::write_csv(
  distribuicao_sindromes,
  arquivo_distribuicao
)

# -------------------------------------------------------------------------
# 8. Impressão no console
# -------------------------------------------------------------------------

cat("\nResumo de parse_ok:\n")
print(
  teste_qwen |>
    dplyr::count(parse_ok)
)

cat("\nDistribuição das síndromes:\n")
print(distribuicao_sindromes)

cat("\nMétricas principais:\n")
print(metricas, width = Inf)

cat("\nAmostra de resultados:\n")
teste_qwen |>
  dplyr::select(
    record_id,
    sindrome_principal_llm,
    febre_presente,
    febre_negada,
    sintomas_identificados,
    justificativa_llm,
    parse_ok
  ) |>
  print(n = n_registros, width = Inf)

cat("\nArquivos gerados:\n")
cat("\n- ", arquivo_resultados)
cat("\n- ", arquivo_metricas)
cat("\n- ", arquivo_distribuicao, "\n")

# -------------------------------------------------------------------------
# 9. Encerramento
# -------------------------------------------------------------------------

DBI::dbDisconnect(con, shutdown = TRUE)