# run_pipeline.R -------------------------------------------------------------
# Script único para executar/renderizar o Quarto Book.
#
# Uso:
# source("run_pipeline.R")

message("Carregando pacotes e funções principais...")
source("R/00_pacotes.R")
source("R/04_duckdb.R")

message("Inicializando DuckDB...")
con <- connect_duckdb()
init_duckdb_schema(con)
DBI::dbDisconnect(con, shutdown = TRUE)

message("Renderizando Quarto Book...")
quarto::quarto_render()

message("Renderização concluída.")
