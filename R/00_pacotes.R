# R/00_pacotes.R -------------------------------------------------------------
# Pacotes principais do Quarto Book de vigilância sindrômica.
#
# Este arquivo centraliza o carregamento de dependências usadas nos capítulos
# e scripts do pipeline. A instalação dos pacotes deve ser feita separadamente,
# de preferência em ambiente controlado com renv.

pacotes_necessarios <- c(
  "dplyr",
  "tibble",
  "stringr",
  "stringi",
  "purrr",
  "readr",
  "lubridate",
  "ggplot2",
  "rlang",
  "tidyr",
  "DBI",
  "duckdb",
  "knitr",
  "jsonlite",
  "httr2",
  "glue",
  "forcats"
)

pacotes_ausentes <- pacotes_necessarios[
  !vapply(pacotes_necessarios, requireNamespace, logical(1), quietly = TRUE)
]

if (length(pacotes_ausentes) > 0) {
  stop(
    "Pacotes ausentes: ",
    paste(pacotes_ausentes, collapse = ", "),
    "\nInstale-os antes de renderizar o Quarto Book."
  )
}

invisible(lapply(pacotes_necessarios, library, character.only = TRUE))
