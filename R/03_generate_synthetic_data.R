# ============================================================================
# R/03_generate_synthetic_data.R
#
# Geração de base sintética para desenvolvimento e demonstração do pipeline.
#
# Esta base simula atendimentos febris com:
# - 700 registros distribuídos ao longo de 12 meses;
# - múltiplas unidades, territórios, sexo e faixas etárias;
# - textos clínicos heterogêneos, com ruídos, caixa alta, erros e negações;
# - CIDs coerentes, ausentes e propositalmente conflitantes;
# - síndrome esperada sintética para avaliação futura do pipeline.
# ============================================================================

generate_synthetic_attendances <- function(
    n = 700,
    seed = 123,
    start_date = as.Date("2025-01-01"),
    end_date = as.Date("2025-12-31")
) {

  set.seed(seed)

  unidades <- c(
    "UPA Centro",
    "UPA Norte",
    "UPA Sul",
    "Hospital Municipal A",
    "Hospital Municipal B",
    "CER Centro",
    "CER Norte"
  )

  territorios <- c("AP 1.0", "AP 2.1", "AP 3.1", "AP 3.2", "AP 4.0", "AP 5.1")

  bairros <- c(
    "Centro", "Tijuca", "Madureira", "Campo Grande", "Bangu",
    "Santa Cruz", "Botafogo", "Jacarepaguá", "Penha", "Méier"
  )

  sindromes <- c(
    "febril_respiratoria",
    "febril_inespecifica",
    "febril_gastrointestinal",
    "febril_exantematica",
    "febril_neurologica_meningea",
    "febril_hemorragica",
    "febril_ictero_hemorragica"
  )

  probs_base <- c(
    febril_respiratoria = 0.30,
    febril_inespecifica = 0.20,
    febril_gastrointestinal = 0.15,
    febril_exantematica = 0.15,
    febril_neurologica_meningea = 0.08,
    febril_hemorragica = 0.07,
    febril_ictero_hemorragica = 0.05
  )

  datas <- sample(seq.Date(start_date, end_date, by = "day"), size = n, replace = TRUE) |>
    sort()

  choose_syndrome_by_month <- function(date) {
    m <- lubridate::month(date)

    p <- probs_base

    # Sazonalidade simulada:
    # respiratória aumenta no outono/inverno;
    # gastrointestinal tem discretos picos no verão;
    # exantemática tem pequenos surtos em março/abril e setembro.
    if (m %in% 5:8) {
      p["febril_respiratoria"] <- p["febril_respiratoria"] + 0.18
      p["febril_inespecifica"] <- p["febril_inespecifica"] - 0.05
      p["febril_exantematica"] <- p["febril_exantematica"] - 0.04
    }

    if (m %in% c(1, 2, 12)) {
      p["febril_gastrointestinal"] <- p["febril_gastrointestinal"] + 0.08
      p["febril_respiratoria"] <- p["febril_respiratoria"] - 0.04
    }

    if (m %in% c(3, 4, 9)) {
      p["febril_exantematica"] <- p["febril_exantematica"] + 0.08
      p["febril_inespecifica"] <- p["febril_inespecifica"] - 0.03
    }

    p <- p / sum(p)

    sample(names(p), size = 1, prob = p)
  }

  sindrome <- purrr::map_chr(datas, choose_syndrome_by_month)

  idade <- sample(
    0:92,
    size = n,
    replace = TRUE,
    prob = dplyr::case_when(
      0:92 < 5 ~ 1.3,
      0:92 < 15 ~ 1.1,
      0:92 < 40 ~ 1.7,
      0:92 < 60 ~ 1.3,
      TRUE ~ 0.9
    )
  )

  sexo <- sample(c("F", "M"), size = n, replace = TRUE, prob = c(0.54, 0.46))

  unidade <- sample(
    unidades,
    size = n,
    replace = TRUE,
    prob = c(0.18, 0.16, 0.16, 0.15, 0.13, 0.12, 0.10)
  )

  territorio <- sample(territorios, size = n, replace = TRUE)
  bairro <- sample(bairros, size = n, replace = TRUE)

  cid_por_sindrome <- list(
    febril_respiratoria = c("J00", "J02", "J06", "J10", "J11", "J18", "U07"),
    febril_inespecifica = c("R50", "B34"),
    febril_gastrointestinal = c("A05", "A08", "A09"),
    febril_exantematica = c("B05", "B06", "A38"),
    febril_neurologica_meningea = c("A39", "G00", "G03"),
    febril_hemorragica = c("A90", "A91", "A99"),
    febril_ictero_hemorragica = c("A27", "A95", "B15", "B17")
  )

  cid_nome <- c(
    J00 = "Nasofaringite aguda",
    J02 = "Faringite aguda",
    J06 = "Infecções agudas das vias aéreas superiores",
    J10 = "Influenza devida a outro vírus identificado",
    J11 = "Influenza devida a vírus não identificado",
    J18 = "Pneumonia por microorganismo não especificado",
    U07 = "COVID-19",
    R50 = "Febre de origem desconhecida",
    B34 = "Infecção viral de localização não especificada",
    A05 = "Outras intoxicações alimentares bacterianas",
    A08 = "Infecções intestinais virais",
    A09 = "Diarreia e gastroenterite de origem infecciosa presumível",
    B05 = "Sarampo",
    B06 = "Rubéola",
    A38 = "Escarlatina",
    A39 = "Infecção meningocócica",
    G00 = "Meningite bacteriana",
    G03 = "Meningite devida a outras causas e a causas não especificadas",
    A90 = "Dengue",
    A91 = "Febre hemorrágica devida ao vírus da dengue",
    A99 = "Febres hemorrágicas virais não especificadas",
    A27 = "Leptospirose",
    A95 = "Febre amarela",
    B15 = "Hepatite aguda A",
    B17 = "Outras hepatites virais agudas"
  )

  make_text <- function(sindrome, idade) {

    templates <- list(

      febril_respiratoria = c(
        "Paciente refere febre há cerca de três dias, tosse seca persistente, coriza e dor de garganta. Nega diarreia, nega exantema e nega sangramentos. Relata contato com familiar gripado na última semana.",
        "FEBRE alta desde ontem, tosse produtiva, congestão nasal e mal estar. Sem manchas na pele. Sem vômitos. Refere piora da tosse durante a noite.",
        "Pcte com febre não aferida, odinofagia, coriza e tosse. Relata cansaço e dor no corpo. Nega sintomas gastrointestinais.",
        "Criança com febre, tosse persistente e nariz escorrendo. Mãe nega manchas vermelhas e nega episódios de diarreia."
      ),

      febril_inespecifica = c(
        "Refere febre não aferida, dor no corpo importante, cefaleia e mal estar geral. Nega tosse, nega diarreia e nega sangramento.",
        "PAC com febre há dois dias, mialgia, calafrios e prostração. Sem queixas respiratórias ou gastrointestinais no momento.",
        "Febre, cefalea, dor no corpo e indisposição. Nega manchas na pele. Nega falta de ar.",
        "Paciente relata febre baixa, artralgia e cansaço. Nega sangramentos, nega rash e nega vômitos."
      ),

      febril_gastrointestinal = c(
        "Paciente com febre, náuseas, vômitos e diarreia desde ontem. Refere dor abdominal em cólica e baixa aceitação alimentar.",
        "Criança com febre alta, vômitos repetidos e diarreia líquida. Mãe refere pouca aceitação alimentar e sonolência leve.",
        "FEBRE + dor abdominal + diarreiaa desde madrugada. Nega tosse, nega manchas na pele e nega sangramentos.",
        "Relata febre, nausea, episódios de vômito e evacuações líquidas. Sem sintomas respiratórios."
      ),

      febril_exantematica = c(
        "Paciente refere febre há três dias, cefaleia e manchas vermelhas pelo corpo. Nega falta de ar, nega sangramento e nega diarreia.",
        "Mãe relata criança com febre alta desde ontem, exantema em tronco e membros superiores. Sem sintomas respiratórios.",
        "Febre e rash cutâneo difuso, prurido importante e dor no corpo. Nega sangramento gengival.",
        "FEBRE, manchas avermelhadas no corpo e dor no corpo. Sem diarreia. Sem tosse importante."
      ),

      febril_neurologica_meningea = c(
        "Paciente com febre, cefaleia intensa, rigidez de nuca e fotofobia. Refere vômitos e piora progressiva desde a manhã.",
        "Febre alta, confusão mental e vômitos em jato. Familiar refere piora nas últimas horas.",
        "Criança com febre e convulsão em casa. Sonolenta na chegada, com irritabilidade importante.",
        "Relata febre, dor de cabeça muito forte e rigidez cervical. Nega manchas na pele."
      ),

      febril_hemorragica = c(
        "Paciente relata febre, epistaxe e pequenas manchas arroxeadas em membros. Refere dor no corpo e prostração.",
        "Febre há quatro dias com gengivorragia e petéquias. Nega icterícia e nega colúria.",
        "FEBRE, sangramento nasal e equimoses em membros inferiores. Sem tosse. Sem diarreia.",
        "Paciente com febre, dor no corpo e relato de sangue na gengiva ao escovar os dentes."
      ),

      febril_ictero_hemorragica = c(
        "Paciente com febre, olhos amarelados e colúria importante. Refere mialgia intensa e mal estar geral.",
        "Febre há cinco dias, pele amarela, urina escura e dor no corpo. Nega tosse e nega diarreia.",
        "FEBRE, icterícia e sangramento gengival discreto. Refere dor em panturrilhas.",
        "Relata febre, mal estar, coluria e escleras amareladas. Sem sintomas respiratórios relevantes."
      )
    )

    txt <- sample(templates[[sindrome]], 1)

    # Ruídos realistas.
    if (runif(1) < 0.18) {
      txt <- stringr::str_replace_all(txt, regex("febre", ignore_case = TRUE), sample(c("fbre", "febree", "FEBRE"), 1))
    }

    if (runif(1) < 0.12) {
      txt <- stringr::str_replace_all(txt, regex("Paciente", ignore_case = TRUE), sample(c("Pcte", "PAC", "paciente"), 1))
    }

    if (runif(1) < 0.10) {
      txt <- paste0(txt, " !!!")
    }

    if (runif(1) < 0.08) {
      txt <- stringr::str_to_upper(txt)
    }

    txt
  }

  cid_status <- character(length(sindrome))
  cid <- character(length(sindrome))

  for (i in seq_along(sindrome)) {
    s <- sindrome[[i]]

    r <- runif(1)

    if (r < 0.18) {
      cid[[i]] <- NA_character_
      cid_status[[i]] <- "ausente"
    } else if (r < 0.26) {
      outra <- sample(setdiff(sindromes, s), 1)
      cid[[i]] <- sample(cid_por_sindrome[[outra]], 1)
      cid_status[[i]] <- "conflitante"
    } else {
      cid[[i]] <- sample(cid_por_sindrome[[s]], 1)
      cid_status[[i]] <- "coerente"
    }
  }

  anamnese <- purrr::map2_chr(sindrome, idade, make_text)

  queixa <- dplyr::case_when(
    sindrome == "febril_respiratoria" ~ "Febre e tosse",
    sindrome == "febril_inespecifica" ~ "Febre e dor no corpo",
    sindrome == "febril_gastrointestinal" ~ "Febre e diarreia",
    sindrome == "febril_exantematica" ~ "Febre e manchas",
    sindrome == "febril_neurologica_meningea" ~ "Febre e cefaleia",
    sindrome == "febril_hemorragica" ~ "Febre e sangramento",
    sindrome == "febril_ictero_hemorragica" ~ "Febre e icterícia",
    TRUE ~ "Febre"
  )

  negados <- sample(seq_len(n), size = round(n * 0.05))

  neg_templates <- c(
    "Paciente afebril no momento. Nega febre.",
    "Sem febre referida no atendimento. Nega episódios febris.",
    "Não apresenta febre segundo relato inicial.",
    "Afebril, sem relato de febre nas últimas 48h."
  )

  anamnese[negados] <- paste(
    sample(neg_templates, length(negados), replace = TRUE),
    anamnese[negados]
  )

  tibble::tibble(
    record_id = sprintf("SYN%04d", seq_len(n)),
    data_atendimento = datas,
    unidade = unidade,
    territorio = territorio,
    bairro = bairro,
    idade = idade,
    faixa_etaria = create_age_group(idade),
    sexo = sexo,
    cid = cid,
    cid_nome = unname(cid_nome[cid]),
    queixa = queixa,
    anamnese = anamnese,
    texto_clinico = paste(queixa, anamnese),
    sindrome_esperada_sintetica = sindrome,
    cid_status_sintetico = cid_status,
    febre_negada_sintetica = record_id %in% sprintf("SYN%04d", negados)
  )
}

save_synthetic_attendances <- function(path = "data/atendimentos_sinteticos.csv", n = 700, seed = 123) {

  dados <- generate_synthetic_attendances(n = n, seed = seed)

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(
    dados,
    path
  )

  invisible(dados)
}
