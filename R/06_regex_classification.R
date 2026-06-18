# ============================================================================
# R/06_regex_classification.R
#
# Classificação sindrômica por regras textuais/regex.
#
# Princípios:
# - A regex é uma camada complementar ao CID.
# - Só classifica síndrome febril textual quando há evidência de febre
#   não negada.
# - Detecta sintomas formais, informais, abreviações, grafias sem acento e
#   erros comuns de escrita.
# - Produz uma única síndrome principal por atendimento.
# ============================================================================

regex_or <- function(patterns) {
  paste0("(", paste(patterns, collapse = "|"), ")")
}

get_regex_dictionary <- function() {

  list(

    febre = regex_or(c(
      "\\bfebre\\b",
      "\\bfebril\\b",
      "\\bfebricula\\b",
      "\\bfebricola\\b",
      "\\bfebrilidade\\b",
      "\\btemperatura alta\\b",
      "\\btemp alta\\b",
      "\\bhipertermia\\b",
      "\\bcalafrio[s]?\\b",
      "\\bfbre\\b",
      "\\bfebree\\b",
      "\\bfebr[e3]\\b",
      "\\bfvr\\b",
      "\\bfeb\\b"
    )),

    negacao_febre = regex_or(c(
      "\\bnega febre\\b",
      "\\bnega.*\\bfebre\\b",
      "\\bsem febre\\b",
      "\\bsem.*\\bfebre\\b",
      "\\bafebril\\b",
      "\\bnao apresenta febre\\b",
      "\\bn[aã]o apresenta febre\\b",
      "\\bnao refere febre\\b",
      "\\bn[aã]o refere febre\\b",
      "\\bsem relato de febre\\b",
      "\\bnega episodios febris\\b",
      "\\bnega epis[oó]dios febris\\b",
      "\\bsem episodios febris\\b",
      "\\bsem epis[oó]dios febris\\b"
    )),

    respiratorio = regex_or(c(
      "\\btosse\\b", "\\btose\\b", "\\btosse seca\\b", "\\btosse produtiva\\b",
      "\\bcoriza\\b", "\\bcorisa\\b", "\\bnariz escorrendo\\b",
      "\\brinorreia\\b", "\\bcongestao nasal\\b", "\\bcongest[aã]o nasal\\b",
      "\\bentupimento nasal\\b", "\\bdor de garganta\\b", "\\bgarganta inflamada\\b",
      "\\bodinofagia\\b", "\\bdispneia\\b", "\\bfalta de ar\\b",
      "\\bcansaco respiratorio\\b", "\\bcansa[cç]o respiratorio\\b",
      "\\bchiado\\b", "\\bsibilancia\\b", "\\bsibil[aâ]ncia\\b",
      "\\bgripe\\b", "\\bresfriado\\b", "\\bsindrome gripal\\b", "\\bs[ií]ndrome gripal\\b"
    )),

    exantematico = regex_or(c(
      "\\bexantema\\b", "\\bexantematico\\b", "\\bexantem[aá]tico\\b",
      "\\brash\\b", "\\brash cutaneo\\b", "\\brash cut[aâ]neo\\b",
      "\\bmanchas?\\b", "\\bmanchas? vermelhas?\\b", "\\bmanchinhas\\b",
      "\\bmancha no corpo\\b", "\\bmanchas pelo corpo\\b", "\\berupcao\\b",
      "\\berup[cç][aã]o\\b", "\\berup[cç][oõ]es\\b", "\\bvermelhidao\\b",
      "\\bvermelhid[aã]o\\b", "\\bpele vermelha\\b", "\\bbolinhas vermelhas\\b",
      "\\bprurido\\b", "\\bcoceira\\b"
    )),

    gastrointestinal = regex_or(c(
      "\\bdiarreia\\b", "\\bdiarr[eé]ia\\b", "\\bdiarreiaa\\b", "\\bdiarr\\b",
      "\\bevacuacoes liquidas\\b", "\\bevacua[cç][oõ]es l[ií]quidas\\b",
      "\\bfezes liquidas\\b", "\\bfezes l[ií]quidas\\b",
      "\\bvomito\\b", "\\bv[oô]mito\\b", "\\bvomitos\\b", "\\bv[oô]mitos\\b",
      "\\bemese\\b", "\\bnausea\\b", "\\bn[aá]usea\\b", "\\benjoo\\b", "\\benj[oô]o\\b",
      "\\bdor abdominal\\b", "\\babdominalgia\\b", "\\bdor na barriga\\b", "\\bdor de barriga\\b",
      "\\bcolica abdominal\\b", "\\bc[oó]lica abdominal\\b"
    )),

    hemorragico = regex_or(c(
      "\\bsangramento\\b", "\\bsangra\\b", "\\bsangrando\\b", "\\bsangue\\b",
      "\\bhemorragia\\b", "\\bhemorragico\\b", "\\bhemorr[aá]gico\\b",
      "\\bepistaxe\\b", "\\bsangramento nasal\\b", "\\bsangue no nariz\\b",
      "\\bgengivorragia\\b", "\\bsangramento gengival\\b", "\\bsangue na gengiva\\b",
      "\\bpetequia\\b", "\\bpetequias\\b", "\\bpet[eé]quias\\b",
      "\\bequimose\\b", "\\bequimoses\\b", "\\bmanchas roxas\\b", "\\bhematomas\\b",
      "\\bmelena\\b", "\\bhematemese\\b", "\\bhematuria\\b", "\\bhemat[uú]ria\\b"
    )),

    icterico = regex_or(c(
      "\\bictericia\\b", "\\bict[eé]ricia\\b", "\\bicterico\\b", "\\bict[eé]rico\\b",
      "\\bamarelao\\b", "\\bamarel[aã]o\\b", "\\bpele amarela\\b",
      "\\bolhos amarelos\\b", "\\bolho amarelo\\b", "\\bescleras amareladas\\b",
      "\\besclera amarela\\b", "\\bcoluria\\b", "\\bcol[uú]ria\\b",
      "\\burina escura\\b", "\\burina cor de coca\\b", "\\burina cor coca\\b"
    )),

    neurologico_meningeo = regex_or(c(
      "\\brigidez de nuca\\b", "\\brigidez nuca\\b", "\\bnuca rigida\\b", "\\bnuca r[ií]gida\\b",
      "\\bmeningismo\\b", "\\bmeningite\\b", "\\bcefaleia intensa\\b", "\\bcefaleia forte\\b",
      "\\bcefalea intensa\\b", "\\bdor de cabeca intensa\\b", "\\bdor de cabe[cç]a intensa\\b",
      "\\bdor de cabeca muito forte\\b", "\\bfotofobia\\b", "\\bconvulsao\\b", "\\bconvuls[aã]o\\b",
      "\\bconvulsivo\\b", "\\bcrise convulsiva\\b", "\\bconfusao mental\\b", "\\bconfus[aã]o mental\\b",
      "\\brebaixamento\\b", "\\bsonolencia\\b", "\\bsonol[eê]ncia\\b", "\\bletargia\\b",
      "\\bvomitos em jato\\b", "\\bv[oô]mitos em jato\\b"
    )),

    inespecifico = regex_or(c(
      "\\bmialgia\\b", "\\bdor no corpo\\b", "\\bdor no corpo todo\\b", "\\bdor muscular\\b",
      "\\bartralgia\\b", "\\bdor nas juntas\\b", "\\bcefaleia\\b", "\\bcefalea\\b",
      "\\bdor de cabeca\\b", "\\bdor de cabe[cç]a\\b", "\\bmal estar\\b", "\\bmal-estar\\b",
      "\\bindisposicao\\b", "\\bindisposi[cç][aã]o\\b", "\\bprostracao\\b", "\\bprostra[cç][aã]o\\b",
      "\\bcansaco\\b", "\\bcansa[cç]o\\b", "\\bcalafrios?\\b"
    ))
  )

}

get_regex_dictionary_display <- function() {

  tibble::tribble(
    ~grupo, ~descricao, ~exemplos_de_termos,
    "febre", "Menção afirmativa de febre ou estado febril.", "febre; febril; febrícula; temperatura alta; temp alta; hipertermia; calafrios; fbre; febree; feb",
    "negacao_febre", "Expressões que negam febre e impedem a classificação textual como síndrome febril.", "nega febre; sem febre; afebril; não apresenta febre; não refere febre; sem relato de febre; nega episódios febris",
    "respiratorio", "Sintomas respiratórios altos ou baixos.", "tosse; tose; tosse seca; tosse produtiva; coriza; nariz escorrendo; rinorreia; congestão nasal; dor de garganta; odinofagia; falta de ar; dispneia; chiado; síndrome gripal",
    "exantematico", "Alterações cutâneas compatíveis com exantema ou rash.", "exantema; rash; manchas; manchas vermelhas; manchinhas; erupção; vermelhidão; pele vermelha; bolinhas vermelhas; prurido; coceira",
    "gastrointestinal", "Sintomas gastrointestinais.", "diarreia; diarréia; diarreiaa; evacuações líquidas; fezes líquidas; vômito; vomitos; êmese; náusea; nausea; enjoo; dor abdominal; dor de barriga; cólica abdominal",
    "hemorragico", "Sangramentos e sinais cutâneos hemorrágicos.", "sangramento; sangue; hemorragia; epistaxe; sangramento nasal; gengivorragia; sangramento gengival; petéquias; equimoses; manchas roxas; hematomas; melena; hematemese; hematúria",
    "icterico", "Icterícia, colúria ou termos populares associados.", "icterícia; ictérico; amarelão; pele amarela; olhos amarelos; escleras amareladas; colúria; urina escura; urina cor de coca",
    "neurologico_meningeo", "Sinais neurológicos ou meníngeos.", "rigidez de nuca; nuca rígida; meningismo; meningite; cefaleia intensa; dor de cabeça muito forte; fotofobia; convulsão; crise convulsiva; confusão mental; sonolência; vômitos em jato",
    "inespecifico", "Sintomas gerais que apoiam síndrome febril inespecífica.", "mialgia; dor no corpo; dor muscular; artralgia; dor nas juntas; cefaleia; dor de cabeça; mal estar; indisposição; prostração; cansaço; calafrios"
  )

}

detect_regex_features <- function(df, text_col = "texto_clinico_norm") {

  dic <- get_regex_dictionary()
  txt <- df[[text_col]]

  df |>
    dplyr::mutate(
      flag_febre = stringr::str_detect(txt, dic$febre),
      flag_febre_negada = stringr::str_detect(txt, dic$negacao_febre),
      febre_valida_regex = flag_febre & !flag_febre_negada,
      flag_respiratorio = stringr::str_detect(txt, dic$respiratorio),
      flag_exantematico = stringr::str_detect(txt, dic$exantematico),
      flag_gastrointestinal = stringr::str_detect(txt, dic$gastrointestinal),
      flag_hemorragico = stringr::str_detect(txt, dic$hemorragico),
      flag_icterico = stringr::str_detect(txt, dic$icterico),
      flag_neurologico_meningeo = stringr::str_detect(txt, dic$neurologico_meningeo),
      flag_inespecifico = stringr::str_detect(txt, dic$inespecifico)
    )

}

classify_by_regex <- function(df, text_col = "texto_clinico_norm") {

  detect_regex_features(df, text_col = text_col) |>
    dplyr::mutate(
      regra_ictero_hemorragica = febre_valida_regex & flag_icterico,
      regra_hemorragica = febre_valida_regex & flag_hemorragico,
      regra_neurologica_meningea = febre_valida_regex & flag_neurologico_meningeo,
      regra_exantematica = febre_valida_regex & flag_exantematico,
      regra_gastrointestinal = febre_valida_regex & flag_gastrointestinal,
      regra_respiratoria = febre_valida_regex & flag_respiratorio,
      regra_inespecifica = febre_valida_regex & flag_inespecifico,
      sindrome_principal_regex = dplyr::case_when(
        !febre_valida_regex ~ NA_character_,
        regra_ictero_hemorragica ~ "febril_ictero_hemorragica",
        regra_hemorragica ~ "febril_hemorragica",
        regra_neurologica_meningea ~ "febril_neurologica_meningea",
        regra_exantematica ~ "febril_exantematica",
        regra_gastrointestinal ~ "febril_gastrointestinal",
        regra_respiratoria ~ "febril_respiratoria",
        regra_inespecifica ~ "febril_inespecifica",
        TRUE ~ NA_character_
      ),
      regex_classificado = !is.na(sindrome_principal_regex),
      fonte_classificacao_regex = dplyr::case_when(
        flag_febre_negada ~ "Febre negada no texto",
        !flag_febre ~ "Sem menção textual de febre",
        flag_febre & !regex_classificado ~ "Febre sem padrão sindrômico textual específico",
        regex_classificado ~ "Regex textual",
        TRUE ~ "Não classificado por regex"
      ),
      sintomas_regex = purrr::pmap_chr(
        list(flag_respiratorio, flag_exantematico, flag_gastrointestinal, flag_hemorragico,
             flag_icterico, flag_neurologico_meningeo, flag_inespecifico),
        function(resp, exant, gastro, hem, ict, neuro, inespec) {
          sintomas <- c(
            if (resp) "respiratorio",
            if (exant) "exantematico",
            if (gastro) "gastrointestinal",
            if (hem) "hemorragico",
            if (ict) "icterico",
            if (neuro) "neurologico_meningeo",
            if (inespec) "inespecifico"
          )
          if (length(sintomas) == 0) NA_character_ else paste(sintomas, collapse = "; ")
        }
      )
    )
}

summarise_regex_classification <- function(df_regex) {
  tibble::tibble(
    indicador = c(
      "Registros avaliados", "Com menção de febre", "Com febre negada",
      "Com febre válida para regex", "Classificados por regex", "Não classificados por regex"
    ),
    valor = c(
      nrow(df_regex),
      sum(df_regex$flag_febre, na.rm = TRUE),
      sum(df_regex$flag_febre_negada, na.rm = TRUE),
      sum(df_regex$febre_valida_regex, na.rm = TRUE),
      sum(df_regex$regex_classificado, na.rm = TRUE),
      sum(!df_regex$regex_classificado, na.rm = TRUE)
    )
  )
}

plot_regex_syndrome_distribution <- function(df_regex) {
  df_regex |>
    dplyr::filter(regex_classificado) |>
    dplyr::count(sindrome_principal_regex, sort = TRUE) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(sindrome_principal_regex, n), y = n)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Atendimentos", title = "Classificação sindrômica por regex") +
    ggplot2::theme_minimal(base_size = 12)
}

compare_cid_regex <- function(df_regex) {
  df_regex |>
    dplyr::mutate(
      convergencia_cid_regex = dplyr::case_when(
        is.na(classificacao_cid) & is.na(sindrome_principal_regex) ~ "sem_classificacao_cid_regex",
        is.na(classificacao_cid) & !is.na(sindrome_principal_regex) ~ "apenas_regex",
        !is.na(classificacao_cid) & is.na(sindrome_principal_regex) ~ "apenas_cid",
        classificacao_cid == sindrome_principal_regex ~ "convergente",
        classificacao_cid != sindrome_principal_regex ~ "divergente",
        TRUE ~ NA_character_
      )
    )
}
