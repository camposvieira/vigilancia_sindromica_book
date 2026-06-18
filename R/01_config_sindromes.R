# Configuração inicial e moldável das síndromes febris.
# Esta tabela deve ser vista como versão operacional inicial, sujeita a ajustes
# com gestores e especialistas do projeto.

sindromes_config <- tibble::tribble(
  ~sindrome_id, ~nome, ~prioridade, ~descricao, ~sintomas_principais, ~doencas_relacionadas, ~cids_relacionados,

  "febril_ictero_hemorragica",
  "Síndrome febril íctero-hemorrágica",
  1,
  "Febre associada a icterícia, colúria, acolia ou sinais compatíveis de acometimento hepático, com ou sem sangramento.",
  "febre; icterícia; olhos amarelos; pele amarela; colúria; acolia; sangramento",
  "leptospirose; febre amarela; hepatites agudas; malária grave",
  "A95; A27; B15; B16; B17; B19; B50; B51; B52; B53; B54",

  "febril_hemorragica",
  "Síndrome febril hemorrágica",
  2,
  "Febre associada a manifestações hemorrágicas ou sinais cutâneo-mucosos sugestivos de sangramento.",
  "febre; sangramento; epistaxe; gengivorragia; petéquias; púrpura; hematêmese; melena",
  "dengue grave; febres hemorrágicas virais; leptospirose grave; meningococcemia",
  "A90; A91; A92; A96; A98; A99",

  "febril_neurologica_meningea",
  "Síndrome febril neurológica/meníngea",
  3,
  "Febre associada a sinais neurológicos, alteração do nível de consciência ou sinais meníngeos.",
  "febre; rigidez de nuca; convulsão; confusão mental; rebaixamento; fotofobia; cefaleia intensa",
  "meningite; encefalite; arboviroses neuroinvasivas; meningococcemia",
  "A39; G00; G01; G02; G03; G04; G05",

  "febril_exantematica",
  "Síndrome febril exantemática",
  4,
  "Febre associada a exantema, rash, manchas vermelhas ou lesões cutâneas agudas.",
  "febre; exantema; rash; manchas vermelhas; lesões de pele; prurido",
  "sarampo; rubéola; zika; dengue com exantema; escarlatina",
  "B05; B06; A38; A92.8; A90",

  "febril_respiratoria",
  "Síndrome febril respiratória",
  5,
  "Febre associada a sintomas respiratórios altos ou baixos.",
  "febre; tosse; coriza; dor de garganta; odinofagia; dispneia; congestão nasal",
  "influenza; COVID-19; pneumonia; outras viroses respiratórias",
  "J00; J01; J02; J03; J04; J05; J06; J09; J10; J11; J12; J13; J14; J15; J16; J17; J18; U07",

  "febril_gastrointestinal",
  "Síndrome febril gastrointestinal",
  6,
  "Febre associada a sintomas gastrointestinais, como diarreia, vômitos, náuseas ou dor abdominal.",
  "febre; diarreia; vômitos; náuseas; dor abdominal; inapetência",
  "gastroenterites infecciosas; intoxicações alimentares; arboviroses com sintomas gastrointestinais",
  "A00; A01; A02; A03; A04; A05; A06; A07; A08; A09",

  "febril_inespecifica",
  "Síndrome febril inespecífica",
  7,
  "Febre associada a sintomas gerais, sem predomínio claro de sistema respiratório, gastrointestinal, exantemático, hemorrágico, ictero-hemorrágico ou neurológico.",
  "febre; cefaleia; mialgia; artralgia; prostração; mal-estar; dor no corpo; calafrios",
  "dengue inicial; chikungunya; zika; influenza inicial; viroses inespecíficas",
  "R50; B34"
)
