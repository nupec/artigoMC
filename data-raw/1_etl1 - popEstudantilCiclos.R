### Carregando as bases
# Fonte: PORTAL INFOESCOLAS - 1 .º CICLO - ENSINO GERAL - DADOS POR NUTS III / MUNICÍPIO
#        Fonte: DGEEC/MEdu (Dados reportados pelas escolas ao sistema de informação do MEdu)
# Disponível em:

# url("https://infoescolas.pt/bds.asp")
# 1 Ciclo -----------------------------------------------------------------------------------


Alunos1CicloPorRegiao <- readxl::read_excel("data-raw/Dados Infoescolas em maio de 2022/2021_1Ciclo_DadosPorRegiao.xlsx",
                                            sheet = "Populacao") |>
  janitor::clean_names() |>
  dplyr::rename(Município = nome_da_nuts_iii_municipio)

# Empilhando as bases

base <- Alunos1CicloPorRegiao # Faço uma cópia do arquivo original
nomesCol <- names(Alunos1CicloPorRegiao) # Faço uma cópia do nome das variável

base1ciclo <- NULL
aux <- NULL

(seq1 <- seq(4, 19 , 5))

for (i in seq1) {
  print(paste("Início do processo. Base", i, "empilhada"))
  aux <- base |>
    dplyr::select(1:3, all_of(i), all_of(i)+1, all_of(i)+2, all_of(i)+3, all_of(i)+4) |>
    dplyr::rename(
      ano = nomesCol[all_of(i)],
      ano1  = nomesCol[all_of(i)+1],
      ano2  = nomesCol[all_of(i)+2],
      ano3  = nomesCol[all_of(i)+3],
      ano4  = nomesCol[all_of(i)+4]
    )
  base1ciclo <- rbind(base1ciclo, aux)

}

base1ciclo <- base1ciclo|>  tidyr::pivot_longer(
  cols = ano1:ano4,
  names_to = "anocurricular",
  values_to = "matricula"
)

rm(Alunos1CicloPorRegiao, aux, base, nomesCol, seq1, i)

# 2 Ciclo -------------------------------------------------------------------------------------

Alunos2CicloPorRegiao <- readxl::read_excel("data-raw/Dados Infoescolas em maio de 2022/2021_2Ciclo_DadosPorRegiao.xlsx",
                                            sheet = "Populacao") |>
  janitor::clean_names() |>
  dplyr::rename(Município = nome_da_nuts_iii_municipio)

# Empilhando as bases

base <- Alunos2CicloPorRegiao
nomesCol <- names(Alunos2CicloPorRegiao)

base2ciclo <- NULL
aux <- NULL

(seq1 <- seq(4, 13, 3))

for (i in seq1) {

  print(paste("Início do processo. Base", i, "empilhada"))

  aux <- base |> dplyr::select(1:3, all_of(i), all_of(i)+1, all_of(i)+2) |>
    dplyr::rename(
      ano = nomesCol[all_of(i)],
      ano5  = nomesCol[all_of(i)+1],
      ano6  = nomesCol[all_of(i)+2]
    )
  base2ciclo <- rbind(base2ciclo, aux)
}

base2ciclo <- base2ciclo|>  tidyr::pivot_longer(
  cols = ano5:ano6,
  names_to = "anocurricular",
  values_to = "matricula")

rm(Alunos2CicloPorRegiao, aux, base, nomesCol, seq1, i)

# 3 Ciclo -------------------------------------------------------------------------------------
Alunos3CicloPorRegiao <- readxl::read_excel("data-raw/Dados Infoescolas em maio de 2022/2021_3Ciclo_DadosPorRegiao.xlsx",
                                            sheet = "Populacao") |>
  janitor::clean_names() |>
  dplyr::rename(Município = nome_da_nuts_iii_municipio)

# Empilhando as bases
base <- Alunos3CicloPorRegiao
nomesCol <- names(Alunos3CicloPorRegiao)

base3ciclo <- NULL
aux <- NULL

(seq1 <- seq(4, 16, 4))

for (i in seq1) {

  print(paste("Início do processo. Base", i, "empilhada"))

  aux <- base |> dplyr::select(1:3, all_of(i),all_of(i)+1, all_of(i)+2, all_of(i)+3) |>
    dplyr::rename(
      ano = nomesCol[all_of(i)],
      ano7  = nomesCol[all_of(i)+1],
      ano8  = nomesCol[all_of(i)+2],
      ano9  = nomesCol[all_of(i)+3]
    )
  base3ciclo <- rbind(base3ciclo, aux)
}

base3ciclo <- base3ciclo|>  tidyr::pivot_longer(
  cols = ano7:ano9,
  names_to = "anocurricular",
  values_to = "matricula")

rm(Alunos3CicloPorRegiao, aux, base, nomesCol, seq1, i)

# Secundário -------------------------------------------------------------------------------------
AlunosSecPorRegiao <- readxl::read_excel("data-raw/Dados Infoescolas em maio de 2022/2021_Secundario_CH_DadosPorRegiao.xlsx",
                                         sheet = "Populacao") |>
  janitor::clean_names() |>
  dplyr::rename(Município = nome_da_nuts_iii_municipio)

# Empilhando as bases
base <- AlunosSecPorRegiao
nomesCol <- names(AlunosSecPorRegiao)

baseSec <- NULL
aux <- NULL

(seq1 <- seq(4, 16, 4))

for (i in seq1) {

  print(paste("Início do processo. Base", i, "empilhada"))

  aux <- base |> dplyr::select(1:3, all_of(i),all_of(i)+1, all_of(i)+2, all_of(i)+3) |>
    dplyr::rename(
      ano = nomesCol[all_of(i)],
      ano10  = nomesCol[all_of(i)+1],
      ano11  = nomesCol[all_of(i)+2],
      ano12  = nomesCol[all_of(i)+3]
    )
  baseSec <- rbind(baseSec, aux)
}

baseSec <- baseSec|>  tidyr::pivot_longer(
  cols = ano10:ano12,
  names_to = "anocurricular",
  values_to = "matricula")

rm(AlunosSecPorRegiao, aux, base, nomesCol, seq1, i)

# Secundário Profissionalizante -------------------------------------------------------------------------------------
AlunosSecProfPorRegiao <- readxl::read_excel("data-raw/Dados Infoescolas em maio de 2022/2021_Secundario_Profissional_DadosPorRegiao.xlsx",
                                             sheet = "Populacao") |>
  janitor::clean_names()

# Empilhando as bases
base <- AlunosSecProfPorRegiao
nomesCol <- names(AlunosSecProfPorRegiao)

baseSecProf <- NULL
aux <- NULL

(seq1 <- seq(4, 11, 2))

for (i in seq1) {

  print(paste("Início do processo. Base", i, "empilhada"))

  aux <- base |> dplyr::select(1:3, all_of(i), all_of(i)+1) |>
    dplyr::rename(
      ano = nomesCol[all_of(i)],
      matricula  = nomesCol[all_of(i)+1]
    )
  baseSecProf <- rbind(baseSecProf, aux)
}

baseSecProf <- baseSecProf|>
  dplyr::mutate(anocurricular = "secp",
                matricula = as.numeric(matricula)) |>
  #   tidyr::drop_na(matricula) |>
  dplyr::relocate(anocurricular, .after = ano) |>
  dplyr::rename(Município = nome_da_nuts_iii_municipio)

rm(AlunosSecProfPorRegiao, aux, base, nomesCol, seq1, i)

# Empilhando tudo -----------------------------------------------------------------------------

# Crio 3 colunas que consolidam os ciclos de três formas diferentes
matriculaPt1619T <- dplyr::bind_rows(base1ciclo, base2ciclo, base3ciclo,
                                     baseSec, baseSecProf) |>
  dplyr::mutate(
    ciclo1 = dplyr::case_when(
      anocurricular %in% c("ano1", "ano2", "ano3", "ano4") ~ "cb1",
      anocurricular %in% c("ano5", "ano6") ~ "cb2",
      anocurricular %in% c("ano7", "ano8", "ano9") ~ "cb3",
      anocurricular %in% c("ano10", "ano11", "ano12") ~ "sec",
      anocurricular == "secp" ~ "secpr"),
    ciclo2 = dplyr::case_when(
      anocurricular %in% c("ano1", "ano2", "ano3", "ano4", "ano5", "ano6",
                           "ano7", "ano8", "ano9") ~ "cb",
      anocurricular %in% c("ano10", "ano11", "ano12") ~ "sec",
      anocurricular == "secp" ~ "secpr"),
    ciclo3 = dplyr::case_when(
      anocurricular %in% c("ano1", "ano2", "ano3", "ano4", "ano5", "ano6",
                           "ano7", "ano8", "ano9") ~ "cb",
      anocurricular %in% c("ano10", "ano11", "ano12", "secp") ~ "sec")) |>
  dplyr::filter(ano != "2016/2017")

# No próximo passo acrescentamos a "chave-primária".
matriculaPt1619T2 <-  matriculaPt1619T |>
  dplyr::mutate(
    chave = paste(dico, ano, ciclo2, sep = "_")
  ) |>
  dplyr::relocate(chave, .before = "dico") |>
  dplyr::group_by(dico, chave, ano, Município, ciclo2) |>
  dplyr::summarise(
    tt_alunos = sum(matricula, na.rm = T)
  )

matriculaPt1719 <-  matriculaPt1619T |>
  dplyr::mutate(
    chave = paste(dico, ano, ciclo1, sep = "_")
  ) |>
  dplyr::rename(ciclo = ciclo1) |>
  dplyr::group_by(dico, chave, ano, Município, ciclo) |>
  dplyr::summarise(
    tt_alunos = sum(matricula, na.rm = T)
  ) |>
  dplyr::relocate(chave) |>
  dplyr::rename(municipio = Município) |>
  dplyr::arrange(municipio, ano)

# Pré-escola ----------------------------------------------------------------------------------

# A importar a base completa de matricula Pré-escolar (pe)

caminho <- list.files("data-raw/DEGES-Escola/",
                      full.names = T)

baseMatricula <- purrr::map_dfr(caminho, readxl::read_excel) |>
  janitor::clean_names()

base_2017e18_2019e20 <- dplyr::filter(baseMatricula, ano_letivo %in% c("2017/2018",
                                                                       "2018/2019",
                                                                       "2019/2020"))
#Lista dos 131 Conselhos a serem utilizadas na análise dos cofinanciamentos
NUTS <- readxl::read_xlsx("dataset/Base Geral/NUTS.xlsx")

concelhos <- NUTS|>
  janitor::clean_names() |>
  dplyr::select(dico, concelho) |>
  dplyr::rename(municipio = concelho) |>
  dplyr::mutate(dico = as.numeric(dico)) |>
  dplyr::arrange(municipio)

#Base matrícula Pré-Escola
matPE <- base_2017e18_2019e20 |>
  # dplyr::filter(natureza == "Público") |> #[Opção: Matrícula Total ou apenas Público]
  dplyr::filter(nivel_de_ensino == "Educação pré-escolar") |>
  dplyr::group_by(ano_letivo, municipio, nivel_de_ensino) |>
  dplyr::summarise(
    tt_alunos = sum(numero_de_alunos_matriculados, na.rm = T)) |>
  dplyr::inner_join(concelhos, by = "municipio") |>
  dplyr::rename(ciclo = nivel_de_ensino,
                ano = ano_letivo) |>
  dplyr::mutate(ciclo = "pe",
                chave = paste(dico, ano, ciclo, sep = "_")) |>
  dplyr::select(chave, dico, ano, municipio, ciclo, tt_alunos) |>
  dplyr::arrange(dico)

baseMatriculaCiclo <- dplyr::bind_rows(matPE, matriculaPt1719) |>
  dplyr::arrange(dico, ano)

# readr::write_rds(baseMatriculaCiclo, "dataset/rds/baseMatriculaCiclo.rds")
# writexl::write_xlsx(baseMatriculaCiclo, "dataset/Base Geral/baseMatriculaCiclo.xlsx")
