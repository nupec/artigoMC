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

base <- Alunos1CicloPorRegiao

nomesCol <- names(Alunos1CicloPorRegiao)

base1ciclo <- NULL
aux <- NULL

(seq1 <- seq(4, 19 , 5))

for (i in seq1) {

  print(paste("Início do processo. Base", i, "empilhada"))

  aux <- base |> dplyr::select(1:3, i,i+1, i+2, i+3, i+4) |>
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


rm(Alunos1CicloPorRegiao)

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

  aux <- base |> dplyr::select(1:3, i,i+1, i+2) |>
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

rm(Alunos2CicloPorRegiao)

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

  aux <- base |> dplyr::select(1:3, i,i+1, i+2, i+3) |>
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

rm(Alunos3CicloPorRegiao)

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

  aux <- base |> dplyr::select(1:3, i,i+1, i+2, i+3) |>
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

rm(AlunosSecPorRegiao)

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

  aux <- base |> dplyr::select(1:3, i,i+1) |>
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

# Empilhando tudo -----------------------------------------------------------------------------

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
        anocurricular %in% c("ano10", "ano11", "ano12", "secp") ~ "sec")

      )|>
    dplyr::mutate(anocurricular = as.factor(anocurricular))

  matriculaPt1619Ciclo1 <-  matriculaPt1619T |>
    dplyr::mutate(
      chave = paste(dico, ano, ciclo1, sep = "_")
    ) |>
    dplyr::relocate(chave, .before = "dico") |>
    dplyr::group_by(chave, dico, ano, Município, ciclo1) |>
    dplyr::summarise(
      tt_alunos = sum(matricula, na.rm = T)
    )


  # readr::write_rds(matriculaPt1619Ciclo1, "data/rds/baseMatriculaCiclo.rds")
  #
  # writexl::write_xlsx(matriculaPt1619Ciclo1, "data/xlsx/baseMatriculaCiclo.xlsx")


  matriculaPt1619Ciclo2 <-  matriculaPt1619T |>
    dplyr::mutate(
    chave = paste(dico, ano, ciclo2, sep = "_")
  ) |>
    dplyr::relocate(chave, .before = "dico") |>
    dplyr::group_by(chave, dico, ano, Município, ciclo2) |>
    dplyr::summarise(
      tt_alunos = sum(matricula, na.rm = T)
    )

  baseMatricula <- matriculaPt1619Ciclo2 |>
    dplyr::filter(!ano=="2016/2017")


# readr::write_rds(baseMatricula, "data/rds/baseMatricula.rds")
#
# writexl::write_xlsx(baseMatricula, "data/xlsx/baseMatricula.xlsx")


   # readr::write_rds(matriculaPt1619T, "data/rds/baseMatricula.rds")
   #
   # writexl::write_xlsx(matriculaPt1619T, "data/xlsx/baseMatricula.xlsx")
   #

  #Agrupando por cidades

# Exportando a base tratada -------------------------------------------------------------------

 # matriculaPt1619      <- dplyr::filter(matriculaPt1619T, tipo_de_regiao == "Município")
 # matriculaPt1719      <- dplyr::filter(matriculaPt1619,
 #                                       ano == c("2017/2018", "2018/2019","2019/2020"))

  # matriculaPt1619T |> dplyr::grup_by()

  # matriculaPt1719 <-  matriculaPt1719 |>
  #   dplyr::group_by(chave, ano, Município) |>
  #   dplyr::summarise(total_matricula = sum(matricula, na.rm =T))



 rm(AlunosSecProfPorRegiao, aux, base, i, nomesCol, seq1,
    base1ciclo, base2ciclo, base3ciclo, baseSec, baseSecProf)
