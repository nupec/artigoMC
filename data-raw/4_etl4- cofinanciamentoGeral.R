# 1) Base dmatricula: 2017/2018 a 2019/2020
# IMPORTANTE: Carregar a base com CTLR + SHIFT + L
baseMatricula <- baseMatriculaCiclo[-1] |>
  tidyr::pivot_wider(
    names_from = "ciclo",
    values_from = "tt_alunos"
  ) |>
  tidyr::replace_na(list(sec   = 0)) |>
  tidyr::replace_na(list(secpr = 0)) |>
  dplyr::relocate(dico) |>
  dplyr::arrange(dico) |>
  dplyr::mutate(
    Total_Matricula = (cb1+cb2+cb3+sec+secpr),
    cb = (cb1+cb2+cb3),
    secundario = (sec+secpr)
  ) |>
  dplyr::relocate(Total_Matricula, .before = cb1) |>
  dplyr::relocate(cb, .before = cb1) |>
  dplyr::relocate(secundario, .before = sec)


# 2) A importar a base de Cofinancianmentos
cofinanciamento <- readxl::read_xlsx("dataset/Cofinanciamento/baseCofGeral.xlsx",
                                     sheet = "RacioPorConselhoNUTSIII") |>
  dplyr::select(-COF_MUN, -COF_NUTSIII, -NUTSIII_COD) |>
  dplyr::mutate(DICO = as.numeric(DICO)) |>
  janitor::clean_names() |>
  dplyr::arrange(dico)

baseMatCof <- dplyr::inner_join(baseMatricula, cofinanciamento, by = "dico")

## (Per capita = pop escolar/cof)
racioCof <- baseMatCof |>
  dplyr::mutate(
    Cof_tt = round(cof_total/Total_Matricula,2),
    Cof_pe = round(Cof_tt*pe,2),
    Cof_Bas = round(Cof_tt*cb,2),
    Cof_eb1 = round(Cof_tt*cb1,2),
    Cof_eb2 = round(Cof_tt*cb2,2),
    Cof_eb3 = round(Cof_tt*cb3,2),
    Cof_Sec = round(Cof_tt*secundario,2),
    Cof_sec_cch = round(cof_total*sec,2),
    Cof_sec_pr = round(cof_total*secpr,2)
  ) |>
  dplyr::arrange(municipio)

baseCoF <- dplyr::select(racioCof, dico, municipio, ano, Cof_eb1, Cof_eb2,
                          Cof_eb3, Cof_sec_cch, Cof_sec_pr) |>
  dplyr::rename(cb1 = Cof_eb1, cb2 = Cof_eb2, cb3 = Cof_eb3,
                sec = Cof_sec_cch, secpr = Cof_sec_pr) |>
  tidyr::pivot_longer(
    cols = cb1:secpr,
    names_to = "ciclo",
    values_to = "cofinanciamento"
  ) |>
  dplyr::mutate(chave = paste(dico, ano, ciclo, sep = "_")) |>
  dplyr::relocate(chave)

# readr::write_rds(baseCoF, "dataset/rds/baseCoF.rds")
# writexl::write_xlsx(baseCoF, "dataset/Base Geral/baseCoF.xlsx")



