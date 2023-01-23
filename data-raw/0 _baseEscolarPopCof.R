# Parte 1: A importar bases de dados  ---------------------

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
  dplyr::relocate(Total_Matricula, .before = pe) |>
  dplyr::relocate(cb, .before = cb1) |>
  dplyr::relocate(secundario, .before = sec)
# 2) A importar a projeção populacional
projPopulacional <- readxl::read_excel("../../Cofinanciamentos/01_Bases_de_dados/PopResGE_Censos21_Municipios.xls",
                                       sheet = "Est_Modelo2",
                                       skip = 1,
                                       n_max =  278) |>
  # o proóximo código foi um artifício para arredondar as projeções
  tidyr::pivot_longer(
    cols = `3-17 anos`:`15-17 anos`,
    names_to = "FH",
    values_to = "pop"
  ) |>
  # a voltar ao formato wider
  dplyr::mutate(pop = round(pop, 0),
                DICO = as.numeric(DICO)) |>
  tidyr::pivot_wider(
    names_from = FH,
    values_from = pop
  ) |>
  janitor::clean_names() |>
  dplyr::relocate(dico) |>
  dplyr::arrange(dico)

# 3) A importar a base de Cofinancianmentos
cofinanciamento <- readxl::read_xlsx("dataset/Cofinanciamento/baseCofGeral.xlsx",
                                     sheet = "RacioPorConselhoNUTSIII")

cofinanciamentoTidy <- cofinanciamento |>
  tidyr::pivot_longer(
    cols = COF_MUN:COF_TOTAL,
    names_to = "VERBAS",
    values_to = "Cofinanciamento"
  ) |>
  dplyr::mutate(DICO = as.numeric(DICO)) |>
  janitor::clean_names() |>
  dplyr::arrange(dico)



# Parte 2: A unir Matricula e Cofinanciamento -------------------------------------------------
baseMatCof <- dplyr::inner_join(baseMatricula, cofinanciamento, by = "dico")

## (Per capita = pop escolar/cof)
  racioCof <- baseMatCof |>
    dplyr::mutate(
    Cof_tt = round(cof_total/Total_Matricula,2),
    Cof_pe = round(Cof_tt*pe,2),
    Cof_cb = round(Cof_tt*cb,2),
    Cof_cb1 = round(Cof_tt*cb1,2),
    Cof_cb2 = round(Cof_tt*cb2,2),
    Cof_cb3 = round(Cof_tt*cb3,2),
    Cof_sec = round(Cof_tt*secundario,2),
    Cof_sec_cch = round(cof_total*sec,2),
    Cof_sec_pr = round(cof_total*secpr,2)
  ) |>
  dplyr::arrange(municipio)

# Base de alunos - Ano de referência
## (Per capita = residentes(em idades escolares)/cof)

baseCofPop <- dplyr::right_join(projPopulacional, cofinanciamento, by = "dico") |>
  dplyr::select(-concelho_dsg) |>
  janitor::clean_names()

  racioCofPop <- baseCofPop |>
    dplyr::mutate(
      perc_3_17 = round((cof_total/x3_17_anos),2),
      perc_3_5 = round((cof_total/x3_17_anos)*x3_5_anos,2),
      perc_6_14 = round((cof_total/x3_17_anos)*x6_14_anos,2),
      perc_6_9 = round((cof_total/x3_17_anos)*x6_9_anos,2),
      perc_10_11 = round((cof_total/x3_17_anos)*x10_11_anos,2),
      perc_12_14 = round((cof_total/x3_17_anos)*x12_14_anos,2),
      perc_15_17 = round((cof_total/x3_17_anos)*x15_17_anos,2),
    )
# writexl::write_xlsx(racioCof, "../../Cofinanciamentos/01_Bases_de_dados/racioCof2.xlsx")
# writexl::write_xlsx(racioCofPop, "../../Cofinanciamentos/01_Bases_de_dados/racioCofPop.xlsx")

