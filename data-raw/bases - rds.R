## code to prepare `baseRet` dataset goes here

 ## Base Geral
NUTS <- readxl::read_xlsx("dataset/Base Geral/NUTS.xlsx") |>
  janitor::clean_names() |>
  dplyr::mutate(dico = as.numeric(dico))
usethis::use_data(NUTS, overwrite = TRUE)

baseModelo <- readxl::read_xlsx("dataset/Modelacao/baseModelo.xlsx")
usethis::use_data(baseModelo, overwrite = TRUE)

baseMatriculaCiclo <- readxl::read_xlsx("dataset/Base Geral/baseMatriculaCiclo.xlsx")
usethis::use_data(baseMatriculaCiclo, overwrite = TRUE)

## Cofinanciamento
cofinanciamento <- readxl::read_xlsx("dataset/Cofinanciamento/baseCofGeral.xlsx",
                                     sheet = "RacioPorConselhoNUTSIII") |>
  janitor::clean_names() |>
  dplyr::mutate(dico = as.numeric(dico))
usethis::use_data(cofinanciamento, overwrite = TRUE)

cofinanciamentoTidy <- readxl::read_xlsx("dataset/Cofinanciamento/cofttTidy.xlsx") |>
  janitor::clean_names() |>
    dplyr::mutate(dico = as.numeric(dico))
usethis::use_data(cofinanciamentoTidy, overwrite = TRUE)

baseCofinanciamentoCiclos <- readxl::read_xlsx("dataset/Base Geral/baseCoF.xlsx")
usethis::use_data(baseCofinanciamentoCiclos, overwrite = TRUE)



racioCofCiclos <- readxl::read_excel("../../Cofinanciamentos/01_Bases_de_dados/racioCof2.xlsx")
usethis::use_data(racioCofCiclos, overwrite = TRUE)

racioCofPop <- readxl::read_excel("../../Cofinanciamentos/01_Bases_de_dados/racioCofPop.xlsx")
usethis::use_data(racioCofPop, overwrite = TRUE)

# baseracioCof <- dplyr::select(racioCof, c(2,3, 14, 15:23)) |>
#   dplyr::mutate(ano = as.numeric(stringr::str_sub(racioCof$ano, 1,4)))
# usethis::use_data(baseracioCof, overwrite = TRUE)

# Projeção populaciona
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

usethis::use_data(projPopulacional, overwrite = TRUE)

# Mapa

MapaBasePT <- baseMapaPT <- sf::st_read("data/shp/MapaBasePT.shx")
usethis::use_data(MapaBasePT, overwrite = TRUE)


