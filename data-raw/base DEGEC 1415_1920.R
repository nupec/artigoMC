# A carregar as base de dados -----------------------------------------------------------------
# caminho dos arquivos
caminho <- list.files("data-raw/DEGES-Escola/",
                    #  pattern = "Tratados",
                      full.names = T)
# a carregar
base <- purrr::map_dfr(caminho, readxl::read_xlsx) |>
  janitor::clean_names()

baseFiltrada <- dplyr::select()

# Visão geral da base
dplyr::glimpse(base)

# Total de alunos por ano
table(base$`ANO LETIVO`)

# Total de alunos por ano e sexo
base |> dplyr::group_by(`ANO LETIVO`,  SEXO) |>
  dplyr::summarise(total = sum( `NÚMERO DE ALUNOS MATRICULADOS`)) |>
  tidyr::pivot_wider(
    names_from = c(SEXO),
    values_from = total
  ) |>
  dplyr::mutate(
    Total = Homens+Mulheres,
    Dif_H_M =  Homens-Mulheres,
    Racio_H_M = Homens/Mulheres
  )
