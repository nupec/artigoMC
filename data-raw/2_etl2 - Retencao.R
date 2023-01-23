### Carregando as bases
# Fonte: https://www.dgeec.mec.pt/np4/248/

# url("https://www.dgeec.mec.pt/np4/248/%7B$clientServletPath%7D/?newsId=382&fileName=DGEEC_DSEE_DEEBS_2022_TxRetDes_NUTSIII20.xlsx")
# 1 Ciclo -----------------------------------------------------------------------------------

# Taxa de retenção e desistência (%), por sexo, nível de ensino, ciclo de estudos e ano de escolaridade -
#   Continente, NUTS II, III e Municípios – 2003/04 a 2020/2

Retencao <- readxl::read_excel("data-raw/Retenção/Tx_Ret2-2003-2021.XLSX") |>
  janitor::clean_names()

# Preechendo as colunas vazias (repetindo as que já aparecem)
base <- Retencao |>
  tidyr::fill("nut_i_continente", "nuts_ii_de_2013", "nuts_iii_de_2013")|>
  tidyr::drop_na(municipio) |> # excluindo as linhas vazias
  dplyr::filter(ano %in% c(2017, 2018, 2019)) # filtrando os anos de interesse

# Selecionando as colunas de interesse e pivotando (
# Opção 1
baseRetencao <- dplyr::select(base, c(ano, ano_letivo, dico,
                                      municipio, cb1, cb2, cb3, sec, secpr))|>
  tidyr::pivot_longer(
    cols = c(cb1:secpr),
    names_to = "ciclo",
    values_to = "tx_ret"
  ) |> # criando as chaves primárias
  dplyr::mutate(
    chave = paste(dico, ano_letivo, ciclo, sep = "_")
  ) |> # alguns arranjos
  dplyr::arrange(municipio, ano) |>
  dplyr::select(-ano_letivo) |>
  tidyr::drop_na(tx_ret) |>
  dplyr::relocate(chave, .before = ano)

# readr::write_rds(baseRetencao, "dataset/rds/baseRetencaoCiclos.rds")
# writexl::write_xlsx(baseRetencao, "dataset/Base Geral/baseRetencaoCiclos.xlsx")
