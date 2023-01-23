# Importante: Antes de utilizar os códigos abaixo, faz-se necessário
# carregar as bases de dados com o comando CTRL+SHIFT+L

# Primeiro Join: Matricula e Retenção

Matricula <- readxl::read_xlsx("dataset/Base Geral/baseMatriculaCiclo.xlsx")
Equidade <- readxl::read_xlsx("dataset/Base Geral/baseEquidade.xlsx")
join1 <- dplyr::left_join(Equidade, Matricula, by = "chave") |>
  dplyr::select(c(1:5,7,12))

Retencao <-  readxl::read_xlsx("dataset/Base Geral/baseRetencaoCiclos.xlsx")
Cofinancimento <- readxl::read_xlsx("dataset/Base Geral/baseCoF.xlsx")
join2 <- dplyr::left_join(Retencao, Cofinancimento, by = "chave") |>
  dplyr::select(c(1, 6, 11))

join <- dplyr::left_join(join1, join2, by = "chave")

baseModelo <- join[-1] |>
  dplyr::rename(
    dico = dico.x,
    municipio = municipio.x,
    ano = ano.x,
    ciclo = ciclo.x,
    equidade = equidade_Media,
    Cofinanciamento = cofinanciamento
  )

# readr::write_rds(baseModelo, "dataset/rds/baseModelo.rds")
# writexl::write_xlsx(baseModelo, "dataset/Modelacao/baseModelo.xlsx")

## Cofinancimento e mapas

## Joins Espaciais ----------------------------------
# Podemos fazer JOINS com objetos espaciais


