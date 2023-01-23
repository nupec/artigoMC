# Importar as bases de dados
racioCof <- readxl::read_excel("../../Cofinanciamentos/01_Bases_de_dados/racioCof2.xlsx")
racioCofPop <- readxl::read_excel("../../Cofinanciamentos/01_Bases_de_dados/racioCofPop.xlsx")
baseracioCof <- dplyr::select(racioCof, c(2,3, 14, 15:23)) |>
  dplyr::mutate(
    ano = lubridate::as_date(ano))
baseracioCofPop <- dplyr::select(racioCofPop, c(2,9:17))

# AED - Racio Cofinanciamento/Ciclos Escolares
library(ggplot2)

df <- dplyr::select(racioCof, ano, municipio, Cof_pe)

grafico1 <- df |>
  dplyr::group_by(ano) |>
  ggplot(aes(ano, Cof_pe))

grafico1

grafico1 +
  geom_boxplot(aes(color=ano))+
  scale_color_viridis_d()

grafico1 +
  geom_point(
    aes(x=ano, y= Cof_pe)
  )

