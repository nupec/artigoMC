# Carregar pacotes -------------
library(sf)
library(geobr)
library(ggplot2)
library(dplyr)
library(fs)

# O pacote {sf} (Simple Features for R) (Pebesma 2020, 2018) possibilita
# trabalhar com bases de dados espaciais.

# Importar dados:
## Duas abordagens:
# - Bases que são georreferenciadas
# - Bases que não são georreferenciadas

# Trabalhando com bases georreferenciadas -----------------

## Primeiro vamos baixar os dados -------------------------
# Isso só precisa ser feito uma vez!

# Criar a pasta onde colocaremos os arquivos
# fs::dir_create("data/shp")

# Ver quais arquivos estão na pasta que criamos
fs::dir_ls("data/shp")

concelhos <- NUTS$concelho
baseMapaPT <- sf::st_read("data/shp/concelhos.shx")

baseMapaPTCont <- baseMapaPT |>
  dplyr::filter(NAME_2 %in% concelhos) |>
  janitor::clean_names() |>
  dplyr::rename(municipio = name_2,
                dico = cca_2) |>
  dplyr::mutate(dico = as.numeric(dico)) |>
  dplyr::arrange(dico)

baseMapaPTnuts <- baseMapaPTCont |>
  dplyr::inner_join(NUTS, "dico") |>
  dplyr::select(dico, geometry)

#Exemplo
baseMapaPTnuts |>
  ggplot() +
  geom_sf()
#Importante!!! Está faltando um Concelho mas não sei ainda qual.

 write_sf(baseMapaPTnuts, "data/shp/MapaBasePT.shp")
