library(fpc)
library(stats)
library(scatterplot3d)
library(purrr)
library(skimr)
library(cluster)
library(recipes)
library(ggplot2)

# Esta função cria k clusters para uma dataframe


## Análise preliminar --------------------------------------------------------------------------

# baseG <- readxl::read_excel("dataset/Modelação/baseModelo1.xlsx")
#
# ggplot(baseG,
#        aes(x=tt_alunos,
#            y=tx_ret,
#            color=ciclo))+
#   geom_point(size=2) #+
 # scale_color_manual(values = c("grey30", "grey65", "grey100")) #+
  #geom_point(shape=1, size=4, color="black")


# 1) Aplicação para o Ciclo Báscio - 2017------------------------------------------------------------
baseBasico <- readxl::read_excel("dataset/Modelação/baseModelo1.xlsx")|>
  dplyr::filter(ciclo == "cb")

ciclo <- "cb"
referencia <- "2017/2018"
ano <- 2017
ncluster <- c("1", "2", "1", "2", "3", "1", "2", "3", "4")
kluster <-  c("k2", "k2", "k3", "k3", "k3", "k4", "k4", "k4", "k4")

base <- baseBasico |>
  dplyr::filter(ano == referencia)

base$equidade <- base$equidade |>
  tidyr::replace_na(median(baseBasico$equidade, na.rm = T))

base$tx_ret <- base$tx_ret |>
  tidyr::replace_na(median(baseBasico$tx_ret, na.rm = T))

base$Cofinanciamento <- base$Cofinanciamento

v1 <- "ano"
v2 <- "equidade"
v3 <- "tx_ret"
v4 <-  "Cofinanciamento"
v5 <-  NULL

df <- base |>
  dplyr::select(dplyr::all_of(v1), dplyr::all_of(v2), dplyr::all_of(v3),
                dplyr::all_of(v4), dplyr::all_of(v5)) |>
  dplyr::mutate(
    ano = stringr::str_sub(base$ano, 1,4))

# Definindo as variáveis que serão consideradas no processo de clusteriazação
clusterGeral <- function(k,t){
  set.seed(1)
  df <- dplyr::filter(df, ano == {{t}})
 # df <- scale(df[2:4])            #padroniza as variáveis
  base <- stats::kmeans(df, {{k}})
}


k <- purrr::map2(2:4, ano, clusterGeral)

ClusterMeans2017 <- rbind(k[[c(1,2)]],
                          k[[c(2,2)]],
                          k[[c(3,2)]]) |> as.data.frame() |>
  dplyr::mutate(ano = ano,
                ciclo = ciclo) |>
  dplyr::bind_cols(ncluster, kluster)

col <- base[1:5]
dfk <- cbind(col,k[[c(1,1)]], k[[c(2,1)]], k[[c(3,1)]]) |>
  as.data.frame()

nomescol <- c("chave", "ano", "dico", "Concelho", "ciclo", "k2","k3", 'k4')

colnames(dfk) <- nomescol

dfkpivot <- dfk |>
  tidyr::pivot_longer(
    cols = k2:k4,
    names_to = "cluster",
    values_to = "k"
  ) |>
  dplyr::mutate(
    ciclo = "bas"
  )

kgeralBas2017 <- dfkpivot

# 2) Básico 2018/2019 -------------------------------------------------------------------------
baseBasico <- readxl::read_excel("dataset/Modelação/baseModelo1.xlsx")|>
  dplyr::filter(ciclo == "cb")

ciclo <- "cb"
referencia <- "2018/2019"
ano <- 2018

base <- baseBasico |>
  dplyr::filter(ano == referencia)

base$equidade <- base$equidade |>
  tidyr::replace_na(median(baseBasico$equidade, na.rm = T))

base$tx_ret <- base$tx_ret |>
  tidyr::replace_na(median(baseBasico$tx_ret, na.rm = T))

v1 <- "ano"
v2 <- "equidade"
v3 <- "tx_ret"
v4 <-  "Cofinanciamento"
v5 <-  NULL

df <- base |>
  dplyr::select(dplyr::all_of(v1), dplyr::all_of(v2), dplyr::all_of(v3),
                dplyr::all_of(v4), dplyr::all_of(v5)) |>
  dplyr::mutate(
    ano = stringr::str_sub(base$ano, 1,4))

# Definindo as variáveis que serão consideradas no processo de clusteriazação
clusterGeral <- function(k,t){
  set.seed(1)
  df <- dplyr::filter(df, ano == {{t}})
 # df <- scale(df[2:4])            #padroniza as variáveis
  base <- stats::kmeans(df, {{k}})
}

col <- base[1:5]
k <- purrr::map2(2:4, ano, clusterGeral)

ClusterMeans2018 <- rbind(k[[c(1,2)]],
                          k[[c(2,2)]],
                          k[[c(3,2)]]) |> as.data.frame() |>
  dplyr::mutate(ano = ano,
                ciclo = ciclo) |>
  dplyr::bind_cols(ncluster, kluster)

dfk <- cbind(col,k[[c(1,1)]], k[[c(2,1)]], k[[c(3,1)]]) |>
  as.data.frame()

# nomescol <- c("dico", "ano", "municipio", "k2","k3", 'k4')

colnames(dfk) <- nomescol

dfkpivot <- dfk |>
  tidyr::pivot_longer(
    cols = k2:k4,
    names_to = "cluster",
    values_to = "k"
  ) |>
  dplyr::mutate(
    ciclo = "bas"
  )

kgeralBas2018 <- dfkpivot

# 3) Básico 2019/2020 -------------------------------------------------------------------------

baseBasico <- readxl::read_excel("dataset/Modelação/baseModelo1.xlsx")|>
  dplyr::filter(ciclo == "cb")

ciclo <- "cb"
referencia <- "2019/2020"
ano <- 2019

base <- baseBasico |>
  dplyr::filter(ano == referencia)

base$equidade <- base$equidade |>
  tidyr::replace_na(median(baseBasico$equidade, na.rm = T))

base$tx_ret <- base$tx_ret |>
  tidyr::replace_na(median(baseBasico$tx_ret, na.rm = T))

v1 <- "ano"
v2 <- "equidade"
v3 <- "tx_ret"
v4 <-  "Cofinanciamento"
v5 <-  NULL

df <- base |>
  dplyr::select(dplyr::all_of(v1), dplyr::all_of(v2), dplyr::all_of(v3),
                dplyr::all_of(v4), dplyr::all_of(v5)) |>
  dplyr::mutate(
    ano = stringr::str_sub(base$ano, 1,4))

# Definindo as variáveis que serão consideradas no processo de clusteriazação
clusterGeral <- function(k,t){
  set.seed(1)
  df <- dplyr::filter(df, ano == {{t}})
 # df <- scale(df[2:4])            #padroniza as variáveis
  base <- stats::kmeans(df, {{k}})
}

 col <- base[1:5]
k <- purrr::map2(2:4, ano, clusterGeral)

ClusterMeans2019 <- rbind(k[[c(1,2)]],
                          k[[c(2,2)]],
                          k[[c(3,2)]]) |> as.data.frame() |>
  dplyr::mutate(ano = ano,
                ciclo = ciclo) |>
  dplyr::bind_cols(ncluster, kluster)

dfk <- cbind(col,k[[c(1,1)]], k[[c(2,1)]], k[[c(3,1)]]) |>
  as.data.frame()

#nomescol <- c("dico", "ano", "municipio", "k2","k3", 'k4')

colnames(dfk) <- nomescol

dfkpivot <- dfk |>
  tidyr::pivot_longer(
    cols = k2:k4,
    names_to = "cluster",
    values_to = "k"
  ) |>
  dplyr::mutate(
    ciclo = "bas"
  )

ClusterMeansInvBas1719 <- rbind(ClusterMeans2017,
                             ClusterMeans2018,
                             ClusterMeans2019)

kgeralBas2019 <- dfkpivot
kgeralBasInv <- rbind(kgeralBas2017, kgeralBas2018, kgeralBas2019)

# 4) Secundário -------------------------------------------------------------------------------
baseBasico <- readxl::read_excel("dataset/Modelação/baseModelo1.xlsx")|>
  dplyr::filter(ciclo == "sec")

ciclo <- "sec"
referencia <- "2017/2018"
ano <- 2017

base <- baseBasico |>
  dplyr::filter(ano == referencia)

base$equidade <- base$equidade |>
  tidyr::replace_na(median(baseBasico$equidade, na.rm = T))

base$tx_ret <- base$tx_ret |>
  tidyr::replace_na(median(baseBasico$tx_ret, na.rm = T))

v1 <- "ano"
v2 <- "equidade"
v3 <- "tx_ret"
v4 <-  "Cofinanciamento"
v5 <-  NULL

df <- base |>
  dplyr::select(dplyr::all_of(v1), dplyr::all_of(v2), dplyr::all_of(v3),
                dplyr::all_of(v4), dplyr::all_of(v5)) |>
  dplyr::mutate(
    ano = stringr::str_sub(base$ano, 1,4))

# Definindo as variáveis que serão consideradas no processo de clusteriazação
clusterGeral <- function(k,t){
  set.seed(1)
  df <- dplyr::filter(df, ano == {{t}})
 # df <- scale(df[2:4])            #padroniza as variáveis
  base <- stats::kmeans(df, {{k}})
}

col <- base[1:5]
k <- purrr::map2(2:4, ano, clusterGeral)

ClusterMeans2017 <- rbind(k[[c(1,2)]],
                          k[[c(2,2)]],
                          k[[c(3,2)]]) |> as.data.frame() |>
  dplyr::mutate(ano = ano,
                ciclo = ciclo)|>
  dplyr::bind_cols(ncluster, kluster)


dfk <- cbind(col,k[[c(1,1)]], k[[c(2,1)]], k[[c(3,1)]]) |>
  as.data.frame()

#nomescol <- c("dico", "ano", "municipio", "k2","k3", 'k4')

colnames(dfk) <- nomescol

dfkpivot <- dfk |>
  tidyr::pivot_longer(
    cols = k2:k4,
    names_to = "cluster",
    values_to = "k"
  ) |>
  dplyr::mutate(
    ciclo = "sec"
  )

kgeralSecInv2017 <- dfkpivot

# 5) Secundário 2018/2019 -------------------------------------------------------------------------

baseBasico <- readxl::read_excel("dataset/Modelação/baseModelo1.xlsx")|>
  dplyr::filter(ciclo == "sec")

ciclo <- "sec"
referencia <- "2018/2019"
ano <- 2018

base <- baseBasico |>
  dplyr::filter(ano == referencia)

base$equidade <- base$equidade |>
  tidyr::replace_na(median(baseBasico$equidade, na.rm = T))

base$tx_ret <- base$tx_ret |>
  tidyr::replace_na(median(baseBasico$tx_ret, na.rm = T))

v1 <- "ano"
v2 <- "equidade"
v3 <- "tx_ret"
v4 <-  "Cofinanciamento"
v5 <-  NULL

df <- base |>
  dplyr::select(dplyr::all_of(v1), dplyr::all_of(v2), dplyr::all_of(v3),
                dplyr::all_of(v4), dplyr::all_of(v5)) |>
  dplyr::mutate(
    ano = stringr::str_sub(base$ano, 1,4))

# Definindo as variáveis que serão consideradas no processo de clusteriazação
clusterGeral <- function(k,t){
  set.seed(1)
  df <- dplyr::filter(df, ano == {{t}})
 # df <- scale(df[2:4])            #padroniza as variáveis
  base <- stats::kmeans(df, {{k}})
}

col <- base[1:5]
k <- purrr::map2(2:4, ano, clusterGeral)

ClusterMeans2018 <- rbind(k[[c(1,2)]],
                          k[[c(2,2)]],
                          k[[c(3,2)]]) |> as.data.frame() |>
  dplyr::mutate(ano = ano,
                ciclo = ciclo)|>
  dplyr::bind_cols(ncluster, kluster)


dfk <- cbind(col,k[[c(1,1)]], k[[c(2,1)]], k[[c(3,1)]]) |>
  as.data.frame()

#nomescol <- c("dico", "ano", "municipio", "k2","k3", 'k4')

colnames(dfk) <- nomescol

dfkpivot <- dfk |>
  tidyr::pivot_longer(
    cols = k2:k4,
    names_to = "cluster",
    values_to = "k"
  ) |>
  dplyr::mutate(
    ciclo = "sec"
  )

kgeralSecInv2018 <- dfkpivot

# 6) Secundário 2019/2020 -------------------------------------------------------------------------

baseBasico <- readxl::read_excel("dataset/Modelação/baseModelo1.xlsx")|>
  dplyr::filter(ciclo == "sec")

ciclo <- "sec"
referencia <- "2019/2020"
ano <- 2019

base <- baseBasico |>
  dplyr::filter(ano == referencia)

base$equidade <- base$equidade |>
  tidyr::replace_na(median(baseBasico$equidade, na.rm = T))

base$tx_ret <- base$tx_ret |>
  tidyr::replace_na(median(baseBasico$tx_ret, na.rm = T))

v1 <- "ano"
v2 <- "equidade"
v3 <- "tx_ret"
v4 <-  "Cofinanciamento"
v5 <-  NULL

df <- base |>
  dplyr::select(dplyr::all_of(v1), dplyr::all_of(v2), dplyr::all_of(v3),
                dplyr::all_of(v4), dplyr::all_of(v5)) |>
  dplyr::mutate(
    ano = stringr::str_sub(base$ano, 1,4))

# Definindo as variáveis que serão consideradas no processo de clusteriazação
clusterGeral <- function(k,t){
  set.seed(1)
  df <- dplyr::filter(df, ano == {{t}})
 # df <- scale(df[2:4])            #padroniza as variáveis
  base <- stats::kmeans(df, {{k}})
}

 col <- base[1:5]
k <- purrr::map2(2:4, ano, clusterGeral)

ClusterMeans2019 <- rbind(k[[c(1,2)]],
                          k[[c(2,2)]],
                          k[[c(3,2)]]) |> as.data.frame() |>
  dplyr::mutate(ano = ano,
                ciclo = ciclo) |>
  dplyr::bind_cols(ncluster, kluster)


dfk <- cbind(col,k[[c(1,1)]], k[[c(2,1)]], k[[c(3,1)]]) |>
  as.data.frame()

# nomescol <- c("dico", "ano", "municipio", "k2","k3", 'k4')

colnames(dfk) <- nomescol

dfkpivot <- dfk |>
  tidyr::pivot_longer(
    cols = k2:k4,
    names_to = "cluster",
    values_to = "k"
  ) |>
  dplyr::mutate(
    ciclo = "sec"
  )

kgeralSecInv2019 <- dfkpivot

#kgeral <- rbind(kgeralBas, kgeralSec, kgeralSecPr)

kgeralSecInv <- rbind(kgeralSecInv2017,
                      kgeralSecInv2018,
                      kgeralSecInv2019)


ClusterMeansInvSec1719 <- rbind(ClusterMeans2017,
                                ClusterMeans2018,
                                ClusterMeans2019)

# 7) Secndário Pr -----------------------------------------------------------------------------
baseBasico <- readxl::read_excel("dataset/Modelação/baseModelo1.xlsx")|>
  dplyr::filter(ciclo == "secpr")

ciclo <- "secpr"
referencia <- "2017/2018"
ano <- 2017

base <- baseBasico |>
  dplyr::filter(ano == referencia)

base$equidade <- base$equidade |>
  tidyr::replace_na(median(baseBasico$equidade, na.rm = T))

base$tx_ret <- base$tx_ret |>
  tidyr::replace_na(median(baseBasico$tx_ret, na.rm = T))

base$Cofinanciamento <- base$Cofinanciamento |>
  tidyr::replace_na(0)

v1 <- "ano"
v2 <- "equidade"
v3 <- "tx_ret"
v4 <-  "Cofinanciamento"
v5 <-  NULL

df <- base |>
  dplyr::select(dplyr::all_of(v1), dplyr::all_of(v2), dplyr::all_of(v3),
                dplyr::all_of(v4), dplyr::all_of(v5)) |>
  dplyr::mutate(
    ano = stringr::str_sub(base$ano, 1,4))

# Definindo as variáveis que serão consideradas no processo de clusteriazação
clusterGeral <- function(k,t){
  set.seed(1)
  df <- dplyr::filter(df, ano == {{t}})
 # df <- scale(df[2:4])            #padroniza as variáveis
  base <- stats::kmeans(df, {{k}})
}

k <- purrr::map2(2:4, ano, clusterGeral)

ClusterMeans2017 <- rbind(k[[c(1,2)]],
                          k[[c(2,2)]],
                          k[[c(3,2)]]) |> as.data.frame() |>
  dplyr::mutate(ano = ano,
                ciclo = ciclo)|>
  dplyr::bind_cols(ncluster, kluster)
col <- base[1:5]
dfk <- cbind(col,k[[c(1,1)]], k[[c(2,1)]], k[[c(3,1)]]) |>
  as.data.frame()

# nomescol <- c("dico", "ano", "municipio", "ciclo","k2","k3", 'k4')

colnames(dfk) <- nomescol

dfkpivot <- dfk |>
  tidyr::pivot_longer(
    cols = k2:k4,
    names_to = "cluster",
    values_to = "k"
  ) |>
  dplyr::mutate(
    ciclo = "secpr"
  )

kgeralSecpr2017 <- dfkpivot

# 8) Secundário Pr 2018/2019 -------------------------------------------------------------------------

baseBasico <- readxl::read_excel("dataset/Modelação/baseModelo1.xlsx")|>
  dplyr::filter(ciclo == "secpr")

ciclo <- "secpr"
referencia <- "2018/2019"
ano <- 2018

base <- baseBasico |>
  dplyr::filter(ano == referencia)

base$equidade <- base$equidade |>
  tidyr::replace_na(median(baseBasico$equidade, na.rm = T))

base$tx_ret <- base$tx_ret |>
  tidyr::replace_na(median(baseBasico$tx_ret, na.rm = T))

base$Cofinanciamento <- base$Cofinanciamento |>
  tidyr::replace_na(0)

v1 <- "ano"
v2 <- "equidade"
v3 <- "tx_ret"
v4 <-  "Cofinanciamento"
v5 <-  NULL

df <- base |>
  dplyr::select(dplyr::all_of(v1), dplyr::all_of(v2), dplyr::all_of(v3),
                dplyr::all_of(v4), dplyr::all_of(v5)) |>
  dplyr::mutate(
    ano = stringr::str_sub(base$ano, 1,4))

# Definindo as variáveis que serão consideradas no processo de clusteriazação
clusterGeral <- function(k,t){
  set.seed(1)
  df <- dplyr::filter(df, ano == {{t}})
 # df <- scale(df[2:4])            #padroniza as variáveis
  base <- stats::kmeans(df, {{k}})
}

 col <- base[1:5]
k <- purrr::map2(2:4, ano, clusterGeral)

ClusterMeans2018 <- rbind(k[[c(1,2)]],
                          k[[c(2,2)]],
                          k[[c(3,2)]]) |> as.data.frame() |>
  dplyr::mutate(ano = ano,
                ciclo = ciclo)|>
  dplyr::bind_cols(ncluster, kluster)

dfk <- cbind(col,k[[c(1,1)]], k[[c(2,1)]], k[[c(3,1)]]) |>
  as.data.frame()

#nomescol <- c("dico", "ano", "municipio", "k2","k3", 'k4')

colnames(dfk) <- nomescol

dfkpivot <- dfk |>
  tidyr::pivot_longer(
    cols = k2:k4,
    names_to = "cluster",
    values_to = "k"
  ) |>
  dplyr::mutate(
    ciclo = "secpr"
  )

kgeralSecpr2018 <- dfkpivot

# 9) Secundário Pr 2019/2020 -------------------------------------------------------------------------
baseBasico <- readxl::read_excel("dataset/Modelação/baseModelo1.xlsx")|>
  dplyr::filter(ciclo == "secpr")

ciclo <- "secpr"
referencia <- "2019/2020"
ano <- 2019

base <- baseBasico |>
  dplyr::filter(ano == referencia)

base$equidade <- base$equidade |>
  tidyr::replace_na(median(baseBasico$equidade, na.rm = T))

base$tx_ret <- base$tx_ret |>
  tidyr::replace_na(median(baseBasico$tx_ret, na.rm = T))

base$Cofinanciamento <- base$Cofinanciamento |>
  tidyr::replace_na(0)

v1 <- "ano"
v2 <- "equidade"
v3 <- "tx_ret"
v4 <-  "Cofinanciamento"
v5 <-  NULL

df <- base |>
  dplyr::select(dplyr::all_of(v1), dplyr::all_of(v2), dplyr::all_of(v3),
                dplyr::all_of(v4), dplyr::all_of(v5)) |>
  dplyr::mutate(
    ano = stringr::str_sub(base$ano, 1,4))

# Definindo as variáveis que serão consideradas no processo de clusteriazação
clusterGeral <- function(k,t){
  set.seed(1)
  df <- dplyr::filter(df, ano == {{t}})
 # df <- scale(df[2:4])            #padroniza as variáveis
  base <- stats::kmeans(df, {{k}})
}

 col <- base[1:5]
k <- purrr::map2(2:4, ano, clusterGeral)

ClusterMeans2019 <- rbind(k[[c(1,2)]],
                          k[[c(2,2)]],
                          k[[c(3,2)]]) |> as.data.frame() |>
  dplyr::mutate(ano = ano,
                ciclo = ciclo)|>
  dplyr::bind_cols(ncluster, kluster)

dfk <- cbind(col,k[[c(1,1)]], k[[c(2,1)]], k[[c(3,1)]]) |>
  as.data.frame()

# nomescol <- c("dico", "ano", "municipio", "k2","k3", 'k4')

colnames(dfk) <- nomescol

dfkpivot <- dfk |>
  tidyr::pivot_longer(
    cols = k2:k4,
    names_to = "cluster",
    values_to = "k"
  ) |>
  dplyr::mutate(
    ciclo = "secpr"
  )

kgeralSecpr2019 <- dfkpivot

kgeralInvSecpr <- rbind(kgeralSecpr2017, kgeralSecpr2018, kgeralSecpr2019)

ClusterMeansInvSecPr1719 <- rbind(ClusterMeans2017,
                               ClusterMeans2018,
                               ClusterMeans2019)

# Final ---------------------------------------------------------------------------------------


ClusterMeansGeralInv1719 <- rbind(ClusterMeansInvBas1719,
                                  ClusterMeansInvSec1719,
                                  ClusterMeansInvSecPr1719) |>
  dplyr::rename(
    Cluster =`...6`,
    `_cluster` = `...7`)

 kgeral2 <- rbind(kgeralBasInv, kgeralSecInv, kgeralInvSecpr)

    # writexl::write_xlsx(kgeral2, "dataset/Modelação/kGeralInv.xlsx")
    # writexl::write_xlsx(ClusterMeansGeralInv1719, "dataset/Modelação/ClusterModelo2.xlsx")

# IMPORTANTE: RODAR PRIMERIO KMEANS - MODELO1
 kgeral <- dplyr::rename(kgeral, sim1 = k)
 kgeral2 <- dplyr::rename(kgeral2, sim2 = k)

 kgeral12 <- cbind(kgeral, kgeral2) |>
   dplyr::select(1:7,14)
#
#   writexl::write_xlsx(kgeral12, "dataset/Modelação/kgeral12.xlsx")
