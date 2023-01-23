base <- readxl::read_excel("data/baseModelo1.xlsx") |>
  dplyr::select(6:8)

base$equidade <- base$equidade |>
  tidyr::replace_na(median(base$equidade, na.rm = T))

base$tx_ret <- base$tx_ret |>
  tidyr::replace_na(median(base$tx_ret, na.rm = T))

totwss <- NULL

for (i in 2:15) {
  totwss <- append(totwss, kmeans(base, centers = i)$tot.withinss)
}

plot(x=2:15, y=totwss, type = "b", xlab = "Clusters",
     ylab = "Total Withinss")

totwss <- totwss |> as_data_frame()

kmeans(base, 12)
