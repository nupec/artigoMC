library(bslib)
library(thematic)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(GGally)
library(scatterplot3d)
library(patchwork)
library(Hmisc)
library(gghighlight)
library(kableExtra)
library(formattable)
library(cowplot)
library(palmerpenguins)
library(forcats)
library(ggforce)
library(mvoutlier)

# Importante: Sempre inicial com CTLR + SHIFT +L

# Parte 1: Dados Cofinanciamento --------------------------------------------------------------

cofinanciamento <- cofinanciamento |>
  janitor::clean_names() |>
  dplyr::filter(cof_total > 0)

cofinanciamentoTidy <-  cofinanciamentoTidy |>
  janitor::clean_names() |>
  dplyr::filter(nutsiii_dsg != "Algarve") |>
  dplyr::select(nutsiii_dsg, fonte, valores) |>
  tidyr::replace_na(list(valores=0))

grafCofTidry <-  cofinanciamentoTidy |>
  dplyr::filter(fonte != "COF_NUTSIII+MUN") |>
  dplyr::group_by(nutsiii_dsg, fonte) |>
  dplyr::summarise(
    TotalCof = sum(valores)
  )


# Gráfico em barra
# podemos inverter os eixos. Uma forma de fazer isso é utilizando a
# função coord_flip():

cofinanciamento |>
  dplyr::mutate(nutsiii_cod = forcats::fct_reorder(nutsiii_cod, cof_total)) |>
  ggplot(aes(x = nutsiii_cod, y = cof_total)) +
  geom_col() +
  scale_y_continuous(labels = scales::dollar)  +
  labs(x = "NUTS III", y = "Cofinanciamento") +
  coord_flip()


cofinanciamento |>
  dplyr::group_by(nutsiii_cod) |>
  dplyr::summarise(
  Total_Cof = sum(cof_total)) |>
  dplyr::mutate(nutsiii_cod = forcats::fct_reorder(nutsiii_cod, Total_Cof)) |>
  ggplot(aes(x = nutsiii_cod, y = Total_Cof)) +
  geom_col() +
  scale_y_continuous(labels = scales::dollar)  +
  labs(x = "NUTS III", y = "Cofinanciamento") +
  coord_flip()

# É interessante também ordenar as barras do gráfico de barras, pois facilita a comparação das
# categorias

cofinanciamento |>
  dplyr::group_by(nutsiii_cod) |>
  dplyr::summarise(
    Total_Cof = sum(cof_total)) |>
  dplyr::mutate(nutsiii_cod = forcats::fct_reorder(nutsiii_cod, Total_Cof)) |>
  ggplot() +
  geom_col(aes(x = nutsiii_cod, y = Total_Cof)) +
  scale_y_continuous(labels = scales::dollar)  +
  labs(x = "NUTS III", y = "Cofinanciamento") +
  coord_flip() +
  theme_bw()

### Aplicação no modelo Tidy


## Barras multiplas 1
grafCofTidry |>
  ggplot(aes(x=forcats::fct_reorder(nutsiii_dsg, TotalCof), y=TotalCof, fill=fonte)) +
  geom_col(position = position_dodge2(preserve = "single")) +
# geom_label(aes(label = TotalCof, y = n/2), fill = "white") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar)  +
  labs(title = "Cofinanciamento",
       subtitle = "Municipal e NUTSIII",
       x = "NUTS III",
       y = "Cofinanciamento",
       color = "Temperatura média (ºC)") +
  theme_bw()

## Barras multiplas 2
grafCofTidry |>
  ggplot(aes(x=forcats::fct_reorder(nutsiii_dsg, TotalCof), y=TotalCof, fill=fonte)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar)  +
  labs(
    title = "Cofinanciamento",
    subtitle = "Municipal e NUTSIII",
    x = "NUTS III",
    y = "Cofinanciamento",
    color = "Verbas") +
  theme_bw()

## Barras 1 - Municipal
grafCofTidry |> dplyr::filter(TotalCof >0 & fonte == "COF_MUN") |>
  tidyr::drop_na(fonte) |>
  ggplot(aes(x=forcats::fct_reorder(nutsiii_dsg, TotalCof), y=TotalCof, fill=fonte)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar)  +
  labs(x = "NUTS III", y = "Cofinanciamento") +
  theme_bw()

## Barras 2 - NUTS III
grafCofTidry |> dplyr::filter(TotalCof >0 & fonte == "COF_NUTSIII") |>
  tidyr::drop_na(fonte) |>
  ggplot(aes(x=forcats::fct_reorder(nutsiii_dsg, TotalCof), y=TotalCof, fill=fonte)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar)  +
  labs(x = "NUTS III", y = "Cofinanciamento") +
  theme_bw()


# Outliers ------------------------------------------------------------------------------------
# Classificando oultlier
cofOul <-  cofinanciamento |>
  dplyr::mutate(
    desvio = cof_total-mean(cof_total),
    z_score_cof = (cof_total-mean(cof_total))/sd(cof_total),
    e_outlier_cof = dplyr::if_else(abs(z_score_cof) > 2, "É outlier", "Não é outlier"),
    quantis = dplyr::if_else(cof_total < 30018, "Q1",
                             dplyr::if_else(cof_total > 470541, "Q3", "IIQ"))
  )

ggplot(cofOul, aes(x = cof_total, y = desvio, colour = e_outlier_cof)) +
  geom_point() +
  facet_zoom(x = e_outlier_cof == "É outlier") +
  scale_x_continuous(labels = scales::dollar)


ggdotchart(cofOul, x = "concelho_dsg", y = "cof_total",
           group = "quantis", color = "quantis",
    palette = c('#999999','#E69F00','#56B4E9'),
    rotate = TRUE,
    sorting = "ascending",
    ggtheme = theme_bw(),
    y.text.col = TRUE,
    label.select = list(criteria = "cof_total < 30018 | cof_total > 470541")
    )


ggplot(cofOul, aes(x = cof_total, y = desvio, colour = quantis)) +
  geom_point() +
  facet_zoom(x = quantis == "Q3", split = TRUE) +
  gghighlight::gghighlight(2, cof_total > 1000000,
                           label_key = concelho_dsg,
                           label_params = list(size = 4, fill = "grey"))


# Anáalise de resíduo

cofinanciamento$residuo <- cofinanciamento$cof_total - mean(cofinanciamento$cof_total)


plot(cofinanciamento$residuo)
hist(cofinanciamento$residuo)

cofinanciamento |>
ggplot(aes(residuo)) +
  geom_line()


# Análise Cofinanciamento X Ciclos Escolares

mapaCofinanciamento <- MapaBasePT |>
  dplyr::inner_join(cofinanciamento, "dico")

mapaCofinanciamento |>
  dplyr::mutate(direcao = forcats::fct_reorder(direcao, n)) %>%
  dplyr::select(-1, -2) |>
  ggplot()+
  geom_sf()

plot(mapaCofinanciamento |>
       dplyr::select(-1, -2))



# Dataframes ----------------------------------------------------------------------------------
racioCof <- readxl::read_excel("../../Cofinanciamentos/01_Bases_de_dados/racioCof2.xlsx")
racioCofPop <- readxl::read_excel("../../Cofinanciamentos/01_Bases_de_dados/racioCofPop.xlsx")
baseracioCof <- dplyr::select(racioCof, c(2,3, 14, 15:23)) |>
  dplyr::mutate(ano = as.numeric(stringr::str_sub(racioCof$ano, 1,4)))

principaisCiclos <- dplyr::select(baseracioCof, c(ano, municipio, Cof_pe,
                                                  Cof_cb, Cof_sec))

principaisCiclosTidy <- principaisCiclos |>
  tidyr::pivot_longer(
    cols = Cof_pe:Cof_sec,
    names_to = "CofCiclos2",
    values_to = "racioCof2"
  )

racioTidy <- racioCof |>
    dplyr::select(ano, municipio, cof_total: Cof_sec_pr) |>
    tidyr::pivot_longer(
      cols = cof_total:Cof_sec_pr,
      names_to = "CofCiclos2",
      values_to = "racioCof2"
    )

principaisCiclosFiltrados <- dplyr::filter(principaisCiclos,
                                           ano == "2017") |>
  tidyr::replace_na(list(Cof_pe=0))


principaisCiclosFiltradosTidy <- principaisCiclosFiltrados |>
  tidyr::pivot_longer(
    cols = Cof_pe:Cof_sec,
    names_to = "ciclos",
    values_to = "racioCof"
  )
# AED - Racio Cofinanciamento/Ciclos Escolares

# scatter plot matrix
principaisCiclosFiltrados |>
  #dplyr::filter(ano == "2017") |>
  dplyr::select(-1,-2) |>
  ggpairs()

# Boxplot: 2017/2018

## Boxplot 1
ggplot(principaisCiclosFiltradosTidy, aes(racioCof, color = ciclos)) +
  geom_boxplot()


## Boxplot 2
p <- ggplot(principaisCiclosFiltradosTidy, aes(factor(ciclos), racioCof,
                                               fill=ciclos)) +
  geom_dotplot(binaxis='y', stackdir='center',
               stackratio=1.5, dotsize=0.3)

p

# dot plot with mean points
p +  stat_summary(fun=mean, fun.min = min, fun.max = max, geom="point",
               size=5, color="red")

p +  coord_flip()+
  stat_summary(fun=mean, fun.min = min, fun.max = max, geom="point",
               size=5, color="red")

# Dot plot with box plot

q <- ggplot(principaisCiclosFiltradosTidy, aes(factor(ciclos), racioCof,
                                               fill=ciclos)) +
  geom_boxplot()+
  #geom_boxplot(notch = TRUE)+
  #geom_violin(trim = FALSE)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.2)

q

# Add mean and standard deviation

q <- ggplot(principaisCiclosFiltradosTidy, aes(factor(ciclos),
                                               racioCof)) +
  geom_dotplot(binaxis='y', stackdir='center',dotsize = 0.3)

q  + stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
                  geom="crossbar", width=0.5)

# Function to produce summary statistics (mean and +/- sd)
data_summary <- function(x) {
  m <- mean(principaisCiclosFiltradosTidy$racioCof)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}







ggplot(principaisCiclosFiltradosTidy,
       aes(racioCof, color = ciclos))+
  geom_dotplot(dotsize = 0.4, na.rm = T, alpha=0.7)




# 3 dimensões


scatterplot3d(principaisCiclosFiltrados$Cof_pe,
              principaisCiclosFiltrados$Cof_cb,
              principaisCiclosFiltrados$Cof_sec,
              highlight.3d = TRUE, col.axis = "blue", col.grid = "lightblue",
              main = "Racio Cofinanciamento por Nível de Estudo", pch = 20,
              xlab = "Racio Cofinanciamento Pré-Escola",
              ylab = "Racio Cofinanciamento Ciclo Básico",
              zlab = "Racio Cofinanciamento Secundário",
              scale.y = 0.7)

# Basic density plot
mu <- principaisCiclosFiltradosTidy %>%
  group_by(ciclos) %>%
  dplyr::summarise(grp.mean = mean(racioCof))

ggplot(principaisCiclosFiltradosTidy, aes(racioCof, color = ciclos)) +
  geom_density()+
  geom_vline(aes(xintercept = grp.mean, color = ciclos),
             data = mu, linetype = 2) +
  scale_color_viridis_d()

#par(mfrow=c(2,2))

principaisCiclosFiltradosTidy %>%
  ggplot(aes(x = racioCof, fill = ciclos)) +
  geom_density(color = 'white', alpha=0.5) +
  theme_bw() +
  labs(x = "Racio Cofinanciamento",
       y = "Ciclos Escolares")

principaisCiclosFiltradosTidy %>%
  ggplot(aes(x = racioCof, fill = ciclos)) +
  geom_histogram(color = 'white', alpha=0.5) +
  theme_bw() +
  labs(x = "Racio Cofinanciamento",
       y = "Ciclos Escolares")


### Sumarização

summary(principaisCiclos[-1,-2])

describe(principaisCiclos[-1])


### Lollipop chart

principaisCiclosFiltradosTidy |>
  dplyr::filter(ano=="2017") |>
ggdotchart(x = "municipio", y = "racioCof",
           color = "ciclos",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "ascending",                        # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           ggtheme = theme_pubr(),                        # ggplot2 theme
           font.tickslab = c(6)
)

principaisCiclosFiltradosTidy |>
  dplyr::filter(ano=="2017") |>
  ggdotchart(x = "municipio", y = "racioCof",
           color = "ciclos",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "ascending",                        # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           ggtheme = theme_bw(),                        # ggplot2 theme
           rotate = TRUE,
           font.tickslab = c(6)
)

## bar plot

ggbarplot(dfm, x = "name", y = "mpg_z",
          fill = "mpg_grp",           # change fill color by mpg_level
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "asc",           # Sort the value in ascending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 90,          # Rotate vertically x axis texts
          ylab = "MPG z-score",
          xlab = FALSE,
          legend.title = "MPG Group"
)


# Tabelas

dt <- mtcars[1:5, 1:6]

(principaisCiclos |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
    Mínimo = min(Cof_pe, na.rm=T),
    Q25 = quantile(Cof_pe, c(0.25), na.rm=T),
    Média = mean(Cof_pe, na.rm=T),
    DP = sd(Cof_pe, na.rm=T),
    Q75 = quantile(Cof_pe, c(0.75), na.rm=T),
    Média = mean(Cof_pe, na.rm=T),
    IIQ = Q75-Q25
  ) |>
  kbl() |>
  kable_styling())

ft_dt <- mtcars[1:5, 1:4]
ft_dt$car <- row.names(ft_dt)
row.names(ft_dt) <- NULL
ft_dt$mpg <- color_tile("white", "orange")(ft_dt$mpg)
ft_dt$cyl <- cell_spec(ft_dt$cyl, angle = (1:5)*60,
                       background = "red", color = "white", align = "center")
ft_dt$disp <- ifelse(
  ft_dt$disp > 200,
  cell_spec(ft_dt$disp, color = "red", bold = T),
  cell_spec(ft_dt$disp, color = "green", italic = T)
)
ft_dt$hp <- color_bar("lightgreen")(ft_dt$hp)
ft_dt <- ft_dt[c("car", "mpg", "cyl", "disp", "hp")]

kbl(ft_dt, escape = F) %>%
  kable_paper("hover", full_width = F) %>%
  column_spec(5, width = "3cm") %>%
  add_header_above(c(" ", "Hello" = 2, "World" = 2))


kbl(principaisCiclos, caption = "Group Rows") %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("Group 1", 4, 7) %>%
  pack_rows("Group 2", 8, 10)


# Análise de Outlier --------------------------------------------------------------------------

dat <- ggplot2::mpg
summary(dat$hwy)

ggplot(dat) +
  aes(x = hwy, y = "" ) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

ggplot(cofinanciamentoTidy) +
  aes(x = valores, y = "", color=fonte) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

library(mvoutlier)
Y <- as.matrix(ggplot2::mpg[, c("cyl", "hwy")])
res <- aq.plot(Y)

y1 <- cofinanciamento[, c(6,7)]
res1 <- aq.plot(y1)


