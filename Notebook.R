library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)


# formato fecha -----------------------------------------------------------

serie$ï..Fecha %>%
  as.Date(format = "%d-%m-%y")

# eliminar outliers -------------------------------------------------------

serie %>%
  filter(Producto == "Trigo") %>%
  ggplot(mapping = aes(x = Mercado, y = Precio)) +
  geom_boxplot()

unique(serie$Producto)

ggplot(data = serie,) + 
  geom_boxplot(mapping = aes(x = Mercado, y = Precio, fill = Mercado)) + 
  facet_wrap(~ Producto, nrow = 2) +
  theme(legend.position="none")
theme_bw()

library(hrbrthemes)
ggplot(data = serie) + 
  geom_boxplot(mapping = aes(x = Mercado, y = Precio, fill = Mercado)) +
  theme_light()+
  theme(legend.position="none",
        panel.spacing = unit(0.5, "lines")) +
  facet_wrap(~ Producto, nrow = 2)

serie %>%
  group_by(Mercado) %>%
  filter(Precio > 45) %>%
  summarise(
    count = n()
  )
serie_mask <- serie %>%
  filter(between(Precio, 45, 1000))
serie_mask

# media mensual -----------------------------------------------------------

serie_mask %>%
  mutate(
   mes = month(Fecha),
   año = year(Fecha)
  ) %>%
  group_by(Mercado, Producto, año, mes) %>%
  summarise(Precio_mensual = mean(Precio))



# variacion interanual ----------------------------------------------------

view(serie_mensual %>%
  group_by(Mercado, Producto) %>%
  # arrange(mes, año) %>%
  mutate(
    var_ia = (Precio_mensual - lag(Precio_mensual, 12)) / lag(Precio_mensual, 12)
  ))

view(serie_mensual %>%
       arrange(año, mes))
