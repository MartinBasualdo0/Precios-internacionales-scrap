ggplot(data = serie) + 
  geom_smooth(mapping = aes(x = ï..Fecha, y = Precio))
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

serie %>%
  filter(Producto == "Trigo") %>%
  ggplot(mapping = aes(x = Mercado, y = Precio)) +
  geom_boxplot()

unique(serie$Producto)

ggplot(data = serie) + 
  geom_boxplot(mapping = aes(x = Mercado, y = Precio)) + 
  facet_wrap(~ Producto, nrow = 2)


serie %>%
  group_by(Mercado) %>%
  filter(Precio > 45) %>%
  summarise(
    count = n()
  )

serie

# formato fecha -----------------------------------------------------------

serie$ï..Fecha %>%
  as.Date(format = "%d-%m-%y")

# eliminar outliers -------------------------------------------------------

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
  summarise(media_mensual = mean(Precio))




