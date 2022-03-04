library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)


# formato fecha -----------------------------------------------------------

serie$?..Fecha %>%
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
   a?o = year(Fecha)
  ) %>%
  group_by(Mercado, Producto, a?o, mes) %>%
  summarise(Precio_mensual = mean(Precio))



# variacion interanual ----------------------------------------------------

view(serie_mensual %>%
  group_by(Mercado, Producto) %>%
  # arrange(mes, a?o) %>%
  mutate(
    var_ia = (Precio_mensual - lag(Precio_mensual, 12)) / lag(Precio_mensual, 12)
  ))

view(serie_mensual %>%
       arrange(a?o, mes))


# Graficos linea ----------------------------------------------------------

serie_rosario <- filter(serie_mensual, Mercado == 'Rosario')

soja_graf <- serie_rosario%>%
  filter(Mercado == 'Rosario' && Producto == 'Soja')

trigo_graf <- serie_rosario%>%
  filter(Mercado == 'Rosario' && Producto == 'Trigo')


girasol_graf <- serie_rosario%>%
  filter(Mercado == 'Rosario' && Producto == 'Girasol')

(girasol_graf)




plot_ly(serie_mensual, x = ~serie_mensual$mes, y = ~trigo_graf$Precio_mensual, name = 'Trigo', type = 'scatter', mode = 'lines')


view(merge(soja_graf,girasol_graf,all=TRUE, by = soja_graf$mes))

view(
  full_join(
    filter(serie_mensual, Mercado=='Rosario' && Producto == 'Soja'),
    filter(serie_mensual, Mercado=='Rosario' && Producto == 'Trigo'),
    by = "mes"
    )%>%
     full_join(filter(serie_mensual, Mercado=='Rosario' && Producto == 'Maiz'), by='mes')%>%
     full_join(filter(serie_mensual, Mercado=='Rosario' && Producto == 'Girasol'), by ='mes')%>%
     full_join(filter(serie_mensual, Mercado=='Rosario' && Producto == 'Sorgo'),by = 'mes')
  )

plot_ly(serie_rosario, x = ~serie_mensual$mes, y = ~trigo_graf$Precio_mensual, name = 'Trigo', type = 'scatter', mode = 'lines')%>% 
  add_trace(y = ~soja_graf$Precio_mensual, name = 'Soja', mode = 'lines') %>%
  add_trace(y = ~girasol_graf$Precio_mensual, name = 'Girasol', mode = 'lines')
