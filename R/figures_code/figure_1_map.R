library(geobr)
library(ggplot2)
library(data.table)
library(sf)

`%nin%` <- Negate(`%in%`)


# download data ----------------------------------------------------

br <- geobr::read_country()
ufs <- geobr::read_state()
acu <- geobr::read_urban_concentrations(year = 2015, simplified = T)




#### remove areas in the international border ------------
#' Internacional de Uruguaiana/Brasil (RS) 4322400
#' Internacional de Foz do Iguaçu/Brasil - Ciudad del Este/Paraguai (PR) 4108304
#' Internacional de Corumba/Brasil (MS) 5003207

#  Santa Cruz do Sul/RS because of min. urban area in 1975
#' Santa Cruz do Sul/RS 4316808

to_be_removed <- c(4322400, 4108304, 5003207)
acu2 <- subset(acu, code_urban_concentration %nin% to_be_removed)
head(acu2)

# only areas above 100K pop
setDT(acu2)[, pop_acu := sum(pop_total_2010), by= code_urban_concentration ]

acu3 <- subset(acu2, pop_acu >= 100000)


acu$name_urban_concentration |> unique() |> length()
acu3$name_urban_concentration |> unique() |> length()


### plot  ----------------------------------------------------

# back to sf
acu3 <- st_sf(acu3)


p <-
  ggplot() +
  geom_sf(data=ufs, color= 'gray', fill='gray99') +
  geom_sf(data=acu3, aes(fill=name_urban_concentration ), color=NA, show.legend = FALSE) +
  theme_void()

ggsave(p, file='./figures/figur_1.pdf', dpi = 300,
       width = 15, height = 15, units = 'cm')






