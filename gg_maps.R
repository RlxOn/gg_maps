##### Script para realizar mapas con ggplot
pacman::p_load(sf, ggplot2, tidyverse, taylor)

##### Mapas a nivel nacional =========================================================================
##Cargamos el archivo .shp
mapa <- "C:/Users/raulb/Documents/Datasets/Shapefile - Censo 2010 (Estatal)/inegi_refcenesta_2010.shp"
mapa_sf <- st_read(mapa)

##Modificamos los nombres por caracteres especiales y dividimos las poblaciones
mapa_sf <- mapa_sf %>%
  mutate(nombre_igg = case_when(
    nombre_igg == "Yucat\xe1n" ~ "Yucatan",
    nombre_igg == "San Luis Potos\xed" ~ "San Luis Potosi",
    nombre_igg == "Quer\xe9taro" ~ "Queretaro",
    nombre_igg == "Nuevo Le\xf3n" ~ "Nuevo Leon",
    nombre_igg == "Michoac\xe1n" ~ "Michoacan",
    nombre_igg == "M\xe9xico" ~ "Mexico",
    TRUE ~ nombre_igg),
  p_total = round(p_total / 1000000,3),
  pobmas = round(pobmas / 1000000,3),
  pobfem = round(pobfem / 1000000,3))

#Mapa 1. Variable continua

##Realizamos el grafico
ggplot() + geom_sf(data = mapa_sf, aes(fill = p_total)) +
  labs(title = "Poblacion total por estado",
       subtitle = NULL,
       caption = "Fuente: INEGI (Censo Estatal 2010)",
       fill = "Poblacion total (millones)") +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  scale_fill_continuous(low = "#ffcece", high = "#590000")

#Mapa 2. Variable discreta

##Hacemos los cortes para tener intervalos
intervalos <- c(0,1,3,5,8)
etiquetas <- c("Menor a un millon", "Entre uno y tres millones",
               "Entre tres y cinco millones", "Entre cinco y ocho millones")

##Creamos la variable
mapa_sf <- mapa_sf %>%
  mutate(pobmas_corte = cut(pobmas, breaks = intervalos, labels = etiquetas))

##Realizamos el grafico
ggplot() + geom_sf(data = mapa_sf, aes(fill = pobmas_corte)) +
  labs(title = "Poblacion masculina por estado",
       subtitle = NULL,
       caption = "Fuente: INEGI (Censo Estatal 2010)",
       fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("#9BAD29", "#319864", "#006372", "#2F4858"))

#### Mapas a nivel municipal =================================================================
#Cargamos el .shp
mapa_muni <- "C:/Users/raulb/Documents/Datasets/Shapefile - Censo 2010 (Municipal)/inegi_refcenmuni_2010.shp"
mapa_sf_muni <- st_read(mapa_muni)

#Elegimos algun estado de nuestro interes
mapa_cdmx <- mapa_sf_muni %>%
  filter(nom_ent == "Distrito Federal")

#Realizamos el grafico
ggplot() + geom_sf(data = mapa_cdmx, aes(fill = pocupada/1000)) +
  labs(title = "Poblacion ocupada por alcaldia",
       subtitle = NULL,
       caption = "Fuente: INEGI (Censo Municipal 2010)",
       fill = "Poblacion (en miles)") +
  scale_fill_continuous(low = "#aeffe8", high = "#006045") +
  theme_minimal()

#### Mapas de temperaturas maximas
#Cargamos el .shp
temp <- "C:/Users/raulb/Documents/Datasets/Shapefile - Temperatura máxima/inegi_conttempma_1998.shp"
mapa_temp <- st_read(temp)

#Modificamos la variable de temp max
mapa_temp <- mapa_temp %>%
  mutate(tmax = case_when(
    tmax_rango == "de 16\xb0 a 20\xb0 C" ~ "De 16 a 20 C",
    tmax_rango == "de 12\xb0 a 16\xb0 C" ~ "De 12 a 16 C",
    tmax_rango == "M\xe1s de 40\xb0 C" ~ "Mas de 40 C",
    tmax_rango == "de 28\xb0 a 32\xb0 C" ~ "De 28 a 32 C",
    tmax_rango == "de 24\xb0 a 28\xb0 C" ~ "De 24 a 28 C",
    tmax_rango == "de 36\xb0 a 40\xb0 C" ~ "De 36 a 40 C",
    tmax_rango == "de 32\xb0 a 36\xb0 C" ~ "De 32 a 36 C",
    tmax_rango == "de 20\xb0 a 24\xb0 C" ~ "De 20 a 24 C",
    tmax_rango == "Menos de 8\xb0 C" ~ "Menos de 8 C",
    tmax_rango == "de 8\xb0 a 12\xb0 C" ~ "De 8 a 12 C",
    TRUE ~ tmax_rango),
    tmax = factor(tmax, levels = c("Menos de 8 C", "De 8 a 12 C", "De 12 a 16 C",
                                   "De 16 a 20 C", "De 20 a 24 C", "De 24 a 28 C",
                                   "De 28 a 32 C", "De 32 a 36 C", "De 36 a 40 C",
                                   "Mas de 40 C"), ordered = TRUE))

#Realizamos el grafico
ggplot() + geom_sf(data = mapa_temp, aes(fill = tmax)) +
  labs(title = "Temperaturas maximas en Mexico",
       subtitle = NULL,
       caption = "Fuente: INEGI",
       fill = "Rangos de temperatura") + 
  theme_minimal() + 
  scale_fill_brewer(palette = "RdBu", direction = -1)