library(maps)
source("tesina/funciones_descarga_inegi.R")
source("tesina/codigos_municipales_nl.R")

# Mapa de los municipios centrales de la ZMM
central_map <- makeMunMap(19,center_codes,city_limits_directory)
# Mapa de los municipios periféricos de la ZMM
outer_map <- makeMunMap(19,outer_codes,city_limits_directory)
# Mapa de la zona metropolitana completa
zmm_map <- makeMunMap(19,zmm_codes,city_limits_directory)
# Mapa de todos los municipios de Nuevo León
nl_map <- makeMunMap(19,nl_codes,city_limits_directory)


# Visualización de mapas
central_map
outer_map
zmm_map
nl_map



##### MÉXICO, NUEVO LEÓN, ZMM

nl <- data.frame()
for (code in nl_codes) {
  code_str <- sprintf("%03d", code)
  # read the corresponding geojson file
  file_name <- paste0("Datos/raw/limites_municipales/",19, code_str, ".geojson")
  data <- read_sf(file_name)
  nl <- rbind(nl,data)
}

zmm_central <- data.frame()
for (code in center_codes) {
  code_str <- sprintf("%03d", code)
  # read the corresponding geojson file
  file_name <- paste0("Datos/raw/limites_municipales/",19, code_str, ".geojson")
  data <- read_sf(file_name)
  zmm_central <- rbind(zmm_central,data)
}

zmm_outer <- data.frame()
for (code in outer_codes) {
  code_str <- sprintf("%03d", code)
  # read the corresponding geojson file
  file_name <- paste0("Datos/raw/limites_municipales/",19, code_str, ".geojson")
  data <- read_sf(file_name)
  zmm_outer <- rbind(zmm_outer,data)
}

non_zmm <- data.frame()
for (code in non_zmm_codes) {
  code_str <- sprintf("%03d", code)
  # read the corresponding geojson file
  file_name <- paste0("Datos/raw/limites_municipales/",19, code_str, ".geojson")
  data <- read_sf(file_name)
  non_zmm <- rbind(non_zmm,data)
}


mapa_mexico <- map_data("world", "mexico")

ggplot() +
  geom_polygon(data = mapa_mexico, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_sf(data = nl, fill = "white", color = "black", alpha = 0.5) +
  coord_sf(crs = st_crs(4326)) +
  #labs(title = "Nuevo León") +
  theme_bw()


ggplot() +
  geom_sf(data = zmm_central, fill = "blue", color = "black", alpha = 0.5) +
  geom_sf(data = zmm_outer, fill = "light blue", color = "black", alpha = 0.5) +
  geom_sf(data = non_zmm, fill = "white", color = "black", alpha = 0.5) +
  theme_bw() #+ 
  #labs(title = "Nuevo León, y Zona \nMetropolitana de Monterrey. Municipios centrales\n y periféricos")

makeAGEBMap(19,zmm_codes,0.5)


#### RUTAS TRANSPORTE MONTERREY
rutas <- read.csv("rutas_transporte.csv")
head(rutas)
rutas <- read_sf("rutas_transporte/rutas_transporte.shp")
rutas <- st_transform(rutas, "+proj=longlat +datum=WGS84")
summary(rutas)
leaflet() %>% addTiles() %>% addPolylines(data = rutas)