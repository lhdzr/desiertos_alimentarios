library(sf)
library(leaflet)
library(tidyverse)
library(htmlwidgets)
source("tesina/funciones_descarga_inegi.R")
source("tesina/codigos_municipales_nl.R")

# DEFINICIÓN DE DIRECTORIOS DE SALIDA
city_limits_directory <- "Datos/raw/limites_municipales/"
ageb_output_dir <- "Datos/raw/agebs"

# LECTURA DATOS CENSALES 2020
censo <- read.csv("Datos/raw/ageb_mza_urbana_19_cpv2020_csv/ageb_mza_urbana_19_cpv2020/conjunto_de_datos/conjunto_de_datos_ageb_urbana_19_cpv2020.csv")


# LECTURA DATOS ÍNDICE DE MARGINACIÓN URBANA
IMU_2020 <- readxl::read_xls("Datos/raw/IMU_2020/IMU_2020.xls", sheet = "IMU_2020") %>% 
  filter(ENT==19) %>%
  select(CVE_AGEB,IM_2020,GM_2020,IMN_2020)


# GENERACIÓN DE DATOS DE LOS AGEBS EN NUEVO LEON 
# (LOS DATOS FUERON DESCARGADOS USANDO EL SCRIPT descarga_datos_geo.R)
agebs_nl <- data.frame()
for (code in nl_codes) {
  mun_str <- sprintf("%03d",code)
  filepath <- paste0(ageb_output_dir,"/ageb19",mun_str,".geojson")
  mun_agebs <- read_sf(filepath)
  agebs_nl <- rbind(agebs_nl,mun_agebs)
  print(paste("Municipio",mun_str,"agregado."))
}

# CRUZAR DATOS DE AGEBS CON DATOS DE INDICE DE MARGINACIÓN URBANA
agebs_nl$CVE_AGEB <- paste0(agebs_nl$cve_agee,agebs_nl$cve_agem,agebs_nl$cve_loc,agebs_nl$cve_ageb)
agebs_nl <- left_join(agebs_nl,IMU_2020,by="CVE_AGEB")
agebs_nl$GM_2020 <- factor(agebs_nl$GM_2020, levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"), ordered = TRUE)
# ENCONTRAR CENTROIDE DE AGEBS
agebs_nl$centroid <- st_centroid(agebs_nl$geometry)



# MAPEAR AGEBS POR GRADO DE MARGINACIÓN
pal <- colorFactor("YlOrRd", agebs_nl$GM_2020, ordered = TRUE)

map <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = agebs_nl, fillColor = ~pal(GM_2020),
                                                stroke = TRUE, opacity = 1,
                                                fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
                                                popup = ~paste("<strong>AGEB:</strong>",agebs_nl$cve_ageb,
                                                               "<br><strong>GMU:</strong>",agebs_nl$GM_2020))

map

# GUARDAR MAPA COMO ARCHIVO HTML
saveWidget(map, file = "Datos/processed/marginacion_nl.html")


#####DENUE

datos_denue <- read_sf("Datos/raw/INEGI_DENUE_12052023/INEGI_DENUE_12052023.shp") %>% 
  select(id,clee,nom_estab,raz_social,
         codigo_act,nombre_act,per_ocu,
         cve_ent,cve_mun,ageb,manzana,
         latitud,longitud,geometry)

buffers <- st_buffer(datos_denue, 1)
st_buffer()
datos_denue_trimmed$buffer <- st_buffer(datos_denue_trimmed$geometry, dist = 1000)
denue_map <- leaflet(data=datos_denue_trimmed) %>% addTiles() %>% 
  addCircleMarkers(~longitud,~latitud, clusterOptions = markerClusterOptions())
denue_map



# MAPA ZMM CON UNIDADES ECONÓMICAS
agebs_zmm <- makeAGEBMap(19,zmm_codes,.5) %>% 
  addCircleMarkers(datos_denue_trimmed$longitud,
                   datos_denue_trimmed$latitud, 
                   clusterOptions = markerClusterOptions(),
                   label = as.character(datos_denue_trimmed$nom_estab))
agebs_zmm
