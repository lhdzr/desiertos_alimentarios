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
#censo <- read.csv("Datos/raw/ageb_mza_urbana_19_cpv2020_csv/ageb_mza_urbana_19_cpv2020/conjunto_de_datos/conjunto_de_datos_ageb_urbana_19_cpv2020.csv")


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

# AGEBS MARGINADOS
agebs_marginados <- agebs_nl %>% filter(GM_2020 %in% c("Medio", "Alto", "Muy alto"))
agebs_marginados$buffer <- st_buffer(agebs_marginados$geometry, dist = 1000)


#####DENUE
datos_denue <- read_sf("Datos/raw/INEGI_DENUE_12052023/INEGI_DENUE_12052023.shp") %>% 
  select(id,clee,nom_estab,raz_social,
         codigo_act,nombre_act,per_ocu,
         cve_ent,cve_mun,ageb,manzana,
         latitud,longitud,geometry)

Encoding(datos_denue$nom_estab) <- "latin1"
Encoding(datos_denue$nombre_act) <- "latin1"

datos_denue$alimento_convencional <- 1

# OXXOS Y SEVEN ELEVENS
datos_denue$alimento_convencional[datos_denue$raz_social %in% c("CADENA COMERCIAL OXXO SA DE CV",
                                          "CADENA COMERCIAL OXXO",
                                          "CADENA COMERCIAL OXXO SUCURSAL HACIENDADEL ANGEL SA DE CV",
                                          "7-ELEVEN MEXICO SA DE CV", 
                                          "7 ELEVEN MEXICO SA DE CV", 
                                          "SEVEN ELEVEN DE MEXICO")] <- 0

oxxos_sevens <- datos_denue[datos_denue$raz_social %in% c("CADENA COMERCIAL OXXO SA DE CV",
                                                          "CADENA COMERCIAL OXXO",
                                                          "CADENA COMERCIAL OXXO SUCURSAL HACIENDADEL ANGEL SA DE CV",
                                                          "7-ELEVEN MEXICO SA DE CV", 
                                                          "7 ELEVEN MEXICO SA DE CV", 
                                                          "SEVEN ELEVEN DE MEXICO"),]

# DULCERÍAS
datos_denue$alimento_convencional[datos_denue$codigo_act=="461160"] <- 0 

# PALETERÍAS
datos_denue$alimento_convencional[datos_denue$codigo_act=="461170"] <- 0 

# OTROS ALIMENTOS
datos_denue$alimento_convencional[datos_denue$codigo_act=="461190"] <- 0 

# MISCELANEAS, TIENDAS DE ABARROTES
datos_denue$alimento_convencional[datos_denue$codigo_act=="461110"] <- 0

#buffer <- st_buffer(datos_denue$geometry,dist = 1000)

### UNIÓN DE DATOS AGEB Y DENUE
st_crs(agebs_nl)
st_crs(datos_denue)
datos_denue <- st_transform(datos_denue, crs = 4326)

#agebs_trimmed <- as.data.frame(agebs_nl) %>% 
  select(cve_agee,cve_agem,cve_loc,cvegeo,centroid) %>%
  rename(geometry.x=centroid) %>%
  st_as_sf()

#agebs_denue <- st_join(agebs_trimmed, datos_denue, join = st_nearest_feature) 


# MAPEAR AGEBS POR GRADO DE MARGINACIÓN
pal <- colorFactor("YlOrRd", agebs_nl$GM_2020, ordered = TRUE)

map <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = agebs_nl, fillColor = ~pal(GM_2020),
                                                stroke = TRUE, opacity = 1,
                                                fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
                                                popup = ~paste("<strong>AGEB:</strong>",agebs_nl$cve_ageb,
                                                               "<br><strong>GMU:</strong>",agebs_nl$GM_2020)) %>%
  addCircleMarkers(datos_denue$longitud,
                   datos_denue$latitud, 
                   clusterOptions = markerClusterOptions(),
                   popup = as.character(datos_denue$nom_estab)) 
map
 
#  addLegend("bottomright", pal = ~pal(GM_2020), values = ~GM_2020, title = "Grado de Marginación") %>%

#datos_denue$buffer <- st_buffer(datos_denue$geometry, 1000)

map <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = agebs_nl, fillColor = ~pal(GM_2020),
              stroke = TRUE, opacity = 1,
              fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
              popup = ~paste("<strong>AGEB:</strong>",agebs_nl$cve_ageb,
                             "<br><strong>GMU:</strong>",agebs_nl$GM_2020,
                             "<br><strong>Municipio:</strong>",agebs_nl$nom_agem)) %>% 
  addCircleMarkers(oxxos_sevens$longitud,
                   oxxos_sevens$latitud, 
                   clusterOptions = markerClusterOptions(),
                   popup = as.character(oxxos_sevens$nom_estab))

map


# GUARDAR MAPA COMO ARCHIVO HTML
saveWidget(map, file = "Datos/processed/marginacion_nl.html")





# MAPA ZMM CON UNIDADES ECONÓMICAS
agebs_zmm <- makeAGEBMap(19,zmm_codes,.5) %>% 
  addCircleMarkers(datos_denue$longitud,
                   datos_denue$latitud, 
                   clusterOptions = markerClusterOptions(),
                   popup = as.character(datos_denue$nom_estab))
agebs_zmm



#### BUFFERS
#### UNIDADES ECONÓMICAS
#buffers_denue <- st_buffer(datos_denue$geometry,dist = 1000)
#write_sf(buffer, "Datos/processed/buffers/buffers_denue/buffers_denue.shp")
buffers_denue <- read_sf("Datos/processed/buffers/buffers_denue/buffers_denue.shp")

ue_acceso <- datos_denue[datos_denue$alimento_convencional==1,]
buffers_acceso <- st_buffer(ue_acceso,dist = 1000)
write_sf(buffers_acceso, "Datos/processed/buffers/buffers_acceso/buffers_acceso.shp")
buffers_acceso <- read_sf("Datos/processed/buffers/buffers_acceso/buffers_acceso.shp")
#### AGEBS
agebs_nl$buffer <- st_buffer(agebs_nl$centroid, dist = 1000)
#write_sf(buffer_agebs, "Datos/processed/buffers/buffers_agebs/buffers_agebs.shp")
buffers_agebs <- read_sf("Datos/processed/buffers/buffers_agebs/buffers_agebs.shp")

### INTERSECCION DE CENTROIDES Y BUFFERS
interseccion <- st_intersects(agebs_nl$centroid,buffers_denue)
interseccion_acceso <- st_intersects(agebs_nl$centroid,buffers_acceso)
conteo <- sapply(interseccion,length)
conteo_acceso <- sapply(interseccion_acceso,length)
which(conteo==0)


#### AGEBS_MARGINADOS 
agebs_nl$buffer <- st_buffer(agebs_nl$centroid, dist = 1000)

buffers_agebs <- agebs_nl %>%
  as.data.frame() %>%
  select(CVE_AGEB,buffer) %>%
  st_as_sf()

agebs_denue <- st_join(buffers_agebs,datos_denue,join = st_intersects) %>%
  as.data.frame() %>%
  select(CVE_AGEB,id,alimento_convencional)

write.csv(agebs_denue, "Datos/processed/agebs_denue.csv")

agebs_denue_count <- agebs_denue  %>% 
  group_by(CVE_AGEB) %>%
  summarise(unidades_totales = n(),
            unidades_saludables = sum(alimento_convencional)) %>%
  mutate(proporcion_saludable = round(unidades_saludables / unidades_totales, 2))

agebs_nl <- agebs_nl %>%
  merge(agebs_denue_count, by = 'CVE_AGEB')

dir.create("Datos/processed/agebs_nl_indice")
write_sf(agebs_nl, "Datos/processed/agebs_nl_indice/agebs_nl_indice.shp")

agebs_marginados <- agebs_nl %>% filter(GM_2020 %in% c("Medio", "Alto", "Muy alto"))
dir.create("Datos/processed/agebs_marginados_indice")
write_sf(agebs_marginados, "Datos/processed/agebs_marginados_indice/agebs_marginados_indice.shp")

agebs_nl$indice_acceso <- 






agebs_marginados_buffers <- agebs_marginados %>% 
  as.data.frame() %>%
  select(CVE_AGEB,buffer) %>%
  st_as_sf()

agebs_marginados_unidades  <- st_join(agebs_marginados_buffers,datos_denue,join = st_intersects)

agebs_marginados_unidades  <- agebs_marginados_unidades  %>%
  as.data.frame() %>%
  select(CVE_AGEB,id,alimento_convencional)

write.csv(agebs_marginados_unidades, "Datos/processed/agebs_marginados_unidades.csv")

marginados_unidades_count <- agebs_marginados_unidades  %>% 
  group_by(CVE_AGEB) %>%
  summarise(unidades_totales = n(),
            unidades_saludables = sum(alimento_convencional)) %>%
  mutate(proporcion_saludable = round(unidades_saludables / unidades_totales, 2))

table(marginados_unidades_count$proporcion_saludable)

barplot(table(agebs_denue_count$unidades_totales), 
     las =2,
     main = "Cantidad de AGEBS según su \naccesibilidad a alimentos",
     xlab = "Cantidad de unidades económicas minoristas de alimento alrededor del AGEB",
     ylab = "Frecuencia")

barplot(table(agebs_denue_count$proporcion_saludable), 
        las =2,
        main = "Cantidad de AGEBS según su \naccesibilidad a alimentos saludables",
        xlab = "Proporción de unidades económicas saludables alrededor del AGEB",
        ylab = "Frecuencia")

agebs_marginados <- agebs_marginados %>%
  merge(marginados_unidades_count, by = 'CVE_AGEB')

dir.create("Datos/processed/agebs_marginados")
write_sf(agebs_marginados, "Datos/processed/agebs_marginados/agebs_marginados.shp")

pal <- colorNumeric("YlOrRd", agebs_nl$unidades_totales,reverse = TRUE)

map <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = agebs_nl, fillColor = ~pal(unidades_totales),
              stroke = TRUE, opacity = 1,
              fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
              popup = ~paste("<strong>AGEB:</strong>",agebs_nl$cve_ageb,
                             "<br><strong>Unidades económicas <br>cercanas:</strong>",agebs_nl$unidades_totales))
map

pal <- colorNumeric("YlOrRd", agebs_nl$proporcion_saludable,reverse = TRUE)

map <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = agebs_nl, fillColor = ~pal(proporcion_saludable),
              stroke = TRUE, opacity = 1,
              fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
              popup = ~paste("<strong>AGEB:</strong>",agebs_nl$cve_ageb,
                             "<strong>MUN:</strong>",agebs_nl$nom_agem,
                             "<br><strong>GMU:</strong>",agebs_nl$GM_2020,
                             "<br><strong>Unidades económicas <br>cercanas:</strong>",agebs_nl$unidades_totales,
                             "<br><strong>Opciones saludables <br>cercanas:</strong>",agebs_nl$unidades_saludables,
                             "<br><strong>Accesibilidad <br>alimento saludable:</strong>",agebs_nl$proporcion_saludable))
map
saveWidget(map, file = "Datos/processed/accesibilidad_alimentaria.html")

#test <- agebs_nl %>% filter(!st_intersects(buffers_acceso, sparse = FALSE)) 

agebs_desiertos <- agebs_nl %>% filter(row_number() %in% which(conteo_acceso==0))
agebs_desiertos <- agebs_desiertos %>% filter(GM_2020 %in% c("Medio", "Alto", "Muy alto"))

map_desiertos <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = agebs_marginados, fillColor = "red",
              stroke = TRUE, opacity = 1,
              fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
              popup = ~paste("<strong>AGEB:</strong>",agebs_marginados$cve_ageb,
                             "<br><strong>GMU:</strong>",agebs_marginados$GM_2020,
                             "<br><strong>Municipio:</strong>",agebs_marginados$nom_agem))

map_desiertos

# NUMERO DE UNIDADES ECONÓMICAS POR HABITANTES
# ESTRATIFICAR 
View(agebs_nl[is.na(agebs_nl$cve_agee),])
