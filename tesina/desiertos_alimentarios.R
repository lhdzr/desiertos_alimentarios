library(sf)
library(leaflet)
library(tidyverse)
library(htmlwidgets)
source("tesina/funciones_descarga_inegi.R")
source("tesina/codigos_municipales_nl.R")
city_limits_directory <- "Datos/raw/limites_municipales/"
ageb_output_dir <- "Datos/raw/agebs"

# DEFINICIÓN DE DIRECTORIOS DE SALIDA
if(!dir.exists(city_limits_directory)) {
  dir.create(city_limits_directory)
}

if(!dir.exists(ageb_output_dir)) {
  dir.create(ageb_output_dir)
}

# LECTURA DATOS CENSALES 2020
censo <- read.csv("Datos/raw/ageb_mza_urbana_19_cpv2020_csv/ageb_mza_urbana_19_cpv2020/conjunto_de_datos/conjunto_de_datos_ageb_urbana_19_cpv2020.csv") %>%
  mutate(CVE_AGEB = paste0(ENTIDAD,sprintf("%03d",MUN),sprintf("%04d",LOC),AGEB)) %>%
  group_by(CVE_AGEB) %>%
  summarise(POB_IND = sum(as.integer(P3YM_HLI)),
            HOM_IND = sum(as.integer(P3YM_HLI_M)),
            MUJ_IND = sum(as.integer(P3YM_HLI_F)))


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
agebs_nl <- agebs_nl %>%
  mutate(CVE_AGEB = paste0(cve_agee,cve_agem,cve_loc,cve_ageb)) %>%
  left_join(IMU_2020,by="CVE_AGEB") %>% 
  mutate(GM_2020 = factor(GM_2020, 
                          levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"), 
                          ordered = TRUE),
         centroid = st_centroid(geometry),
         pobtot = as.numeric(pobtot),
         tvivhab = as.numeric(tvivhab))

  

##### DENUE
datos_denue <- read_sf("Datos/raw/INEGI_DENUE_12052023/INEGI_DENUE_12052023.shp") %>% 
  select(id,clee,nom_estab,raz_social,
         codigo_act,nombre_act,per_ocu,
         cve_ent,cve_mun,ageb,manzana,
         latitud,longitud,geometry)

Encoding(datos_denue$nom_estab) <- "latin1"
Encoding(datos_denue$nombre_act) <- "latin1"

# DEFINIR CUÁLES AGEBS SE CONSIDERAN SALUDABLES
datos_denue$alimento_convencional <- 1

# OXXOS Y SEVEN ELEVENS
datos_denue$alimento_convencional[datos_denue$raz_social %in% c("CADENA COMERCIAL OXXO SA DE CV",
                                          "CADENA COMERCIAL OXXO",
                                          "CADENA COMERCIAL OXXO SUCURSAL HACIENDADEL ANGEL SA DE CV",
                                          "7-ELEVEN MEXICO SA DE CV", 
                                          "7 ELEVEN MEXICO SA DE CV", 
                                          "SEVEN ELEVEN DE MEXICO")] <- 0

# oxxos_sevens <- datos_denue[datos_denue$raz_social %in% c("CADENA COMERCIAL OXXO SA DE CV",
#                                                           "CADENA COMERCIAL OXXO",
#                                                           "CADENA COMERCIAL OXXO SUCURSAL HACIENDADEL ANGEL SA DE CV",
#                                                           "7-ELEVEN MEXICO SA DE CV", 
#                                                           "7 ELEVEN MEXICO SA DE CV", 
#                                                           "SEVEN ELEVEN DE MEXICO"),]

# DULCERÍAS, PALETERÍAS, OTROS ALIMENTOS, MISCELÁNEAS
actividades_no_saludables <- c("461160","461170","461190","461110")
datos_denue$alimento_convencional[datos_denue$codigo_act %in% actividades_no_saludables] <- 0 



#buffer <- st_buffer(datos_denue$geometry,dist = 1000)

### UNIÓN DE DATOS AGEB Y DENUE
st_crs(agebs_nl) == st_crs(datos_denue)
datos_denue <- st_transform(datos_denue, crs = 4326)




# VISUALIZACIONES
####################################################################################3
# VISUALIZAR MARGINACIÓN Y UNIDADES ECONÓMICAS TOTALES
# MAPEAR AGEBS POR GRADO DE MARGINACIÓN

# pal <- colorFactor("YlOrRd", agebs_nl$GM_2020, ordered = TRUE)
# 
# map <- leaflet() %>% 
#   addProviderTiles("CartoDB.Positron") %>% 
#   addPolygons(data = agebs_nl, fillColor = ~pal(GM_2020),
#                                                 stroke = TRUE, opacity = 1,
#                                                 fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
#                                                 popup = ~paste("<strong>AGEB:</strong>",agebs_nl$cve_ageb,
#                                                                "<br><strong>GMU:</strong>",agebs_nl$GM_2020)) %>%
#   addCircleMarkers(datos_denue$longitud,
#                    datos_denue$latitud, 
#                    clusterOptions = markerClusterOptions(),
#                    popup = as.character(datos_denue$nom_estab)) 
# map
 


# GUARDAR MAPA COMO ARCHIVO HTML
# saveWidget(map, file = "Datos/processed/marginacion_nl.html")



# MAPA DE AGEBS DE NL CON UNIDADES ECONÓMICAS
# agebs_zmm <- makeAGEBMap(19,zmm_codes,.5) %>% 
#   addCircleMarkers(datos_denue$longitud,
#                    datos_denue$latitud, 
#                    clusterOptions = markerClusterOptions(),
#                    popup = as.character(datos_denue$nom_estab))
# agebs_zmm

###############################################################################

# GENERACIÓN DE BUFFER ALREDEDOR DE AGEBS
agebs_nl$buffer <- st_buffer(agebs_nl$centroid, dist = 1000)

buffers_agebs <- agebs_nl %>%
  as.data.frame() %>%
  select(CVE_AGEB,buffer) %>%
  st_as_sf()

# INTERSECCIÓN ENTRE BUFFERS DE AGEBS Y UNIDADES ECONÓMICAS (UEs)
agebs_denue <- st_join(buffers_agebs,datos_denue,join = st_intersects) %>%
  as.data.frame() %>%
  select(CVE_AGEB,id,alimento_convencional)

write.csv(agebs_denue, "Datos/processed/agebs_denue.csv")

agebs_denue_count <- agebs_denue  %>% 
  group_by(CVE_AGEB) %>%
  summarise(unidades_totales = n(),
            unidades_saludables = sum(alimento_convencional)) %>%
  mutate(proporcion_saludable = round(unidades_saludables / unidades_totales, 2))

# CONTABILIZACIÓN DE UEs por AGEB
agebs_nl <- agebs_nl %>%
  merge(agebs_denue_count, by = 'CVE_AGEB') %>%
  merge(censo, by = 'CVE_AGEB',all.x = TRUE)

prueba_agebs_nl <- as.data.frame(agebs_nl) %>%
  merge(agebs_denue_count, by = 'CVE_AGEB') %>%
  merge(censo, by = 'CVE_AGEB',all.x = TRUE) %>%
  select(-c(centroid,buffer)) %>%
  mutate(indice_acceso = case_when(
    GM_2020 == "Muy bajo" ~ 5,
    GM_2020 == "Bajo" ~ 4,
    GM_2020 == "Medio" ~ 3,
    GM_2020 == "Alto" ~ 2,
    GM_2020 == "Muy alto" ~ 1
  ),indice_acceso = indice_acceso * 0.5,
  indice_acceso = ifelse(is.na(proporcion_saludable),indice_acceso, indice_acceso + proporcion_saludable))
  st_as_sf()

# CREAR DIRECTORIO DE DATAFRAME CON ÍNDICE DE ACCESO
agebs_nl_indice_path <- "Datos/processed/agebs_nl_indice"
if(!dir.exists(agebs_nl_indice_path)) {
  dir.create(agebs_nl_indice_path)
}


# GENERACIÓN DE INDICE DE ACCESIBILIDAD
agebs_nl <- agebs_nl %>%
  mutate(indice_acceso = case_when(
    GM_2020 == "Muy bajo" ~ 5,
    GM_2020 == "Bajo" ~ 4,
    GM_2020 == "Medio" ~ 3,
    GM_2020 == "Alto" ~ 2,
    GM_2020 == "Muy alto" ~ 1
  ),indice_acceso = indice_acceso * 0.5,
  indice_acceso = ifelse(is.na(proporcion_saludable),indice_acceso, indice_acceso + proporcion_saludable))

prueba_agebs_nl <- prueba_agebs_nl 


write_sf(prueba_agebs_nl, "Datos/processed/agebs_nl_indice/agebs_nl_indice.shp")
agebs_nl <- read_sf("Datos/processed/agebs_nl_indice/agebs_nl_indice.shp")
names_agebs_nl <- c("CVE_AGEB","cve_agee","nom_agee","cve_agem",
                    "nom_agem","cve_loc","cvegeo","pobtot","pobmas",
                    "pobfem","tvivhab","cve_ageb","IM_2020","GM_2020",
                    "IMN_2020","unidades_totales","unidades_saludables",
                    "proporcion_saludable","POB_IND","HOM_IND","MUJ_IND",
                    "indice_acceso","buffer")
names(agebs_nl) <- names_agebs_nl

# FILTRO DE AGEBS MARGINADOS
agebs_marginados <- agebs_nl %>% filter(GM_2020 %in% c("Medio", "Alto", "Muy alto"))
#dir.create("Datos/processed/agebs_marginados_indice")
#write_sf(agebs_marginados, "Datos/processed/agebs_marginados_indice/agebs_marginados_indice.shp")

agebs_desiertos <- agebs_marginados[agebs_marginados$unidades_saludables==0,]
agebs_pantanos <- agebs_marginados[agebs_marginados$proporcion_saludable<0.5,]
agebs_oasis <- agebs_nl %>% 
  filter(GM_2020 %in% c("Bajo", "Muy bajo")) %>%
  filter(proporcion_saludable>0.5)


# VISUALIZACIONES

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




# pal <- colorNumeric("YlOrRd", agebs_nl$unidades_totales,reverse = TRUE)
# 
# map <- leaflet() %>% 
#   addProviderTiles("CartoDB.Positron") %>% 
#   addPolygons(data = agebs_nl, fillColor = ~pal(unidades_totales),
#               stroke = TRUE, opacity = 1,
#               fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
#               popup = ~paste("<strong>AGEB:</strong>",agebs_nl$cve_ageb,
#                              "<br><strong>Unidades económicas <br>cercanas:</strong>",
#                              agebs_nl$unidades_totales))
# map


# MAPA DE ÍNDICE DE ACCESO ALIMENTARIO
pal <- colorNumeric("YlOrRd", agebs_nl$indice_acceso,reverse = TRUE)

indice_map <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = agebs_nl, fillColor = ~pal(indice_acceso),
              stroke = TRUE, opacity = 1,
              fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
              popup = ~paste("<strong>AGEB:</strong>",agebs_nl$cve_ageb,
                             "<strong>MUN:</strong>",agebs_nl$nom_agem,
                             "<br><strong>Grado de Marginación:</strong>",agebs_nl$GM_2020,
                             "<br><strong>Unidades económicas <br>cercanas:</strong>",agebs_nl$unidades_totales,
                             "<br><strong>Opciones saludables:</strong>",agebs_nl$unidades_saludables,
                             "<br><strong>Índice de acceso:</strong>",agebs_nl$indice_acceso)) %>%
  addLegend(position = "bottomright", pal = pal, values = agebs_nl$indice_acceso, 
            title = "Acceso a alimento saludable", opacity = 1)
indice_map



# pal <- colorNumeric("YlOrRd", agebs_nl$POB_IND,reverse = TRUE)
# 
# map <- leaflet() %>% 
#   addProviderTiles("CartoDB.Positron") %>% 
#   addPolygons(data = agebs_nl, fillColor = ~pal(POB_IND),
#               stroke = TRUE, opacity = 1,
#               fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
#               popup = ~paste("<strong>AGEB:</strong>",agebs_nl$cve_ageb,
#                              "<strong>MUN:</strong>",agebs_nl$nom_agem,
#                              "<br><strong>Grado de Marginación:</strong>",agebs_nl$GM_2020,
#                              "<br><strong>Unidades económicas <br>cercanas:</strong>",agebs_nl$unidades_totales,
#                              "<br><strong>Opciones saludables:</strong>",agebs_nl$unidades_saludables,
#                              "<br><strong>Índice de acceso:</strong>",agebs_nl$indice_acceso,
#                              "<br><strong>Población Indígena:</strong>",agebs_nl$POB_IND)) %>%
#   addLegend(position = "bottomright", pal = pal, values = agebs_nl$POB_IND, 
#             title = "Población Indígena", opacity = 1)
# map

saveWidget(indice_map, file = "Datos/processed/accesibilidad_alimentaria.html")

#test <- agebs_nl %>% filter(!st_intersects(buffers_acceso, sparse = FALSE)) 

# agebs_desiertos <- agebs_nl %>% filter(row_number() %in% which(conteo_acceso==0))
# 
# agebs_desiertos <- agebs_desiertos %>% filter(GM_2020 %in% c("Medio", "Alto", "Muy alto"))

# MAPA DE DESIERTOS ALIMENTARIOS
map_desiertos <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = agebs_desiertos, fillColor = "red",
              stroke = TRUE, opacity = 1,
              fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
              popup = ~paste("<strong>AGEB:</strong>",agebs_desiertos$cve_ageb,
                             "<br><strong>GMU:</strong>",agebs_desiertos$GM_2020,
                             "<br><strong>Municipio:</strong>",agebs_desiertos$nom_agem))

map_desiertos

# MAPA DE PANTANOS ALIMENTARIOS
map_pantanos <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = agebs_pantanos, fillColor = "red",
              stroke = TRUE, opacity = 1,
              fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
              popup = ~paste("<strong>AGEB:</strong>",agebs_pantanos$cve_ageb,
                             "<br><strong>GMU:</strong>",agebs_pantanos$GM_2020,
                             "<br><strong>Municipio:</strong>",agebs_pantanos$nom_agem))

map_pantanos


# MAPA DE OASIS ALIMENTARIOS
map_oasis <- leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = agebs_oasis, fillColor = "green",
              stroke = TRUE, opacity = 1,
              fillOpacity = 0.8, color = "#BDBDC3", weight = .1,
              popup = ~paste("<strong>AGEB:</strong>",agebs_oasis$cve_ageb,
                             "<br><strong>GMU:</strong>",agebs_oasis$GM_2020,
                             "<br><strong>Municipio:</strong>",agebs_oasis$nom_agem))

map_oasis



# NUMERO DE UNIDADES ECONÓMICAS POR HABITANTES
# ESTRATIFICAR 
View(agebs_nl[is.na(agebs_nl$cve_agee),])

# Acceso a unidades económicas alimentarias según índice de marginación
lm_tot_imu <- lm(unidades_totales ~ IM_2020, data = agebs_nl)
summary(lm_tot_imu)
plot(agebs_nl$IM_2020,agebs_nl$unidades_totales,
     main = "Acceso a unidades económicas alimentarias\nsegún índice de marginación del AGEB",
     xlab = "Nivel socioeconómico",
     ylab = "Tiendas cercanas")
abline(lm_tot_imu)


# Acceso a unidades económicas alimentarias saludables según índice de marginación
lm_sal_imu <- lm(unidades_saludables ~ IM_2020, data = agebs_nl)
summary(lm_sal_imu)
plot(agebs_nl$IM_2020,agebs_nl$unidades_saludables,
     main = "Acceso a tiendas de alimento saludable \nsegún índice de marginación por AGEB",
     xlab = "Nivel socioeconómico",
     ylab = "Tiendas de alimento saludable cercanas")
abline(lm_sal_imu)

# Proporción de opciones saludables según índice de marginación
lm_prop_imu <- lm(proporcion_saludable ~ IM_2020, data = agebs_nl)
summary(lm_prop_imu)
plot(agebs_nl$IM_2020,agebs_nl$proporcion_saludable,
     main = "Acceso a alimentos saludables según\n índice de marginación por AGEB",
     xlab = "Nivel socioeconómico",
     ylab = "Proporción de opciones saludables")
abline(lm_prop_imu)



# EFECTO DE POBLACIÓN TOTAL Y VIVIENDAS HABITADAS
lm_blight <- lm(indice_acceso~pobtot+tvivhab, data = agebs_nl)
summary(lm_blight)

# REGRESIÓN DE ACCESO ALIMENTARIO SOBRE NÚMERO DE VIVIENDAS HABITADAS
lm(indice_acceso~tvivhab, data = agebs_nl)
summary(lm(indice_acceso~tvivhab, data = agebs_nl))
plot(agebs_nl$tvivhab, agebs_nl$indice_acceso)
abline(lm(indice_acceso~tvivhab, data = agebs_nl))

# REGRESIÓN DE UNIDADES TOTALES SOBRE NÚMERO DE VIVIENDAS HABITADAS
lm(unidades_totales~tvivhab, data = agebs_nl)
summary(lm(unidades_totales~tvivhab, data = agebs_nl))
plot(agebs_nl$tvivhab, agebs_nl$unidades_totales)
abline(lm(unidades_totales~tvivhab, data = agebs_nl))

lm(unidades_totales~poly(tvivhab,4), data = agebs_nl)
summary(lm(unidades_totales~poly(tvivhab,2), data = agebs_nl))
plot(agebs_nl$tvivhab, agebs_nl$unidades_totales)
abline(lm(unidades_totales~poly(tvivhab,2), data = agebs_nl))

# REGRESIÓN DE ACCESO ALIMENTARIO SOBRE POBLACIÓN TOTAL
lm(indice_acceso~pobtot, data = agebs_nl)
summary(lm(indice_acceso~pobtot, data = agebs_nl))
plot(agebs_nl$pobtot, agebs_nl$indice_acceso)
abline(lm(indice_acceso~pobtot, data = agebs_nl))

summary(lm_tot_imu)
summary(lm_sal_imu)
summary(lm_prop_imu)

# REGRESIÓN PROPORCIÓN SALUDABLE SOBRE VIVIENDAS HABITADAS
plot(agebs_nl$proporcion_saludable,agebs_nl$tvivhab)
abline(lm(tvivhab ~ proporcion_saludable, data = agebs_nl))

# REGRESIÓN DE ACCESO ALIMENTARIO SOBRE NÚMERO DE VIVIENDAS HABITADAS
plot(agebs_nl$tvivhab,agebs_nl$proporcion_saludable)
abline(lm(proporcion_saludable ~ tvivhab, data = agebs_nl))

# UNIDADES PER CAPITA INDICE DE MARGINACION
# CREAR UNIDADES PER CAPITA
# CAMBIAR A NA LOS VALORES DE POBTOT == 0 PARA NO GENERAR VALORES INFINITOS
agebs_nl$pobtot <- ifelse(agebs_nl$pobtot==0,NA,agebs_nl$pobtot)
# UNIDADES TOTALES ENTRE POBLACIÓN TOTAL
agebs_nl <- agebs_nl %>%
  mutate(uni_per_capita = unidades_totales/pobtot)
# ELIMINAR LOS VALORES POR ENCIMA DE 10
agebs_nl$uni_per_capita[agebs_nl$uni_per_capita>10] <- NA
# UNIDADES SALUDABLES ENTRE POBLACIÓN TOTAL
agebs_nl <- agebs_nl %>%
  mutate(uni_sal_per_capita = unidades_saludables/pobtot)

# UNIDADES PER CAPITA SOBRE NIVEL SOCIOECONÓMICO
lm_upc_imu <- lm(uni_per_capita ~ agebs_nl$IM_2020, data = agebs_nl)
#prueba <- lm(uni_per_capita ~ agebs_nl$IM_2020, data = agebs_nl)
summary(lm_upc_imu)
plot(agebs_nl$IM_2020, agebs_nl$uni_per_capita,
     main = "Unidades económicas per cápita y nivel sociodemográfico",
     xlab = "Nivel Socioeconómico",
     ylab = "Tiendas per cápita")
abline(lm_upc_imu)

lm_upc_imu_2 <- lm(uni_per_capita ~ agebs_nl$IM_2020 + I(agebs_nl$IM_2020^2), data = agebs_nl)
summary(lm_upc_imu_2)
plot(agebs_nl$IM_2020, agebs_nl$uni_per_capita,
     xlab = "Nivel Socioeconómico",
     ylab = "Tiendas per cápita")
abline(lm_upc_imu_2,col="red")

# PROPORTION AS DEPENDENT, MARGINALIZATION, (WITH AND WITHOUT) TOTAL STORES, AND TOTAL POP AS INDEPENDENT
# PROPORCIÓN SALUDABLE SOBRE NIVEL SOCIOECONÓMICO
lm_prop_ses <- lm(proporcion_saludable ~ IM_2020, data = agebs_nl)
summary(lm_prop_ses)
plot(agebs_nl$IM_2020, agebs_nl$proporcion_saludable)
abline(lm_prop_ses)

# PROPORCIÓN SOBRE UNIDADES TOTALES
lm_prop_tot <- lm(proporcion_saludable ~ unidades_totales, data = agebs_nl)
summary(lm_prop_tot)
plot(agebs_nl$unidades_totales,agebs_nl$proporcion_saludable)
abline(lm_prop_tot)

# PROPORCIÓN SOBRE POBLACIÓN TOTAL
lm_prop_pob <- lm(proporcion_saludable ~ pobtot, data = agebs_nl)
summary(lm_prop_pob)
plot(agebs_nl$pobtot,agebs_nl$proporcion_saludable,
     main = "Población indígena y acceso alimentario",
     xlab = "Población Indígena",
     ylab = "Índice de acceso alimentario")
abline(lm_prop_tot, col="red")

# ACCESO ALIMENTARIO SOBRE POBLACIÓN INDÍGENA
lm_acc_ind <- lm(indice_acceso ~ POB_IND, data = agebs_nl)
summary(lm_acc_ind)
plot(agebs_nl$POB_IND, agebs_nl$indice_acceso)
abline(lm_acc_ind)

# PROPORCIÓN SALUDABLE SOBRE POBLACIÓN INDÍGENA
lm_acc_ind <- lm(proporcion_saludable ~ POB_IND, data = agebs_nl)
summary(lm_acc_ind)
plot(agebs_nl$POB_IND, agebs_nl$proporcion_saludable)
abline(lm_acc_ind)

# NIVEL SOCIOECONÓMICO SOBRE POBLACIÓN INDÍGENA
lm_imu_ind <- lm(IM_2020 ~ POB_IND, data = agebs_nl)
summary(lm_imu_ind)
plot(agebs_nl$POB_IND, agebs_nl$IM_2020)
abline(lm_imu_ind)
