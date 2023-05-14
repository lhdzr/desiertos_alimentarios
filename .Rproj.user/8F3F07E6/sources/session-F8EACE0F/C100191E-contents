library(httr)
library(geojsonio)
library(sf)
library(leaflet)
library(tidyverse)


catalogo_municipios <- read.csv("Datos/raw/catun_municipio/AGEEML_20233131250803.csv", encoding = "latin1")


getStateCodes <- function(nombre_ent) {
  codes <- unique(catalogo_municipios$CVE_ENT[catalogo_municipios$NOM_ENT %in% nombre_ent])
  return(codes)
}

getMunCodes <- function(codigo_ent,nombres_mun=NULL){
  if(is.null(nombres_mun)){
    codes <- unique(catalogo_municipios$CVE_MUN[catalogo_municipios$CVE_ENT == codigo_ent])
  }else{
    codes <- unique(catalogo_municipios$CVE_MUN[(catalogo_municipios$CVE_ENT == codigo_ent) & (catalogo_municipios$NOM_MUN %in% nombres_mun)])
  }
  return(codes)
}

downloadCityLimits <- function(entidad,codigos_mun,output_dir){
  base_url <- "https://gaia.inegi.org.mx/wscatgeo/geo/mgem/"
  ent_str <- sprintf("%02d", entidad)
  
  for (code in codigos_mun) {
    # Formatear el código para que tenga tres dígitos
    code_str <- sprintf("%03d", code)
    
    # Pegar el url y el código municipal
    url <- paste0(base_url,ent_str,code_str)
    
    # Definir el nombre del archivo
    filename <- paste0(ent_str, code_str, ".geojson")
    filepath <- file.path(output_dir, filename)
    
    # Hacer el GET del url
    response <- GET(url)
    
    # Checar si la conexión fue exitosa
    if (status_code(response) == 200) {
      # Guardar la respuesta al archivo con el nombre definido anteriormente
      writeBin(rawToChar(content(response, as = "raw")), filepath)
      cat(paste0("Data downloaded successfully for code ", ent_str,code_str, ".\n"))
    } else {
      cat(paste0("Error downloading data for code ",ent_str, code_str, ". Status code:", status_code(response), "\n"))
    }
    
    # Los archivos se guardan con un espacio adicional que no permite su lectura.
    # Este código borra el espacio y vuelve a guardar el archivo.
    geojson_string <- readLines(filepath)
    geojson_string_trimmed <- trimws(geojson_string, "right")
    writeLines(geojson_string_trimmed, filepath)
  }
}

makeMunMap <- function(entidad,municipality_code_list,file_location) {
  ent_str <- sprintf("%02d", entidad)
  map <- leaflet() %>% addTiles()
  
  for (code in municipality_code_list) {
    code_str <- sprintf("%03d", code)
    # read the corresponding geojson file
    file_name <- paste0(file_location,ent_str, code_str, ".geojson")
    data <- read_sf(file_name)
    
    # add the geojson layer to the map with the municipality name as the tooltip
    map <- map %>% addPolygons(data = data, fillColor = "blue",
                               stroke = TRUE, weight = 1, opacity = 1,
                               fillOpacity = 0.5, color = "white",
                               label = data$nom_agem)
  }
  return(map)
}

downloadAGEBData <- function(entidad,codigos_mun,output_dir){
  ent_str <- sprintf("%02d",entidad)
  agebs_base_url <- "https://gaia.inegi.org.mx/wscatgeo/geo/agebu/"
  
  for (code in codigos_mun) {
    code_str <- sprintf("%03d", code)
    url <- paste0(agebs_base_url,ent_str,"/",code_str)
    print(url)
    response <- GET(url)
    filename <- paste0("ageb",ent_str,code_str,".geojson")
    filepath <- file.path(output_dir, filename)
    print(filepath)
    
    if (status_code(response) == 200) {
      # Guardar la respuesta al archivo con el nombre definido anteriormente
      writeBin(rawToChar(content(response, as = "raw")), filepath)
      cat(paste0("Data downloaded successfully for code ", code_str, ".\n"))
    } else {
      cat(paste0("Error downloading data for code ", code_str, ". Status code:", status_code(response_prueba), "\n"))
    }
    
    geojson_string <- readLines(filepath)
    geojson_string_trimmed <- trimws(geojson_string, "right")
    writeLines(geojson_string_trimmed, filepath)
  }
}

makeAGEBMap <- function(entidad,mun_codes,fill_opacity){
  ent_str <- sprintf("%02d",entidad)
  data_completo <- data.frame()
  
  for (code in mun_codes) {
    mun_str <- sprintf("%03d",code)
    filepath <- paste0('Datos/raw/agebs/ageb',ent_str,mun_str,'.geojson')
    data <- read_sf(filepath)
    data_completo <- rbind(data_completo,data)
  }
  #%>% addTiles()
  map <- leaflet() %>%  addProviderTiles("CartoDB.Positron") %>% addPolygons(data = data_completo, fillColor = "blue",
                                                                             stroke = TRUE, weight = .2, opacity = 1,
                                                                             fillOpacity = fill_opacity, color = "white",
                                                                             popup = data_completo$cve_ageb)
  return(map)
}