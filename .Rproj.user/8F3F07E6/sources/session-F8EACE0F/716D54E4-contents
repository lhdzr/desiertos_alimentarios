source("tesina/funciones_descarga_inegi.R")
source("tesina/codigos_municipales_nl.R")

city_limits_directory <- "Datos/raw/limites_municipales/"
ageb_output_dir <- "Datos/raw/agebs"

# Descarga de los archivos de límites municipales de municipios de Nuevo León
downloadCityLimits(19,nl_codes,city_limits_directory)
makeMunMap(19,nl_codes,city_limits_directory)
# Descarga de los datos de los AGEB de los municipios de Nuevo León
downloadAGEBData(19,nl_codes,ageb_output_dir)

