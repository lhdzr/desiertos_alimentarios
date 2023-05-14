source("tesina/funciones_descarga_inegi.R")

# Vectores de nombres de municipios en la ZMM
center <- c("Apodaca","García","San Pedro Garza García",
            "General Escobedo","Guadalupe","Juárez",
            "Monterrey","San Nicolás de los Garza", "Santa Catarina")

outer <- c("Abasolo","Cadereyta Jiménez", "El Carmen",
           "Ciénega de Flores", "General Zuazua",
           "Pesquería","Salinas Victoria","Hidalgo",
           "Santiago")

# Códigos de los municipios de Nuevo León
nl_codes <- getMunCodes(19)
# Códigos de los municipios centrales
center_codes <- getMunCodes(19,center)
# Códigos de los municipios periféricos
outer_codes <- getMunCodes(19,outer)
# Lista completa de códigos municipales de la ZMM
zmm_codes <- c(center_codes, outer_codes)
# Códigos de los municipios no pertenecientes a la ZMM
non_zmm_codes <- nl_codes[!nl_codes %in% zmm_codes]

