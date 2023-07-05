library(rstudioapi)

# Archivo de configuracion
ruta_archivo_conf <- "configuracionGraficas.txt"

#' Funcion que transforma as liñas seleccionadas nun diccionario
#' 
#' @param tipo indica el tipo de visualizacion
#' @param num_linhas indica el numero de opciones que se pueden activar
diccionario <- function(tipo,num_linhas) {
  
  # Lemos o arquivo
  contido <- readLines(ruta_archivo_conf)
  
  # Buscamos as liñas relativas ao tipo
  inicio_taboas <- grep(tipo, contido)
  linhas_seguintes <- contido[(inicio_taboas + 1):(inicio_taboas + num_linhas)]
  
  # Separamos cada linha nas duas palabras que a compoñen
  preferencias <- sapply(strsplit(linhas_seguintes, " "), function(x) x[1])
  clave <- sapply(strsplit(linhas_seguintes, " "), function(x) x[2])
  
  # Transformamos a boolean
  preferencias <- ifelse(preferencias == "SI", TRUE, FALSE)
  
  # Xeramos o diccionario usando un data frame
  diccionario <- setNames(preferencias, clave)
  
  return(diccionario)
}

#' Funcion que devolve un diccionario coas taboas desexadas
taboas <- function() {
  taboas <- diccionario("#TABOAS", 7)
  return(taboas)
}

#' Funcion que devolve un diccionario coas taboas combinadas desexadas
taboas_combinadas <- function() {
  taboas_combinadas <- diccionario("#TABOAS_COMBINADAS", 6)
  return(taboas_combinadas)
}

#' Funcion que devolve un diccionario coas taboas de mellores tempos desexadas
taboas_mellores_tempos <- function() {
  taboas_mellores_tempos <- diccionario("#TABOAS_MELLORES_TEMPOS", 2)
  return(taboas_mellores_tempos)
}

#' Funcion que devolve un diccionario cos boxplots desexados
boxplots <- function() {
  box_plots <- diccionario("#BOX_PLOTS", 2)
  return(box_plots)
}

#' Funcion que devolve un diccionario coas graficas de problemas desexadas
graficas_problemas <- function() {
  graficas_problemas <- diccionario("#GRAFICAS_PROBLEMAS", 12)
  return(graficas_problemas)
}