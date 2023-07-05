library(openxlsx)

# Directorio onde están os arquivos
directorio_arquivos <- "resultados_textoPlano"

# Nome dos arquivos en texto plano para cada problema
arquivo <- c("SAIDA_alphapinene.dat_procesada_conerros","SAIDA_BBG.dat_procesada_conerros",
          "SAIDA_FHN.dat_procesada_conerros",
          "SAIDA_harmonic.dat_procesada_conerros", "SAIDA_lotkaVolterraP.dat_procesada_conerros",
          "SAIDA_lotkaVolterraF.dat_procesada_conerros",
          "SAIDA_daisy_mamil3P.dat_procesada_conerros","SAIDA_daisy_mamil3F.dat_procesada_conerros",
          "SAIDA_hivP.dat_procesada_conerros","SAIDA_hivF.dat_procesada_conerros",
          "SAIDA_crausteP.dat_procesada_conerros","SAIDA_crausteF.dat_procesada_conerros"
)
arquivo <-  paste(directorio_arquivos,arquivo,sep="/")

# Numero de parametros a estimar en cada problema
ns <- c(5,4,3,4,5,5,8,8,15,15,18,18)

# Nomes dos problemas
nomes <- c("alpha_pinene","BBG","FHN","harmonic","lotkaVolterraP","lotkaVolterraF",
        "daisy_mamil3P","daisy_mamil3F","hivP","hivF","crausteP","crausteF")

# Identificador para as execucions sen error
aceptamos=c("solved","limit","solved?")

#' Funcion para xerar o excel a partir dos arquivos en texto plano de cada problema
#'
#' @param nomeExcel Nome do arquivo excel de saída
#' @param filtrarErros Booleano para filtrar os erros (TRUE) ou non (FALSE)
#'
xerarExcel <- function(nomeExcel, filtrarErros){
  
  if (!file.exists(nomeExcel)) {
    # Creamos un novo libro
    libro <- createWorkbook()
    
    # Creamos un diccionario para ir gardando os distintos parametros
    claves <- c("problemas", "solvers", "metodos", "modes", "PEN", "tol", "N")
    parametros <- sapply(claves, function(x) NULL) 
    
    # Para cada problema
    for (prob in 1:length(ns)) {
      # Agregar unha nova folla ao libro
      addWorksheet(wb=libro, sheetName=nomes[prob])
      
      # Ler o arquivo de texto plano
      dat=read.table(arquivo[prob], header=T, stringsAsFactors=TRUE)
      
      # Comprobar se está resolto
      dat$resolto="non"
      dat$resolto[dat$resolto1=="si" & dat$resolto2=="si" & dat$resolto3=="si" 
                  & dat$resolto4=="si"]="si"
      
      # Filtramos os erros se é necesario
      if(filtrarErros){
        dat=dat[dat$solve_result %in% aceptamos,]
      }
      
      # Escribir os datos na folla de Excel
      writeData(wb=libro, sheet=nomes[prob], x=dat)
      
      # Collemos os parametros
      parametros$problemas<-unique(c(parametros$problemas,nomes[prob]))
      parametros$solvers<-unique(c(parametros$solvers,levels(dat$SOLVER)))
      parametros$metodos<-unique(c(parametros$metodos,levels(dat$metodo)))
      parametros$modes<-unique(c(parametros$modes,dat$mode))
      parametros$PEN<-unique(c(parametros$PEN,dat$PEN))
      parametros$tol<-unique(c(parametros$tol,dat$tol))
      parametros$N<-unique(c(parametros$N,dat$N))
    }
    
    # Crear unha folla para os parametros
    addWorksheet(libro, "Parametros")
  
    # Escribir os datos na folla
    for (i in 1:length(parametros)) {
      # Obter a clave e os valores
      clave <- names(parametros)[i]
      valores <- parametros[[i]]
      
      # Escribir o nome da clave na primeira columna
      writeData(libro, sheet = "Parametros", x = clave, startRow = 1, startCol = i)
      
      # Escribir os valores na segunda columna
      writeData(libro, sheet = "Parametros", x = valores, startRow = 2, startCol = i)
    }
    
    # Gardamos o libro como un arquivo de Excel
    saveWorkbook(libro, nomeExcel)
  }
}

#' Funcion para xerar un arquivo plano con todos os casos
#'
#' @param nomeArquivo Nome do arquivo de saída
#' @param filtrarErros Booleano para filtrar os erros (TRUE) ou non (FALSE)
#'
agruparDatos <- function(nomeArquivo, filtrarErros){
  
  if (!file.exists(nomeArquivo)) {
    # Data frame onde gardar todos os datos
    datos=data.frame()
    
    # Para cada problema
    for (prob in 1:length(ns)) {
      
      # Ler o arquivo de texto plano
      dat=read.table(arquivo[prob],header=T,stringsAsFactors=TRUE)
      
      # Eliminamos os parametros
      n=ns[prob]
      dat=dat[,-(13:(12+n))]
      
      # Comprobar se está resolto
      dat$resolto="non"
      dat$resolto[dat$resolto1=="si" & dat$resolto2=="si" & dat$resolto3=="si" 
                  & dat$resolto4=="si"]="si"
      
      # Engadimos o nome do problema
      dat$problema=rep(nomes[prob],nrow(dat))
      
      # Filtramos os erros se é necesario
      if(filtrarErros){
        dat=dat[dat$solve_result %in% aceptamos,]
      }
      
      # Engadimos as novas filas ao data frame
      datos=rbind(datos,dat)
    }
    
    # Gardamos o data frame nun novo arquivo de texto plano
    write.table(datos, file = nomeArquivo, row.names = FALSE, quote = FALSE)
  }
  
}

