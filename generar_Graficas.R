library(ggplot2)
library(plotly)
library(htmlwidgets)
library(htmltools)

generar_Graficas <- function(datos, globais, locais, problemas){
  
  grafica <- function(dat, colors, shapes, titulo, tipo){
    # Debuxamos a gráfica distancia-tempo para solvers globais e incluímola no arquivo de saída
    if(tipo=="Tempo"){
      my_plot <- ggplot(dat,aes(x=erro_ps2,y=time,col=SOLVER,shape=SOLVER))
    }
    else{
      my_plot <- ggplot(dat,aes(x=erro_ps2,y=NRMSE,col=SOLVER,shape=SOLVER))
    }
    my_plot <- my_plot + geom_point(size=4)
    # Asignamos as cores e formas
    my_plot <- my_plot + scale_color_manual(values = colors)
    my_plot <- my_plot + scale_shape_manual(values = shapes)
    # Personalizamos os títulos
    my_plot <- my_plot + labs(title = titulo, x = 'Distancia', y = tipo)
    my_plot <- my_plot + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
                               axis.text=element_text(size=12),
                               axis.title=element_text(size=14))
  }
  
  # Creamos o directorio raiz se non existe
  directorio_raiz <- "Graficas"
  if (!file.exists(directorio_raiz)) {
    dir.create(directorio_raiz)
  }
  
  # Definir los colores y formas para cada valor de la columna 'grupo'
  colorsLocal<-c("orange","darkgreen","purple","navy","red")
  shapesLocal<-c(15:17,9:10)
  colorsGlobal<-colorsLocal[1:length(globais)]
  shapesGlobal<-shapesLocal[1:length(globais)]
  
  # Para amosar o título xeral unha vez
  flag2 <- TRUE
  
  # Representamos gráficas para cada problema
  for (i in 1:length(problemas)){
    
    flag <- eval(parse(text = paste0("graficas_problemas[['", problemas[i], "']]")))
    # Se non se quere representar ese problema pasamos a seguinte iteración
    if(!flag){
      next
    }
    
    # Amosamos o título se é a primeira vez
    if(flag2){
      cat("# Gráficas")
      cat("\n")
      flag2=FALSE
    }
    
    cat(paste("## Gráficas para o problema", problemas[i], sep=" "))
    
    # Creamos a estructura de directorios para 
    directorio_problema <- paste(directorio_raiz,problemas[i],sep="/")
    if (!file.exists(directorio_problema)) {
      dir.create(directorio_problema)
    }
    directorio_global <- paste(directorio_problema,"global",sep="/")
    if (!file.exists(directorio_global)) {
      dir.create(directorio_global)
    }
    directorio_local <- paste(directorio_problema,"local",sep="/")
    if (!file.exists(directorio_local)) {
      dir.create(directorio_local)
    }
    
    # Collemos os datos do problema e separamos en locais e globais
    dat=datos[datos$problema==problemas[i] & datos$SOLVER %in% locais,]
    dat2=datos[datos$problema==problemas[i] & datos$SOLVER %in% globais,]
    
    # Gráfica distancia-tempo para solvers globais
    # Asignamos as cores e formas
    current_colors <- colorsGlobal[match(unique(dat2$SOLVER), globais)]
    current_shapes <- shapesGlobal[match(unique(dat2$SOLVER), globais)]
    # Personalizamos os títulos
    titulo <- paste("Distancia-Tempo para solvers globais en", problemas[i], sep=" ")
    # Obtemos a gráfica
    my_plot <- grafica(dat2, current_colors, current_shapes, titulo, "Tempo")
    # Mostramola
    print(my_plot)
    # Facemos que a gráfica sexa interactiva e gardamola nun html
    my_plot<-ggplotly(my_plot)
    nome_arquivo <- paste0(problemas[i], "_distancia_tempo_global.html")
    nome_arquivo <- paste(directorio_global,nome_arquivo,sep="/")
    htmlwidgets::saveWidget(my_plot, nome_arquivo)
    
    # Gráfica distancia-tempo para solvers locais
    # Asignamos as cores e formas
    current_colors <- colorsLocal[match(unique(dat$SOLVER), locais)]
    current_shapes <- shapesLocal[match(unique(dat$SOLVER), locais)]
    # Personalizamos o título
    titulo <- paste("Distancia-Tempo para solvers locais en", problemas[i], sep=" ")
    # Obtemos a gráfica
    my_plot <- grafica(dat, current_colors, current_shapes, titulo, "Tempo")
    # Mostramola
    print(my_plot)
    # Facemos que a gráfica sexa interactiva e a gardamola nun html
    my_plot<-ggplotly(my_plot)
    nome_arquivo <- paste0(problemas[i], "_distancia_tempo_local.html")
    nome_arquivo <- paste(directorio_local,nome_arquivo,sep="/")
    htmlwidgets::saveWidget(my_plot, nome_arquivo)
    
    # Gráfica distancia-erro para solvers globais
    # Asignamos as cores e formas
    current_colors <- colorsGlobal[match(unique(dat2$SOLVER), globais)]
    current_shapes <- shapesGlobal[match(unique(dat2$SOLVER), globais)]
    # Personalizamos o título
    titulo <- paste("Distancia-Erro para solvers globais en", problemas[i], sep=" ")
    # Obtemos a gráfica
    my_plot <- grafica(dat2, current_colors, current_shapes, titulo, "Erro")
    # Mostramola
    print(my_plot)
    # Facemos que a gráfica sexa interactiva e a gardamos nun html
    my_plot<-ggplotly(my_plot)
    nome_arquivo <- paste0(problemas[i], "_distancia_erro_global.html")
    nome_arquivo <- paste(directorio_global,nome_arquivo,sep="/")
    htmlwidgets::saveWidget(my_plot, nome_arquivo)
    
    # Gráfica distancia-erro para solvers locais
    # Asignamos as cores e formas
    current_colors <- colorsLocal[match(unique(dat$SOLVER), locais)]
    current_shapes <- shapesLocal[match(unique(dat$SOLVER), locais)]
    # Personalizamos o título
    titulo <- paste("Distancia-Erro para solvers locais en", problemas[i], sep=" ")
    # Obtemos a gráfica
    my_plot <- grafica(dat, current_colors, current_shapes, titulo, "Erro")
    # Mostramola
    print(my_plot)
    # Facemos que a gráfica sexa interactiva e a gardamos nun html
    my_plot<-ggplotly(my_plot)
    nome_arquivo <- paste0(problemas[i], "_distancia_erro_local.html")
    nome_arquivo <- paste(directorio_local,nome_arquivo,sep="/")
    htmlwidgets::saveWidget(my_plot, nome_arquivo)
    
    # Gráfica distancia-erro para solvers globais con resultado 0
    dat2=dat2[dat2$solve_result_num==0,]
    if (nrow(dat2) == 0) {
      next
    }
    my_plot <- ggplot(dat2,aes(x=erro_ps2,y=fobj2,col=SOLVER,shape=SOLVER))
    my_plot <- my_plot + geom_point(size=4)
    # Asignamos as cores e formas
    current_colors <- colorsGlobal[match(unique(dat2$SOLVER), globais)]
    current_shapes <- shapesGlobal[match(unique(dat2$SOLVER), globais)]
    my_plot <- my_plot + scale_color_manual(values = current_colors)
    my_plot <- my_plot + scale_shape_manual(values = current_shapes)
    # Personalizamos o título
    titulo <- paste("Distancia-Erro para solvers globais con resultado 0 en", problemas[i], sep=" ")
    # Obtemos a gráfica
    my_plot <- grafica(dat2, current_colors, current_shapes, titulo, "Erro")
    # Mostramola
    print(my_plot)
    # Facemos que a gráfica sexa interactiva e a gardamos nun html
    my_plot<-ggplotly(my_plot)
    nome_arquivo <- paste0(problemas[i], "_distancia_erro_global_resultado0.html")
    nome_arquivo <- paste(directorio_global,nome_arquivo,sep="/")
    htmlwidgets::saveWidget(my_plot, nome_arquivo)
    
    cat("\\newpage")
  }
}
