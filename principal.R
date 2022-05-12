library(igraph)
library(datastructures)
menu= function() {
  salir<-TRUE
  while(salir){
    opcion<-readline("Presione un boton: ")
    
  cat("----------------------------------------------\n")
  cat("APLICACIÓN DE BÚSQUEDAS A CIEGAS Y HEURÍSTICAS\n")
  cat("----------------------------------------------")
  cat("\n")
  
  cat("Menú de Opciones")
  cat("\n")
  cat("1. Búsqueda en amplitud\n")
  cat("2. Búsqueda en profundidad\n")
  cat("3. Búsqueda en profundidad iterativa\n")
  cat("4. Búsqueda en bidireccional\n")
  cat("5. Búsqueda de Costo Uniforme\n")
  cat("6. Búsqueda en Primero el mejor\n")
  cat("7. Búsqueda de el Gradiente\n")
  cat("8. Búsqueda de A Estrella\n")
  cat("9. TODOS\n")
  opcion<-readline("Ingrese opcion: ")
  opcion<- as.numeric(opcion)
  if(opcion==1){
    source("amplitud final.r")
    
  }
  
  if(opcion==2){
    source("profundidad final.r")
    
  }
  
  if(opcion==3){
    source("profundidad iterativa final.r")
    
  }
  if(opcion==4){
    source("bidireccional final.r")
    
  }
  if(opcion==5) {
    source("costo uniforme final.r")
    
  }
  if(opcion==6) {
    source("primero mejor final.r")
    
  }
  if(opcion==7) {
    source("gradiente final.r")
    
  }
  if(opcion==8) {
    source("a Estrella final.r")
    
  }
  if(opcion==9) {
    source("amplitud final.r")
    source("profundidad final.r")
    source("bidireccional final.r")
    source("costo uniforme final.r")
    source("primero mejor final.r")
    source("gradiente final.r")
    source("a Estrella final.r")
  }
  if(opcion==10){
   salir<-FALSE
    
  }
  }
}
menu()
