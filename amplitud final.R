library(datastructures)
library(igraph)

#creamos una funcion para el metodo de amplitud
busquedaAmplitud = function(nodoInicio, nodosFinales,grafo) {

  visitados <<- list()
  #Se ingresa una lista para buscar todos los elementos de la lista dada
  #El nodo inicio es de tipo character
  cola <- queue()
  names(nodosFinales) <- nodosFinales
  extraccion <- ""
  cola<- insert(cola,nodoInicio)
  
  cat("\n")
  cat("\n")
  cat("AMPLITUD ")
  cat("\n")
  while (TRUE) {
    
    
    # si es que la extraccion esta en el vectir de los nodos finales
    if (!is.null(nodosFinales[[extraccion]])) {
      
      nodosFinales[[extraccion]] <- NULL
      
      
      if (length(nodosFinales) == 0) {
        cola<- NULL
      }
   
  
    }
    if(extraccion!= ""){
    
    cat("!!!!Se extrae: [",extraccion,"]")
    cat("\n ")
  
    cat("-------------------------------------------------------")
    cat("\n ")
    }
    # ver esto
    if (length(cola)==0|length(nodosFinales) == 0) {
     # print("entro aqui")
      break
    }
    extraccion <- pop(cola)
   
    
    while(!is.null(visitados[[extraccion]])){
      cat("\n ")
     
      cat(" El nodo [", extraccion, "] ya esta en visitados, se descarta!")
      cat("\n ")
     
      extraccion <- pop(cola)
      
      if (is.null(extraccion)) {
        break
      }
      
    }
    
  
    
   # revisar esto
    #vecinos = neighbors(grafo,extraccion)$name
    nuevo = neighbors(grafo,extraccion)$name
    #cat("Voy a tomar los adyacentes de :",extraccion)
    #cat('\n')
    #cat("los adyacentes son :",vecinos)
    #cat('\n')

    for (i in 1:length(nuevo)) {
     
        cola<- insert(cola,nuevo[i])
      
    }
    visitados[[extraccion]] <<- "1"
  }
 
 
  if (length(nodosFinales) != 0 & !is.null(nodosFinales)) {
    if (nodosFinales[[1]]!=""){
      cat("\n ")
      cat("Nodos no encontrados")
    }
    
  }
}



data<-read.csv("grafoPrueba.csv",sep = ",")
data<-data[data[[2]]!="",]
dataIgraph<-graph_from_data_frame(data, directed = FALSE)
grafo <- graph.data.frame(data, directed = FALSE)  # Crea igraph 
class(grafo)                                     # Clase del objeto
V(grafo)$name                                    # Nombres de los vértices
E(grafo)$weight                                  # Peso de las aristas
plot(grafo, edge.label = paste(E(grafo)$weight, sep = "")) # Gráfico de abajo
start_time <- Sys.time()
busquedaAmplitud("a",list("z"),grafo)
#busquedaAmplitud("a",list(""),grafo)
#busquedaAmplitud("a",list("a,e,z"),grafo)

end_time <- Sys.time()
cat("El tiempo es :",end_time - start_time,"\n")

grados<-degree(dataIgraph,mode="out")
hijosPromedio<-round(mean(grados[grados>0]), digits = 0)
maxHijos<-max(degree(dataIgraph,mode="out"))

cat("---------------------------------")
cat("\n")
cat("El numero de hijos promedio es :",maxHijos)
distancias <- distances(dataIgraph, "a")
nivelNodo<-distancias[, "z"] #Se extrae solo la columna en R
cat("\n")
cat("La profundidad del arbol es : ",nivelNodo)
cat("\n")
cat("Complejidad espacial : ",maxHijos^nivelNodo)
cat("\n")
cat("Complejidad temporal : ",maxHijos^nivelNodo)

