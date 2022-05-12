library(datastructures)
library(igraph)
busquedaProfundidadIterativa = function(nodoInicio, listNodosBuscar, nivelBusqueda) {
  if (length(listNodosBuscar)==0){
    listNodosBuscar[[1]]<-""
  }
  nivel<-0
  nivelBusqd<-list()
  listNodosVisitados <<- list()
  #Se ingresa una lista para buscar todos los elementos de la lista dada
  #El nodo inicio es de tipo character
  pila <- stack()
  names(listNodosBuscar) <- listNodosBuscar
  extracciones <- "--"
  nivelBusqd[[nodoInicio]]<-nodoInicio
  pila= insert(pila,nodoInicio)
 
  while (TRUE) {
    
    if (!is.null(listNodosBuscar[[extracciones]])) {
      valor <- listNodosBuscar[[extracciones]]
      listNodosBuscar[[extracciones]] <- NULL
      
      if (length(listNodosBuscar) == 0) {
        pila<- NULL
      }
      cat("\n ")
      cat(paste("--->El nodo encontrado es: ", valor, sep = " "))
      cat("\n ")
     
    }
    cat("\n ")
    cat(paste("La extracci√≥n es: ", extracciones, sep = " "))
    cat("\n ")
  
    cat(paste("El nodo de la extracci√≥n se encuentra en el nivel:",nivelBusqd[[extracciones]]$nivel, sep = " "))
    cat("\n ")
    cat("********************************************************************")
    cat("\n ")
    
    if (length(pila)==0 | length(listNodosBuscar) == 0) {
      break
    }
    
    extracciones <- pop(pila)
    #Es para poder obviar los que ya estan repetidos y no extraer de nuevo
    while(!is.null(listNodosVisitados[[extracciones]])){
      cat("\n ")
      cat(paste("<El nodo", extracciones, "ya ha sido visitado, no se visitara!>", sep = " "))
      cat("\n ")
      cat("La pila actual es: ", pila$look())
      cat("\n ")
      extracciones <- pila$pop()
      if (is.null(extracciones)) {
        break
      }
     
      
    }
    
    #Puede darse el caso de que la cola este vacia
    if (is.null(extracciones)) {
      break
    }
    
    
   
    if(nivelBusqd[[extracciones]]$nivel< nivelBusqueda){
      nodosAdyacentes <- getNodoAristas(extracciones)$adyacentes
      for (nodo in nodosAdyacentes) {
        pila$push(nodo$id)
        nivelBusqd[[nodo$id]]<-list(id=nodo$id, nivel=nivelBusqd[[extracciones]]$nivel+1)
        
      }
      listNodosVisitados[[extracciones]] <<- "1"
      nivel<-nivel+1
    }
  }
  
  
  if (length(listNodosBuscar) != 0 & !is.null(listNodosBuscar)) {
    if (listNodosBuscar[[1]]!=""){
      cat("\n ")
      cat("No se ha encontrado todos los nodos")
    }
    
  }
  
}



data<-read.csv("prueba.csv",sep = ",")

data<-data[data[[2]]!="",]
data
grafo <- graph.data.frame(data, directed = FALSE)  # Crea igraph 
class(grafo)                                     # Clase del objeto
V(grafo)$name                                    # Nombres de los vÈrtices
E(grafo)$weight                                  # Peso de las aristas
plot(grafo, edge.label = paste(E(grafo)$weight, sep = "")) # Gr·fico de abajo
busquedaProfundidadIterativa("A",list("J","I"),grafo)