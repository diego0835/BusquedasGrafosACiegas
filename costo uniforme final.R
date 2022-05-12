# busqueda de costo uniforme
#Diego Pando Vargas

#Clase cola, de tipo dataframe , que sirve para crear nuestra cola


# metodos para manejar la propia cola
colaO <- setRefClass("colaO", fields = list(cola = "data.frame"),
  methods = list(
    estaVacia = function() {
      if (nrow(cola) == 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    retornarCola = function() {
      if (estaVacia()) {
        return(NULL)
      }else{
        return(cola)
      }
    },
    desencolar = function() {
      if (estaVacia()) {
        return(NULL)
      } else {
        valor <- cola[c(1),]
        cola <<-cola[-c(1),]
        return(valor)
      }
    },
    ordenar = function(campoOrdenar){
      cola<<-cola[order(as.numeric(cola[[campoOrdenar]]),na.last = TRUE,method = c("radix"),decreasing = FALSE),]
    },
    encolar = function(elemento) {
      cola[nrow(cola) + 1,]<<-elemento
    },
    vaciarCola = function() {
      cola <<- cola[-c(1:nrow(cola)),]
    }
   
  )
)

# para agregar un nodo adyacente

nodo <- setRefClass("Nodo", fields = list(
  vecinos = "list",
  id = "character"),
  methods = list(
    agregarAdyacente = function(nodoAdyacente, peso) {
      vecinos[[nodoAdyacente]] <<- list(id = nodoAdyacente, peso = peso)
    }
  )
)

#Clase grafo para agregar Nodos y mostrar el grafo
grafo <- setRefClass("Grafo", fields = list(
  listNodos = "list",
  nombreArchivo = "character",
  NodosVisitados = "list",
  listNombreNodos = "list"),
  
  methods = list(
    agregarNodo = function(id) {
      if (is.null(listNombreNodos[[id]])) {
        listNombreNodos[[id]] <<- id
        listNodos[[id]] <<- nodo(id = id)
      }
    },
    getNodoAristas = function(id) {
      return(listNodos[[id]])
    },
   
    
    initGrafo = function(cabecerasEnArchivo) {
     

      dataFrameGrafo <- read.csv(nombreArchivo, sep = ",", header = cabecerasEnArchivo, stringsAsFactors = FALSE)
     
      #borramos todos los que tienen la columna 2 vacia
      
     
     
      dataFrameGrafo<-dataFrameGrafo[dataFrameGrafo[[2]]!="",]
      
      for (k in 1:length(dataFrameGrafo[[1]])) {
        agregarArista(as.character(dataFrameGrafo[[1]][[k]]), as.character(dataFrameGrafo[[2]][[k]]), dataFrameGrafo[[3]][[k]])
      }
      return(graph_from_data_frame(dataFrameGrafo, directed = TRUE))
    },
    plotGrafo = function(grafoDirigido) {
      
      dataFrameGrafo <- read.csv(nombreArchivo, sep = ",", header = TRUE, stringsAsFactors = FALSE)
      dataFrameGrafo<-dataFrameGrafo[dataFrameGrafo[[2]]!="",]
      plotGraph <- graph.data.frame(dataFrameGrafo, directed = grafoDirigido)
      plot(plotGraph, edge.label = paste(E(plotGraph)$peso, sep = ""))
    },
    
    getNombreNodos = function() {
      return(listNombreNodos)
    },
    
    agregarArista = function(inicio, fin, peso) {
      
      # Agregamos ambos nodos a la lista de nodos
      agregarNodo(inicio)
      agregarNodo(fin)
      listNodos[[inicio]]$agregarAdyacente(fin, peso)
  
    },
   
    
    costoUniforme = function(nodoInicio, nodosFinales) {
      cat("\n")
      cat("\n")
      cat("COSTO UNIFORME ")
      cat("\n")
      
      if (length(nodosFinales)==0){
        nodosFinales[[1]]<-""
      }
      NodosVisitados <<- list()
      
      #Se asigna un nodo temporal para que tenga formato
      cola<-colaO(cola=data.frame(nodo=character(),peso=numeric(),costo=numeric(),stringsAsFactors = FALSE))
      names(nodosFinales) <- nodosFinales
      cola$encolar(c("-",0,0))
      extracciones<-cola$desencolar()
      cola$encolar(c(nodoInicio,0,0))
      
      while (TRUE) {
        if (!is.null(nodosFinales[[extracciones$nodo]])) {
          valor <- nodosFinales[[extracciones$nodo]]
          nodosFinales[[extracciones$nodo]] <- NULL
          
          if (length(nodosFinales) == 0) {
            cola$vaciarCola()
          }
          cat("\n ")
          cat(paste("NODO SOLUCION : ", valor, sep = " "))
          cat("\n ")
          
        }
        cat("\n ")
        cat(paste("Extraccion[", extracciones$nodo,"][",extracciones$costo,"]" ,sep = " "))
        cat("\n ")
        if(!is.null(cola$retornarCola())){
        cat("COLA : ")
        cat("\n ")
        print(cola$retornarCola(),row.names = FALSE)
        cat("\n ")
        cat("--------------------------------")
        cat("\n ")
        }
        else{
          cat("COLA VACIA!!!!!!!!!")
        }
        if (is.null(cola$retornarCola()) | length(nodosFinales) == 0) {
          break
        }
        
        extracciones <- cola$desencolar()
        
        
        while(!is.null(NodosVisitados[[extracciones$nodo]])){
         
          cat(paste(" El nodo [", extracciones$nodo, "] ya esta en visitados, se descarta!", sep = " "))
          cat("\n ")
          cat("COLA:")
          cat("\n ")
          print(cola$retornarCola(),row.names = FALSE)
          extracciones <- cola$desencolar()
          cat("\n ")
          if (is.null(extracciones)) {
            break
          }
          if (is.null(NodosVisitados[[extracciones$nodo]])){
            
          }
          
        }
        
        if (is.null(extracciones)) {
          break
        }
        
        
        nodosAdyacentes <- getNodoAristas(extracciones$nodo)$vecinos
        numExtracciones<-as.numeric(extracciones$peso)
        for (nodo in nodosAdyacentes) {
          numNodo<-as.numeric(nodo$peso)
          cola$encolar(c(nodo$id,numNodo,(numExtracciones+numNodo)))
        }
        cola$ordenar("costo")
        NodosVisitados[[extracciones$nodo]] <<- "1"
      }
      if (length(nodosFinales) != 0 & !is.null(nodosFinales)) {
        if (nodosFinales[[1]]!=""){
          cat("\n ")
          cat("No se ha encontrado todos los nodos")
        }
        
      }
    }
  ))


grafo <- grafo(nombreArchivo = "grafoPrueba.csv")
grafo$initGrafo(TRUE)
grafo$plotGrafo(TRUE)
start_time<-Sys.time()
grafo$costoUniforme("a",list("z"))


end_time <- Sys.time()

data<-read.csv("grafoPrueba.csv",sep = ",")
data<-data[data[[2]]!="",]
dataIgraph<-graph_from_data_frame(data, directed = FALSE)

grados<-degree(dataIgraph,mode="out")
hijosPromedio<-round(mean(grados[grados>0]), digits = 0)
maxHijos<-max(degree(dataIgraph,mode="out"))
cat("---------------------------------")
cat("\n")
cat("El tiempo es :",end_time-start_time)
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