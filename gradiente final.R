library(igraph)

colaO <- setRefClass("colaO", fields = list(
  cola = "data.frame"),
  methods = list(
    desencolar = function() {
      if (estaVacia()) {
        return(NULL)
      } else {
        valor <- cola[c(1),]
        cola <<-cola[-c(1),]
        return(valor)
      }
    },
    retornarCola = function() {
      if (estaVacia()) {
        return(NULL)
      }else{
        return(cola)
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
    },
    estaVacia = function() {
      if (nrow(cola) == 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  )
)


nodo <- setRefClass("Nodo", fields = list(
  adyacentes = "list",
  id = "character"),
  methods = list(
    agregarAdyacente = function(nodoAdyacente, peso,heuristica) {
      adyacentes[[nodoAdyacente]] <<- list(id = nodoAdyacente, peso = peso, heuristica=heuristica)
    },
    obtenerAdyacentes = function() {
      return(adyacentes)
    }
  )
)

#Grafo de una sola direcciÃ³n para todos los metodos excepto el bidireccional
grafoGene <- setRefClass("Grafo", fields = list(
  listNodos = "list",
  nombreArchivo = "character",
  listNodosVisitados = "list",
  listNombreNodos = "list",
  listaHeuristicas="list"),
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
    #Inicializa el grafo el parametro indica el orden si es FALSE entoces es ascendente 1,2,3 ...5 si es TRUE es descente 5,4,3,...1
    #Este parametro me sirve para ver si el archivo tiene cabeceras puede ser TRUE o FALSE
    initGrafo = function() {
      #inicializa el grafo
      listaHeuristicas<<-list()
      dataFrameGrafo <- read.csv(nombreArchivo, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  
      #Mando a limpiar los datos que no me sirve, las filas que solo contienen la heuristica del nodo
      
      for (j in 1:length(dataFrameGrafo[[1]])) {
        listaHeuristicas[[as.character(dataFrameGrafo[[1]][[j]])]]<<-dataFrameGrafo[[4]][[j]]
      }
     
      dataFrameGrafo<-dataFrameGrafo[dataFrameGrafo[[2]]!="",]
      
      for (k in 1:length(dataFrameGrafo[[1]])) {
        agregarArista(as.character(dataFrameGrafo[[1]][[k]]), as.character(dataFrameGrafo[[2]][[k]]), dataFrameGrafo[[3]][[k]],listaHeuristicas[[as.character(dataFrameGrafo[[2]][[k]])]])
      }
      return(graph_from_data_frame(dataFrameGrafo, directed = TRUE))
    },
    
    plotGrafo = function() {
      
      dataFrameGrafo <- read.csv(nombreArchivo, sep = ",", header = TRUE, stringsAsFactors = FALSE)
      dataFrameGrafo<-dataFrameGrafo[dataFrameGrafo[[2]]!="",]
      plotGraph <- graph.data.frame(dataFrameGrafo, directed = TRUE)
      V(plotGraph)$name                                    # Nombres de los vértices
      E(plotGraph)$weight                                  # Peso de las aristas
      #plot(plotGraph,layout=layout.reingold.tilford(plotGraph$peso,root=1)) # Gráfico de abajo
      
      plot(plotGraph, edge.label = paste(E(plotGraph)$peso,sep = ""))
      
    },
    getNombreNodos = function() {
      return(listNombreNodos)
    },
    
    agregarArista = function(inicio, fin, peso,heuristica) {

      
      agregarNodo(inicio)
      agregarNodo(fin)
      listNodos[[inicio]]$agregarAdyacente(fin, peso,heuristica)
    
    },
    
    getNodos = function() {
      return(listNombreNodos)
    },
    
    gradiente = function(nodoInicio, listNodosBuscar) {
      
      cat("\n")
      cat("\n")
      cat("GRADIENTE ")
      cat("\n")
      
      
      if (length(listNodosBuscar)==0){
        listNodosBuscar[[1]]<-""
      }
      listNodosVisitados <<- list()
      
      
      cola<-colaO(cola=data.frame(nodo=character(),heuristica=numeric(),stringsAsFactors = FALSE))
      names(listNodosBuscar) <- listNodosBuscar
      cola$encolar(c("--",0))
      extracciones<-cola$desencolar()
      cola$encolar(c(nodoInicio,listaHeuristicas[[nodoInicio]]))
      
      while (TRUE) {
        if (!is.null(listNodosBuscar[[extracciones$nodo]])) {
          valor <- listNodosBuscar[[extracciones$nodo]]
          listNodosBuscar[[extracciones$nodo]] <- NULL
          
          if (length(listNodosBuscar) == 0) {
            cola$vaciarCola()
          }
          cat("\n ")
          cat(paste("NODO SOLUCION !!! [", valor,"]", sep = " "))
          cat("\n ")
          
        }
        cat("\n ")
        cat(paste("Extraccion :", extracciones$nodo," heuristica: [",extracciones$heuristica,"]", sep = " "))
        cat("\n ")
        if(!is.null(cola$retornarCola())){
        cat("COLA ES :")
        cat("\n ")
        print(cola$retornarCola(),row.names = FALSE)
        cat("\n ")
        cat("----------------------------------")
        cat("\n ")
        }
        else{
          
          cat("COLA VACIAA !!!!!!")
        }
        if (is.null(cola$retornarCola()) | length(listNodosBuscar) == 0) {
          break
        }
        
        extracciones <- cola$desencolar()
        
        cola$vaciarCola()
        
        while(!is.null(listNodosVisitados[[extracciones$nodo]])){
          cat(paste("El nodo [", extracciones$nodo, "] ya esta en visitados,se descarta", sep = " "))
          cat("\n ")
          cat("COlA ES:")
          cat("\n ")
          print(cola$retornarCola(),row.names = FALSE)
          extracciones <- cola$desencolar()
          cat("\n ")
          if (is.null(extracciones)) {
            break
          }
        
          
        }
        
        if (is.null(extracciones)) {
          break
        }
        
        
        nodosAdyacentes <- getNodoAristas(extracciones$nodo)$adyacentes
        
        for (i in nodosAdyacentes) {
          numNodo<-as.numeric(i$heuristica)
          cola$encolar(c(i$id,(numNodo)))
        }
        cola$ordenar("heuristica")
        listNodosVisitados[[extracciones$nodo]] <<- "1"
      }
      if (length(listNodosBuscar) != 0 & !is.null(listNodosBuscar)) {
        if (listNodosBuscar[[1]]!=""){
          cat("\n ")
          cat("No se encontro todos los nodos")
        }
        
      }
    }
  )
  )

grafo <- grafoGene(nombreArchivo = "grafoPrueba.csv")
grafo$initGrafo()
grafo$plotGrafo()
start_time <- Sys.time()
grafo$gradiente("a",list("z"))
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
cat("Complejidad espacial : ",nivelNodo)
cat("\n")
cat("Complejidad temporal : ",nivelNodo)