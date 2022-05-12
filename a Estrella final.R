library(igraph)




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
colaO <- setRefClass("colaO", fields = list(
  cola = "data.frame"),
  methods = list(
    
    estaVacia = function() {
      if (nrow(cola) == 0) {
        return(TRUE)
      } else {
        return(FALSE)
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
    retornarCola = function() {
      if (estaVacia()) {
        return(NULL)
      }else{
        return(cola)
      }
    },
    encolar = function(elemento) {
      cola[nrow(cola) + 1,]<<-elemento
    },
    clean = function() {
      cola <<- cola[-c(1:nrow(cola)),]
    }
  )
)


grafo <- setRefClass("Grafo", fields = list(
  listNodos = "list",
  nombreArchivo = "character",
  listNodosVisitados = "list",
  listNombreNodos = "list",
  heuristicaList="list"),
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

    initGrafo = function() {
      #inicializa el grafo
      heuristicaList<<-list()
      dataFrameGrafo <- read.csv(nombreArchivo, sep = ",", header = TRUE, stringsAsFactors = FALSE)
      
      #se eliminan los que en la columna 2 no tengan nombre
      
      for (j in 1:length(dataFrameGrafo[[1]])) {
        heuristicaList[[as.character(dataFrameGrafo[[1]][[j]])]]<<-dataFrameGrafo[[4]][[j]]
      }
    
      dataFrameGrafo<-dataFrameGrafo[dataFrameGrafo[[2]]!="",]
      
      for (k in 1:length(dataFrameGrafo[[1]])) {
        agregarArista(as.character(dataFrameGrafo[[1]][[k]]), as.character(dataFrameGrafo[[2]][[k]]), dataFrameGrafo[[3]][[k]],heuristicaList[[as.character(dataFrameGrafo[[2]][[k]])]])
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
    
    
    aEstrella = function(nodoInicio, nodosFinales) {
      
      cat("\n")
      cat("\n")
      cat("A ESTRELLA  ")
      cat("\n")
    
      if (length(nodosFinales)==0){
        nodosFinales[[1]]<-""
      }
      listNodosVisitados <<- list()
      
      #se asigna un nodo para dar formato
      cola<-colaO(cola=data.frame(nodo=character(),peso=numeric(),heuristica=numeric(),suma=numeric(),stringsAsFactors = FALSE))
      names(nodosFinales) <- nodosFinales
      cola$encolar(c("--",0,0,0))
      extracciones<-cola$desencolar()
      cola$encolar(c(nodoInicio,0,heuristicaList[[nodoInicio]],heuristicaList[[nodoInicio]]))
      
      while (TRUE) {
        if (!is.null(nodosFinales[[extracciones$nodo]])) {
          valor <- nodosFinales[[extracciones$nodo]]
          nodosFinales[[extracciones$nodo]] <- NULL
          
          if (length(nodosFinales) == 0) {
            cola$clean()
          }
          cat("\n ")
          cat(paste("NODO SOLUCION : ", valor, sep = " "))
          cat("\n ")
         
        }
        cat("\n ")
        cat(paste("Extraccion: [", extracciones$nodo,"] suma : [",extracciones$suma, "]",sep = " "))
        cat("\n ")
        cat("La cola es:")
        cat("\n ")
        print(cola$retornarCola(),row.names = FALSE)
        cat("\n ")
        cat("-----------------------------------------")
        cat("\n ")
        
        if (is.null(cola$retornarCola()) | length(nodosFinales) == 0) {
          break
        }
        
        extracciones <- cola$desencolar()
        
        while(!is.null(listNodosVisitados[[extracciones$nodo]])){
          cat(paste(" El nodo [", extracciones$nodo, "] ya esta en visitados, se descarta!", sep = " "))
          cat("\n ")
          cat("La cola  es:")
          cat("\n ")
          print(cola$retornarCola(),row.names = FALSE)
          extracciones <- cola$desencolar()
          cat("\n ")
          if (is.null(extracciones)) {
            break
          }
          if (is.null(listNodosVisitados[[extracciones$nodo]])){
            
          }
        }
        
        
        if (is.null(extracciones)) {
          break
        }
        
      
        
        
        nodosAdyacentes <- getNodoAristas(extracciones$nodo)$adyacentes
        
        
        for (nodo in nodosAdyacentes) {
          numHeuristica<-as.numeric(nodo$heuristica)
          numPeso<-as.numeric(nodo$peso)
          cola$encolar(c(nodo$id,numPeso,numHeuristica,(numHeuristica+numPeso)))
        }
        cola$ordenar("suma")
        
        listNodosVisitados[[extracciones$nodo]] <<- "1"
        
        
      }
      if (length(nodosFinales) != 0 & !is.null(nodosFinales)) {
        if (nodosFinales[[1]]!=""){
          cat("\n ")
          cat("TOdos los nodos no encontrados")
        }
      }
    }
  )
)

grafo <- grafo(nombreArchivo = "grafoPrueba.csv")
grafo$initGrafo()
grafo$plotGrafo()
start_time<-Sys.time()
grafo$aEstrella("a",list("z"))
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

