library(igraph)

colaO <- setRefClass("colaO", fields = list(
  cola = "data.frame"),
  methods = list(
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
    clean = function() {
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
grafo <- setRefClass("Grafo", fields = list(
  listNodos = "list",
  nombreArchivo = "character",
  listNodosVisitados = "list",
  listNombreNodos = "list",
  listaHeuristica="list"),
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
      listaHeuristica<<-list()
      dataFrameGrafo <- read.csv(nombreArchivo, sep = ",", header = TRUE, stringsAsFactors = FALSE)
      dataAux <- dataFrameGrafo[order(as.character(dataFrameGrafo[[1]]),as.character(dataFrameGrafo[[2]]),na.last = TRUE),]
      #Mando a limpiar los datos que no me sirve, las filas que solo contienen la heuristica del nodo
      
      
      #Limpio todas las que en la segunda columna no tengan nada
      
      for (j in 1:length(dataAux[[1]])) {
        listaHeuristica[[as.character(dataAux[[1]][[j]])]]<<-dataAux[[4]][[j]]
      }
      dataAux<-dataAux[dataAux[[2]]!="",]
      dataFrameGrafo<-dataFrameGrafo[dataFrameGrafo[[2]]!="",]
      for (k in 1:length(dataAux[[1]])) {
        agregarArista(as.character(dataAux[[1]][[k]]), as.character(dataAux[[2]][[k]]), dataAux[[3]][[k]],listaHeuristica[[as.character(dataAux[[2]][[k]])]])
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
      #Me agrega una arista para poder conectar los nodos
      
      agregarNodo(inicio)
      agregarNodo(fin)
      listNodos[[inicio]]$agregarAdyacente(fin, peso,heuristica)
    
    },
    
    getNodos = function() {
      return(listNombreNodos)
    },
    
    primeroMejor = function(nodoInicio, nodosFinales) {
      cat("\n")
      cat("\n")
      cat("PRIMERO MEJOR ")
      cat("\n")
      if (length(nodosFinales)==0){
        nodosFinales[[1]]<-""
      }
      listNodosVisitados <<- list()
      
  
      cola<-colaO(cola=data.frame(nodo=character(),heuristica=numeric(),stringsAsFactors = FALSE))
      names(nodosFinales) <- nodosFinales
      cola$encolar(c("--",0))
      extracciones<-cola$desencolar()
      cola$encolar(c(nodoInicio,listaHeuristica[[nodoInicio]]))
      
      while (TRUE) {
        if (!is.null(nodosFinales[[extracciones$nodo]])) {
          valor <- nodosFinales[[extracciones$nodo]]
          nodosFinales[[extracciones$nodo]] <- NULL
          
          if (length(nodosFinales) == 0) {
            cola$clean()
          }
          cat("\n ")
          cat(paste("NODO SOLUCION !!!! : ", valor, sep = " "))
          cat("\n ")
         
        }
        cat("\n ")
        cat(paste("Extraccion :[", extracciones$nodo,"], heuristica: [",extracciones$heuristica,"]" ,sep = " "))
        cat("\n ")
        cat("COLA :")
        cat("\n ")
        print(cola$retornarCola(),row.names = FALSE)
        cat("\n ")
        cat("-----------------------")
        cat("\n ")
        
        if (is.null(cola$retornarCola()) | length(nodosFinales) == 0) {
          break
        }
        
        extracciones <- cola$desencolar()
        
        
        while(!is.null(listNodosVisitados[[extracciones$nodo]])){
          cat(paste("El nodo [", extracciones$nodo, "]  ya esta en visitados, se descarta!", sep = " "))
          cat("\n ")
          cat("COLA:")
          cat("\n ")
          print(cola$retornarCola(),row.names = FALSE)
          cat("\n ")
          extracciones <- cola$desencolar()
          if (is.null(extracciones)) {
            break
          }
        
          
        }
        
        if (is.null(extracciones)) {
          break
        }
        
        
        nodosAdyacentes <- getNodoAristas(extracciones$nodo)$adyacentes
        
        for (nodo in nodosAdyacentes) {
          numNodo<-as.numeric(nodo$heuristica)
          cola$encolar(c(nodo$id,(numNodo)))
        }
        cola$ordenar("heuristica")
        listNodosVisitados[[extracciones$nodo]] <<- "1"
      }
      if (length(nodosFinales) != 0 & !is.null(nodosFinales)) {
        if (nodosFinales[[1]]!=""){
          cat("\n ")
          print("Paso")
          cat("No se ha encontrado todos los nodos")
        }
        
      }
    }
    
  )
  )

grafo <- grafo(nombreArchivo = "grafoPrueba.csv")
grafo$initGrafo()
grafo$plotGrafo()
grafo$primeroMejor("a",list("e"))

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
cat("Complejidad espacial : ",hijosPromedio^(nivelNodo))
cat("\n")
cat("Complejidad temporal : ",hijosPromedio^(nivelNodo))