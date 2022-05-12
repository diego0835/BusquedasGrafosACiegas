
library(igraph)



colaG <- setRefClass("colaG", fields = list(
  cola = "list"),
  methods = list(
    retornarCola = function() {
      return(unlist(cola, use.names = FALSE))
    },
    desencolar = function() {
      if (estaVacia()) {
        return(NULL)
      } else {
        valor <- cola[[1]]
        cola[[1]] <<- NULL
        return(valor)
      }
    },
    estaVacia = function() {
      if (length(cola) == 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    encolar = function(elemento) {
      longitud <- length(cola) + 1
      cola[[longitud]] <<- elemento
    }
    
    
  )
)


busqueda = function(nodoInicio, nodoFin,grafo) {
  cat("\n")
  cat("\n")
  cat("BIDIRECCIONAL ")
  cat("\n")
  cola<-list()
  finalizar=0
  visitados <- list()
  extracciones <- list()
  
  #intersect nos devuelve la lista de caracteres que se intersectan  
  
  if (length(intersect(nodoInicio,nodoFin))!=0){
    cat("NO HAY NODOS/// NODO INICIAL Y FINAL ES EL MISMO")
  } else{
    nodoFin[[nodoInicio]]<-nodoInicio
    for (i in nodoFin){
      cola[[i]]<-colaG()
      cola[[i]]$encolar(i)
      visitados[[i]]<-list()
      extracciones[[i]]<-''
    }
    cont<-1
    # TERMINARA CUANDO FINALIZAR ==1
    while (finalizar==0) {
      cat("\n ")
      cat("--------------------------------------------------------------------------------------")
      cat("\n ")
      cat(cat("Contador:", cont, sep = " "))
      cat("\n ")
      
      for (nodo in nodoFin){
        if(nodo==nodoInicio){
          cat("\n ")
          cat("PARA EL NODO INICIAL [",nodo,"]")
          
        }else{
          cat("\n ")
          cat("PARA EL NODO [",nodo,"]")
        }
        
        #ENCONTRAR UN CAMINO COMUN
        cat("\n ")
        cat("Extraccion:",extracciones[[nodo]])
        cat("\n ")
        
        cat(paste("Cola del nodo [",nodo,"] es: ", sep = " "))
        cat(cola[[nodo]]$retornarCola(),sep = " ")
        cat("\n ")
        
        valor<-intersect(cola[[nodo]]$retornarCola(),cola[[nodoInicio]]$retornarCola())
        if (length(valor)!=0 & nodoInicio!=nodo ) {
          cat("\n ")
          cat("Cola del nodo Inicial: ",nodoInicio)
          cat("",cola[[nodoInicio]]$retornarCola())
          cat("\n ")
          cat("Cola de: ",nodo)
          cat(paste("",cola[[nodo]]$retornarCola()),sep = " ")
          cat("\n ")
          cat("CAMINO ENCONTRADO! ENTRE ",nodo,"y",nodoInicio," los elementos que coinciden: ")
          cat(paste(valor,sep = " "))
          cat("\n ")
          
          if (nodo!=nodoInicio){
            nodoFin<-NULL
          }
          
          
          finalizar<-1
          break()
        }
        
        
        if (is.null(cola[[nodo]]$retornarCola())) 
        {
          nodoFin<-NULL
        }
        
        if (length(nodoFin) == 1) 
        {
          cat(" entro aqui")
          finalizar=FALSE
          break
        }
        extracciones[[nodo]] <- cola[[nodo]]$desencolar()
        while(!is.null(visitados[[nodo]][[extracciones[[nodo]]]])){
          cat(paste("<El nodo", extracciones[[nodo]], "ya ha sido visitado, no se visitara!>", sep = " "))
          cat("\n ")
          extracciones[[nodo]] <- cola[[nodo]]$desencolar()
          cat(paste("cola actual del nodo",nodo,"es:", sep = " "))
          cat("\n ")
          if (is.null(extracciones)) {
            break
          }
        }
        if (is.null(extracciones)) {
          break
        }
        vecinos = neighbors(grafo,extracciones[[nodo]])$name
        
        for (i in 1:length(vecinos)) {
          cola[[nodo]]$encolar(vecinos[i])
          
          
        }
        
        visitados[[nodo]][[extracciones[[nodo]]]] <- "1"
        
      }
      cont<-cont+1
    }
  }
}



data<-read.csv("grafoPrueba.csv",sep = ",")

data<-data[data[[2]]!="",]
data
grafo <- graph.data.frame(data, directed = FALSE)  # Crea igraph 
class(grafo)                                     # Clase del objeto
V(grafo)$name                                    # Nombres de los vértices
E(grafo)$weight                                  # Peso de las aristas
#plot(grafo, edge.label = paste(E(grafo)$weight, sep = "")) # Gráfico de abajo
plot(grafo,layout=layout.reingold.tilford(grafo,root=1)) # Gráfico de abajo
# tiempo inicio del algoritmo
start_time <- Sys.time()
busqueda("a","z",grafo)
end_time <- Sys.time()


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
cat("Complejidad espacial : ",maxHijos^(nivelNodo/2))
cat("\n")
cat("Complejidad temporal : ",maxHijos^(nivelNodo/2))
