# To read networks in any of the formats and return an igraph object

readEcoNetwork <- function(FileName) {
  web <- read.csv(FileName, header = T, check.names = F) # leer en formato .csv un data.frame
  if((ncol(web)-1) == nrow(web) ) {            # saber si es una matriz o una lista de interacciones
    g <- graph_from_adjacency_matrix(web)
  } else {                         # si no es una matriz de adyacencia, es una lista de interacciones
    web <- web[,c(2,1)]
    g <- graph_from_data_frame(web)
  }
    
  return(g) # Con el resultado
}

  # En la información de la función, aclarar qué tipo de datos

# Plot the network with trophic levels

plotEcoNetworkTrophLevel <- function(g){
  tl <- TrophInd(get.adjacency(g, sparse=F))
  
  lMat <-matrix(
    nrow=vcount(g),  
    ncol=2
  )
  
  lMat[,2]<-jitter(tl$TL,0.1)              
  lMat[,1]<-runif(vcount(g))   
  
  colTL <-as.numeric(cut(tl$TL,11))
  colnet <- brewer.pal(11,"RdYlGn")   
  V(g)$color <- colnet[12-colTL]
  
  plot(g, edge.width=.3,edge.arrow.size=.4,
       vertex.label=NA,
       edge.color="grey50",
       edge.curved=0.3, layout=lMat)  
  
}


# Calculate all the networks indices given an igraph object and return a data.frame

topologicalIndicesEcoNetwork <- function(g){

deg <- degree(g, mode="out") # calculate the in-degree: the number of predators  

V(g)$outdegree <-  deg # se lo agrega como variable 

nTop <- length(V(g)[outdegree==0]) # Top predators do not have predators

deg <- degree(g, mode="in") # calculate the in-degree: the number of preys

V(g)$indegree <-  deg

nBasal <- length(V(g)[indegree==0]) # Basal species do not have preys 

vcount(g)-nTop-nBasal # Intermediate species

size <- vcount(g) # número de vértices

links <- ecount(g) # número de interacciones

linkDen <- links/size          # Linkage density, densidad de interacciones promedio por especie

conn <- links/size^2           # Connectance, número de interacciones realizadas vs el número de posibles

pathLength <- average.path.length(g)   # Average path length 

clusCoef <- transitivity(g, type = "global")   # Clustering coefficient
  
g <- data.frame(size, links, linkDen,conn, pathLength, clusCoef)

return(g)
}