library(igraph)

# ==================================
# PageRank computation on a Graph
# ==================================

# Adjacency matrix for the example graph
adjMat <- rbind(
  c(0,1,1,0,0,0,0),
  c(1,0,1,1,0,0,0),
  c(1,1,0,0,0,0,0),
  c(0,1,0,0,1,1,1),
  c(0,0,0,1,0,1,0),
  c(0,0,0,1,1,0,1),
  c(0,0,0,1,0,1,0))

# Creating a graph object from the adjacency matrix
g  <- graph.adjacency(adjMat, mode = "undirected")
plot(g)

# Pretty visualization using plot function
plot(g, layout = layout.fruchterman.reingold,
     vertex.size = 25,
     vertex.color="red",
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,  
     edge.color="black")

# Compute the Transition Matrix for Random Walk
transMat <- t(adjMat / rowSums(adjMat))
transMat

# Compute the principal eigenvector of transMat
transEig <- eigen(transMat)
pageRank <- transEig$vec[,1]
pageRank

# Normalized PageRank
npageRank <- pageRank / sum(pageRank)
npageRank

# Represent PageRank as size and color of vertices
V(g)$size <- 25 + 100 * round(npageRank[V(g)], 2)
V(g)$color <- 100 * round(npageRank[V(g)], 2) + 1

plot(g, layout = layout.fruchterman.reingold,
     vertex.size = V(g)$size,
     vertex.color= V(g)$color,
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,  
     edge.color="black")


# Shortcut : Use a stock pagerank function in R
npageRank <- page.rank(g)
npageRank

V(g)$size <- 25 + 100 * round(npageRank$vector[V(g)], 2)
V(g)$color <- 100 * round(npageRank$vector[V(g)], 2) + 1

plot(g, layout = layout.fruchterman.reingold,
     vertex.size = V(g)$size,
     vertex.color= V(g)$color,
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,  
     edge.color="black")


# Example graph: Zachary's Karate Club
g <- make_graph("Zachary")

plot(g, layout = layout.fruchterman.reingold,
     vertex.size = 10,
     vertex.label = NA,
     vertex.color="red",
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,  
     edge.color="black")

npageRank <- page.rank(g)
npageRank

V(g)$size <- 10 + 100 * round(npageRank$vector[V(g)], 2)
V(g)$color <- 100 * round(npageRank$vector[V(g)], 2) + 1

plot(g, layout = layout.fruchterman.reingold,
     vertex.size = V(g)$size,
     vertex.color= V(g)$color,
     vertex.label = NA,
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,  
     edge.color="black")



# ==================================
# Partitioning/Clustering in a Graph
# ==================================

# Adjacency matrix for the example graph
adjMat <- rbind(
  c(0,1,1,0,0,0,0),
  c(1,0,1,1,0,0,0),
  c(1,1,0,0,0,0,0),
  c(0,1,0,0,1,1,1),
  c(0,0,0,1,0,1,0),
  c(0,0,0,1,1,0,1),
  c(0,0,0,1,0,1,0))

# Creating a graph object from the adjacency matrix
g  <- graph.adjacency(adjMat, mode = "undirected")

plot(g, layout = layout.fruchterman.reingold,
     vertex.size = 25,
     vertex.color="red",
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,  
     edge.color="black")


# Create the Laplacian Matrix of the example graph
# This is the naive way -- produces a dense matrix
degMat <- diag(degree(g))
lapMat <- degMat - adjMat
lapMat

# There is a stock function for Laplacian Matrix
# which produces a nice sparse matrix (prefered)
lapMat <- laplacian_matrix(g)
lapMat

# Compute the eigenvalues of Laplacian Matrix
# Spectral Decomposition (try ?eigen to know more)
lapEig <- eigen(lapMat)
lapEig

# Smallest eigenvalue and corresponding eigenvector
# Notice that the minimum eigenvalue is 0 (approx)
# Not much meaningful in sense of partitioning
lapEig$values[ncol(lapEig$vectors)]
lapEig$vectors[,ncol(lapEig$vectors)]

# Second Smallest eigenvalue and corresponding eigenvector
# Most useful to start partitioning the graph
lapEig$values[ncol(lapEig$vectors) - 1]
lapEig$vectors[,ncol(lapEig$vectors) - 1]
snzEvec <- lapEig$vectors[,ncol(lapEig$vectors) - 1]

# Identify nodes based on +ve and -ve values
fCom <- which(snzEvec > 0)
sCom <- which(snzEvec < 0)

# Color nodes based on the above identification
V(g)$color <- ifelse(V(g) %in% fCom, "red", "blue")

plot(g, layout = layout.fruchterman.reingold,
     vertex.size = 25,
     vertex.color=V(g)$color,
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,  
     edge.color="black")



# Shortcut : Use a stock clustering function in R
fc <- cluster_fast_greedy(g)
V(g)$color <- ifelse(membership(fc)==1,"red","blue")

plot(g, layout = layout.fruchterman.reingold,
     vertex.size = 25,
     vertex.color=V(g)$color,
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,  
     edge.color="black")


# Example graph: Zachary's Karate Club
g <- make_graph("Zachary")

plot(g, layout = layout.fruchterman.reingold,
     vertex.size = 10,
     vertex.label = NA,
     vertex.color="red",
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,  
     edge.color="black")

fc <- cluster_fast_greedy(g)
V(g)$color <- ifelse(membership(fc)==1,"red","blue")

plot(g, layout = layout.fruchterman.reingold,
     vertex.size = 10,
     vertex.label = NA,
     vertex.color=V(g)$color,
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,  
     edge.color="black")



# ==================================
# More tools in the igraph package
# ==================================

# Undirected graph by specifying edges
g <- graph( c(1,2,1,3,1,4,3,4,3,5,5,6,6,7,7,8,8,9,3,8,5,8), directed = FALSE )

plot(g, layout = layout.fruchterman.reingold,
     vertex.size = 25,
     vertex.color="red",
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,  
     edge.color="black")

# Directed graph by specifying edges
g <- graph( c(1,2,1,3,1,4,3,4,3,5,5,6,6,7,7,8,8,9,3,8,5,8), directed = TRUE )

plot(g, layout = layout.fruchterman.reingold,
     vertex.size = 25,
     vertex.color="red",
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=2,  
     edge.color="black")


# Assign attributes to the graph
g$name    <- "Example Graph"
V(g)$name  <- toupper(letters[1:9])
V(g)$color <- sample(rainbow(9), 9, replace=FALSE)
E(g)$weight <- runif(length(E(g)), .5, 4)

plot(g, layout = layout.fruchterman.reingold, 
     main = g$name,
     vertex.label = V(g)$name,
     vertex.size = 25,
     vertex.color= V(g)$color,
     vertex.frame.color= "white",
     vertex.label.color = "white",
     vertex.label.family = "sans",
     edge.width=E(g)$weight, 
     edge.color="black")



# ==================================
# Random Graphs using igraph package
# ==================================

# Plot an Erdos-Renyi random graph
# Different color for each component
g <- erdos.renyi.game(100, 1/100)
com <- clusters(g)
V(g)$color <- com$membership + 1
plot(g, layout=layout.fruchterman.reingold, 
     vertex.size=5, vertex.label=NA,
     edge.width=5, margin=0)

# Degree distribution of Erdos-Renyi graphs
ddg <- degree.distribution(g)
plot(ddg, type="lines")

# Plot a Barabasi-Albert random graph
# Different color for each component
g <- barabasi.game(100, directed = FALSE)
plot(g, layout=layout.fruchterman.reingold, 
     vertex.size=5, vertex.label=NA,
     edge.width=5, margin=0)

# Degree distribution of Barabasi-Albert graphs
ddg <- degree.distribution(g)
plot(ddg, type="lines")


# Plot Erdos-Renyi random graphs with varying p
par(mfrow=c(3,3))
par(mar=c(0,0,0,0))
for(k in -4:4) {
  g <- erdos.renyi.game(100, 1/(100 - 10*k))
  com <- clusters(g)
  V(g)$color <- com$membership + 1
  avgEdges <- 100 / (100 - 10*k)
  plot(g, layout=layout.fruchterman.reingold, 
       vertex.size=5, vertex.label=NA, 
       vertex.color = V(g)$color, 
       edge.width=5, margin = 0)
  text(1, 1, format(round(avgEdges, 2), nsmall = 2), cex = 1.25, font = 2)
}
dev.off()

# Plot Barabasi-Albert random graphs with varying n
par(mfrow=c(3,3))
par(mar=c(0,0,0,0))
for(k in -4:4) {
  g <- barabasi.game(100 + 20*k, directed = FALSE)
  numNodes <- (100 + 20*k)
  plot(g, layout=layout.fruchterman.reingold, 
       vertex.size=5, vertex.label=NA, 
       edge.width=5, margin = 0)
  text(1, 1, numNodes, cex = 1.25, font = 2)
}
dev.off()

