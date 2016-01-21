library(igraph)
g = graph.tree(40,3,mode="undirected")
member = spinglass.community(g,spins=3)
V(g)$member = member$membership
mem.col = rainbow(3,alpha=0.3)
V(g)$color = mem.col[member$membership]
V(g)[1]$color = "red"
plot(g,vertex.color=V(g)$color)
#


