###
# initial test of igraph
###

library(igraph)
library(plyr)


g2 <- graph( c(1,2,2,3,3,4,5,6), directed=TRUE )

plot(g2)

g3 <- graph(c(1:10,1:10,1:6),directed=FALSE)

el <- matrix( c("foo", "bar", "bar", "foobar"), nc=2, byrow=TRUE)
g13 <- graph.edgelist(el)

plot(g13)

edge.set <- t(combn(letters[1:5],2))

g9 <- graph.edgelist(edge.set,directed = FALSE)

plot(g9)

g <- make_ring(10)
degree(g)
g2 <- sample_gnp(1000, 10/1000)
degree_distribution(g2)

