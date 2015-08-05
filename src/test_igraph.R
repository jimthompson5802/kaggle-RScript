library(igraph)
library(plyr)


my.vertices <- letters[1:10]

my.edges <- matrix(c("a","b",
              "c","d",
              "b","a",
              "e","f",
              "h","e",
              "f","g",
              "f","e",
              "j","a",
              "a","e"),
              ncol=2,byrow=TRUE)
              
colnames(my.edges) <- c("col1","col2")


g <- graph.empty(directed=FALSE)

vert.set <- vertices(my.vertices)


g <- g + vertices(my.vertices) 
g <- g + edges(c(t(my.edges)))

plot(g)

# consolidate multiple edges and determine number of times an edge appeared in the graph
g.edges.df <- as_long_data_frame(g)
g.edges.df[,3] <- as.character(g.edges.df[,3])
g.edges.df[,4] <- as.character(g.edges.df[,4])
names(g.edges.df)[3:4] <- c("col1","col2")
g.edges.df <- ddply(g.edges.df,.(col1,col2),summarize,count=length(col1))

# create graph with consolidated edges
g2 <- graph.empty(directed=FALSE)
g2 <- g2 + vertices(my.vertices)
g2 <- g2 + edges(c(t(g.edges.df[c("col1","col2")])))

# set edge color to represent number of times edge appeared in original data
E(g2,P=c(t(g.edges.df[c("col1","col2")])))$weight <- g.edges.df$count

# set edge color attribute based on count value
edge.weight <- E(g2,P=c(t(g.edges.df[c("col1","col2")])))$weight
E(g2,P=c(t(g.edges.df[c("col1","col2")])))$color <- ifelse(edge.weight==1,"green","red")

# set selected vertices to specified color
V(g2)[name %in% c("a","d")]$color <- "green"

plot(g2)

# extract out each component subgraph
cmps <- decompose(g2)

community.size <- sapply(cmps,function(g){length(V(g))})

# find larges
g3 <- cmps[[which(community.size==max(community.size))]]
plot(g3)

# find vertices with larges number of connections
degree.count <- degree(g3)[rev(order(degree(g3)))]

# find neighors of largest vertices
idx <- which(degree.count==max(degree.count))
names(idx)
adjacent_vertices(g3,names(idx))
