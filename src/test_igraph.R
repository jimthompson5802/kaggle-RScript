###
# initial test of igraph
###

library(igraph)
library(plyr)

load("./competition_data.RData")

g2 <- graph( c(1,2,2,3,3,4,5,6), directed=TRUE )

el <- matrix( c("foo", "bar", "bar", "foobar"), nc=2, byrow=TRUE)
g13 <- graph.edgelist(el)

plot(g13)

edge.set <- t(combn(letters[1:5],2))

g9 <- graph.edgelist(edge.set,directed = FALSE)

plot(g9)

df <- subset(team.df, team.place <= 3)
#              competition.name %in% c("Heritage Health Prize",
#                                      "Otto Group Product Classification Challenge"))

winners <- subset(df,team.place <= 3)

create_edges <- function(df) {
    edges <- t(combn(df$member.url,2))
    colnames(edges) <- c("player1","player2")
    return(edges)
}


# create edge list
df1 <- ddply(subset(df,team.type=="team-link team-player"),
             .(competition.name,team.place),create_edges)
df1$player1 <- as.character(df1$player1)
df1$player2 <- as.character(df1$player2)

g <- graph.empty(directed=FALSE) 

g <- g + vertices(df$member.url)

V(g)$color <- ifelse(V(g)$name %in% winners$member.url,"red","green")

df.edges <- df1[,c("player1","player2")]
df.edges <- do.call(cbind,lapply(1:nrow(df.edges),function(row,df){df[row,]},df.edges))

g <- g + edges(as.matrix(df.edges))

plot(g,vertex.label=NA, vertex.size=2,
     layout=layout.fruchterman.reingold)

