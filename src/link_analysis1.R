###
#  player link analysis
###

library(igraph)
library(plyr)

load("./competition_data.RData")

df <- subset(team.df, team.place <= 3 ) # & competition.name == "Heritage Health Prize")
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

df.edges <- c(t(df1[,c("player1","player2")]))

g <- g + edges(df.edges)

plot(g,vertex.label=NA, vertex.size=2,
     layout=layout.fruchterman.reingold)

degree(g)[rev(order(degree(g)))]

