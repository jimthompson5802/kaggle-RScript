###
#  player link analysis
###

library(igraph)
library(plyr)

load("./competition_data.RData")

df <- subset(team.df, team.place <= 10000)# & competition.name %in% c("Heritage Health Prize","Otto Group Product Classification Challenge"))
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

g <- g + vertices(unique(df$member.url))


V(g)$winner <- V(g)$name %in% winners$member.url
V(g)$color <- ifelse(V(g)$winner,"red","green")
V(g)$size <- ifelse(V(g)$winner,5,2)

df.edges <- c(t(df1[,c("player1","player2")]))

g <- g + edges(df.edges)

lyt <- layout_nicely(g)

plot(g, 
     vertex.size=2,
     layout=lyt,
     vertex.label=NA)

g <- g - vertices(V(g)$name[!V(g)$winner])

degree(g)[rev(order(degree(g)))]

clq1 <- max_cliques(g,min=5)
clq2 <- largest_cliques(g)

