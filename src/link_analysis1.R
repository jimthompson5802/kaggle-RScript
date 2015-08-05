###
#  player link analysis
###

library(igraph)
library(plyr)

load("./competition_data.RData")

df <- subset(team.df, team.place <= 10000)# & competition.name %in% c("Heritage Health Prize","Otto Group Product Classification Challenge"))
#              competition.name %in% c("Heritage Health Prize",
#                                      "Otto Group Product Classification Challenge"))

# extract out winner identifiers
winners <- subset(df,team.place <= 3,member.url)

# function to transform raw team data into graph edges
create_edges <- function(df) {
    # df:  data frame containing only members of one team
    edges <- t(combn(df$member.url,2))
    colnames(edges) <- c("player1","player2")
    return(edges)
}

# create edge list
df1 <- ddply(subset(df,team.type=="team-link team-player"),
             .(competition.name,team.place),create_edges)
df1$player1 <- as.character(df1$player1)
df1$player2 <- as.character(df1$player2)

# create a graph to perform initial data scrubbing
g <- graph.empty(directed=FALSE) 
g <- g + vertices(unique(df$member.url))
g <- g + edges(c(t(df1[,c("player1","player2")])))

lyt <- layout_nicely(g)

plot(g, 
     vertex.size=2,
     layout=lyt,
     vertex.label=NA)

# consolidate multiple edges and determine number of times an edge appeared in the graph
g.edges.df <- as_long_data_frame(g)
g.edges.df[,3] <- as.character(g.edges.df[,3])
g.edges.df[,4] <- as.character(g.edges.df[,4])
names(g.edges.df)[3:4] <- c("player1","player2")
g.edges.df <- ddply(g.edges.df,.(player1,player2),summarize,number.competitions=length(player1))

# reload the edge data after cleaining it up
g <- g - E(g)
g <- g + edges(c(t(g.edges.df[,c("player1","player2")])))

# set edge attribute represent number of times edge appeared in original data
E(g,P=c(t(g.edges.df[c("player1","player2")])))$number.competiions <- g.edges.df$number.competitions

V(g)$winner <- V(g)$name %in% winners$member.url
V(g)$color <- ifelse(V(g)$winner,"red","green")  # distinguish winners and non-winners
V(g)$size <- ifelse(V(g)$winner,5,2)  # to make visible in full plot

# display revised graph
plot(g, 
     # vertex.size=2,
     layout=lyt,
     vertex.label=NA)

# select only players who have won a competition
g <- g - vertices(V(g)$name[!V(g)$winner])
V(g)$size <- 2

#display winner only graph
lyt <- layout_nicely(g)
plot(g, 
     layout=lyt,
     vertex.label=NA)

#eliminate single only teams
g <- g - vertices(V(g)[degree(g)==0])

#display winner only graph
lyt <- layout_nicely(g)
plot(g, 
     layout=lyt,
     vertex.label=NA)


# extract out each community subgraph
cmnties <- decompose(g)

#determine size of each community
community.size <- sapply(cmnties,function(g){length(V(g))})
summary(community.size)

g2 <- cmnties[[which(community.size == max(community.size))]]

lyt <- layout_nicely(g2)
plot(g2,
     layout=lyt,
     vertex.label=NA)


g.df <- as_long_data_frame(g)
g.df <- subset(g.df,from_winner == FALSE & to_winner == TRUE)

degree(g)[rev(order(degree(g)))]

clq1 <- max_cliques(g,min=5)
clq2 <- largest_cliques(g)

count_components(g)

cmnt1 <- cluster_walktrap(g)
plot(cmnt1,g,
     vertex.label=NA)
