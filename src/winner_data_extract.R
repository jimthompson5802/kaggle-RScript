###
#  Extract winning team data for graph analysis
#  This data will be manually copied into Rmarkdown document for
#  submission to Kaggle RScript competition.
###

load("./competition_data.RData")


winners <- subset(team.df,team.place <= 3)
write.csv(winners,file="./winners.csv",row.names=FALSE)
