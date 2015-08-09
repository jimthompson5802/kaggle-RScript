###
#  Program to extract and normalize location data for the members of the
#  top 3 teams 
###

# standard R packages
library(XML)
library(ggmap)
library(tm)
library(plyr)

PLAYER.URL.PREFIX <- "https://www.kaggle.com"

load("competition_data.RData")

# extract medal winners, i.e., top 3 finishing teams for each competition
top3.df <- subset(team.df,team.place <= 3,select=c(member.url,team.place,competition.name,team.name))

# pro-rate medal weight for multi-player team
medal.df <- ddply(top3.df,.(competition.name,team.place),summarize,
                 medal.weight=1/length(member.url),
                 team.member.count=length(member.url))



# non-standard package - installed from github.com
# library(devtools)
# install_github(repo="Rwebdriver", username="crubba")
# Rwebdriver requires non-R software from http://www.seleniumhq.org/projects/webdriver/
library(Rwebdriver)  

# start Selenium Webdriver server prior to running this R program
# create session to Webdriver software
start_session(root="http://127.0.0.1:4444/wd/hub/",browser="firefox")
implicit_wait(5000)  # wait for 5 seconds to locate elements on web page


getMemberLocation <- function(onerow) {
    
     #retrieve member profile page
     post.url(url=paste0(PLAYER.URL.PREFIX,onerow$member.url))
     element_xpath_find('//*[@id="profile-bio"]/h2')
     
     #get member page data
     member.page <- htmlParse(page_source())
     
     #find user location
     user.location <- xmlValue(xpathApply(member.page,'//*[@id="profile2-bio-vitals"]/dd')[[1]])
     
     if (nchar(user.location) > 0 && !is.na(user.location)) {
         onerow$member.location <- geocode(user.location,output = "more")$country
     } else {
         onerow$member.location <- "unknown"
     }
    
    return(onerow)
}

# extract team member location and normalize using google map geocode API
top3.df <- adply(top3.df,1,getMemberLocation)
comment(top3.df) <- paste("created on",Sys.time())
save(top3.df,file="./top3_team_members.RData")

#clean up column names
top3.df <- merge(top3.df,medal.df,by=c("competition.name","team.place"))
names(top3.df)[names(top3.df)=="medal.weight.x"] <- "medal.weight"
names(top3.df)[names(top3.df)=="team.member.count.x"] <- "team.member.count"
top3.df$medal.weight.y <- NULL
top3.df$team.member.count.y <- NULL

total.records <- nrow(top3.df)

# eliminate missing or NA records
top3.df <- subset(top3.df,!(is.na(top3.df$member.location) | top3.df$member.location == "unknown"))

with.location.data <- nrow(top3.df)

cat("removed:",total.records - with.location.data,"out of",total.records,"\n")

# determine medal count by contry
medal.count.df <- ddply(top3.df,.(member.location,team.place),summarize,medals=sum(medal.weight))

# create source text file that contains medal count data for the Rmarkdown report
lines.to.write <- c(paste0("country=c(",paste(medal.count.df$member.location,collapse=","),")"),
                    paste0("place=c(",paste(medal.count.df$team.place,collapse=","),")"),
                    paste0("medal.count=c(",paste(medal.count.df$medals,collapse=","),")")
                    )
writeLines(lines.to.write,"./medal_count_data.txt")

