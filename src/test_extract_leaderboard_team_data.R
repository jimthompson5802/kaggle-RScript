###
# Initial test on Leaderbaord
###

library(XML)
library(RCurl)
library(ggmap)
library(Rwebdriver)
library(tm)

PLAYER.URL.PREFIX <- "https://www.kaggle.com"

# start up RWebdriver to access member profile data
start_session(root="http://127.0.0.1:4444/wd/hub/",browser="firefox")

url <- "https://www.kaggle.com/c/otto-group-product-classification-challenge/leaderboard"

post.url(url)

extractTeamData <- function(team) {
    
    # extract place of team
    team.place <- as.integer(xmlValue(getNodeSet(team,"td[@class='leader-number']")[[1]]))
    
    # determine type of team single player or multiple player team
    team.info <- getNodeSet(team,"td/div/a[contains(@class,'team-link')]")
    team.name <- trimws(stripWhitespace(xmlValue(team.info[[1]])))
    team.type <- xmlAttrs(team.info[[1]])
    team.type <- team.type["class"]
    if (team.type == "team-link team-player") {
        # multiple player team
        team.members <- getNodeSet(team,"td/div/ul/li")
        
        member.url <- sapply(team.members,function(anode){
            x <- xmlAttrs(getNodeSet(anode,"a")[[1]])
            trimws(stripWhitespace(x["href"]))
        })
        
        member.name <- sapply(team.members,function(anode){
            x <- unlist(getNodeSet(anode,"a",fun=xmlValue))
            trimws(stripWhitespace(x))
        })
        
    } else {
        # single player team
        member.url <- xmlAttrs(getNodeSet(team,"td/div/a[contains(@class,'team-link')]")[[1]])["href"]
        member.name <- trimws(stripWhitespace(xmlValue(getNodeSet(team,"td/div/a[contains(@class,'team-link')]")[[1]])))
    }
    
    if (team.place <= 3) {

        # determine member location
        member.location <- sapply(member.url,function(url.frag){
            #retrieve member profile page
            post.url(url=paste0(PLAYER.URL.PREFIX,url.frag))
            # Sys.sleep(2)
            element_xpath_find('//*[@id="profile-bio"]/h2')
            
            #get member page data
            member.page <- htmlParse(page_source())
            
            #find user location
            user.location <- xmlValue(xpathApply(member.page,'//*[@id="profile2-bio-vitals"]/dd')[[1]])
            
            if (nchar(user.location) > 0 ) {
                geocoded.location <- geocode(user.location,output = "more")$country
            } else {
                geocoded.location <- "unknown"
            }
            
            page_back()
            element_xpath_find('//*[@id="leaderboard-table"]/tbody/tr[1]/th[1]')
            
            return(geocoded.location)
        })
        
        # pro-rate place medals
        medal.weight <- 1/length(member.url)
    } else {
        member.location <- NA
        medal.weight <- NA
    }
    
    data.frame(team.type=team.type,team.place=team.place,
               team.name=team.name,
               member.name=member.name,
               member.url=member.url,
               member.location=member.location,
               medal.weight=medal.weight,
               stringsAsFactors=FALSE)

}

extractTeamDataWrapper <- function(lb.idx) {
    
    one.row <- xpathApply(lb.html,
                          paste0('//*[@id="leaderboard-table"]/tbody/tr[',
                                 lb.idx,']'))[[1]] 
    team.place <- getNodeSet(one.row,"td[@class='leader-number']")
    
    if (length(team.place) == 1 && xmlValue(team.place[[1]]) >= 1) {
        df <- extractTeamData(one.row)
    } else {
        df <- NULL
    }
    
    return(df)
}

lb.html <- htmlParse(page_source())

# isolate the html table containing leaderboard data
lb.table <- xpathApply(lb.html,'//*[@id="leaderboard-table"]/tbody')

# extract out each row in the leaderboard 
leaderboard <- xpathApply(lb.table[[1]],"tr")

ll <- lapply(1:length(leaderboard),extractTeamDataWrapper)

df <- do.call(rbind,ll)
# 
# competition.info.node <- xpathApply(html1,"//header[@id='comp-header']")
# competition.name <- xmlValue(getNodeSet(competition.info.node[[1]],"//h1/a")[[1]])
# competition.date <- xmlValue(getNodeSet(competition.info.node[[1]],
#                                         "//div[@id='comp-header-stats']/div[@id='comp-header-stats-end']")[[1]])
# ptr <- regexpr("\\d{1,2} \\w{3} \\d\\d\\d\\d",competition.date,ignore.case=TRUE)
# competition.end.date <- substr(competition.date,ptr,ptr+attr(ptr,"match.length")-1)
# 
# df <- cbind(competition.name,competition.end.date,df,stringsAsFactors=FALSE)
# 
# quit_session()
