###
# Initial test on Leaderbaord
###

library(XML)
library(RCurl)
library(ggmap)
library(Rwebdriver)

competition.list.url <- "https://www.kaggle.com/competitions"

COMPETITON.URL.PREFIX <- "https://www.kaggle.com/c"
PLAYER.URL.PREFIX <- "https://www.kaggle.com"

# start up RWebdriver to access member profile data
start_session(root="http://127.0.0.1:4444/wd/hub/",browser="firefox")

url <- "https://www.kaggle.com/c/otto-group-product-classification-challenge/leaderboard"

webpage <- getURL(url)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
html1 <- htmlParse(webpage)

# find leader table
winners <- xpathApply(html1,"//table/tr[td[@class='leader-number']<=3]")
teams <- xpathApply(html1,"//table/tr[td[@class='leader-number']>=1]")

score <- xmlValue(getNodeSet(winners[[2]],"td/abbr[@class='score']")[[1]])

one.team <- getNodeSet(winners[[1]],
                      "td/div/a[contains(@class,'team-link')]/parent::node()/parent::node()")

team.type <- getNodeSet(winners[[1]],"td/div/a")

team.place <- xmlValue(getNodeSet(winners[[2]],"td[@class='leader-number']")[[1]])

team.members <- getNodeSet(winners[[1]],"td/div/ul/li")
x <- xmlAttrs(getNodeSet(team.members[[1]],"a")[[1]])

extractTeamData <- function(team) {
    
    # extract place of team
    team.place <- xmlValue(getNodeSet(team,"td[@class='leader-number']")[[1]])
    
    # determine type of team single player or multiple player team
    team.info <- getNodeSet(team,"td/div/a[contains(@class,'team-link')]")
    team.name <- xmlValue(team.info[[1]])
    team.type <- xmlAttrs(team.info[[1]])
    team.type <- team.type["class"]
    if (team.type == "team-link team-player") {
        # multiple player team
        team.members <- getNodeSet(team,"td/div/ul/li")
        
        member.url <- sapply(team.members,function(anode){
            x <- xmlAttrs(getNodeSet(anode,"a")[[1]])
            x["href"]
        })
        
        member.name <- sapply(team.members,function(anode){
            x <- unlist(getNodeSet(anode,"a",fun=xmlValue))
        })
    } else {
        # single player team
        member.url <- xmlAttrs(getNodeSet(team,"td/div/a[contains(@class,'team-link')]")[[1]])["href"]
        member.name <- xmlValue(getNodeSet(team,"td/div/a[contains(@class,'team-link')]")[[1]])
    }
    
    # determine member location
    member.location <- sapply(member.url,function(url.frag){
        #retrieve member profile page
        post.url(url=paste0(PLAYER.URL.PREFIX,url.frag))
        Sys.sleep(2)  # delay to allow page to load before getting page, this a heuristic
        
        #get member page data
        member.page <- htmlParse(page_source())
        
        #find user location
        location <- geocode(xmlValue(xpathApply(member.page,'//dd[@data-bind="text: location"]')[[1]]),
                            output="more")$country
        
    })
    
    # pro-rate place medals
    medal.weight <- 1/length(member.url)
    
    data.frame(team.type=team.type,team.place=team.place,
               team.name=team.name,
               member.name=member.name,
               member.url=member.url,
               member.location=member.location,
               medal.weight=medal.weight,
               stringsAsFactors=FALSE)

}

ll <- lapply(winners,extractTeamData)

df <- do.call(rbind,ll)

competition.info.node <- xpathApply(html1,"//header[@id='comp-header']")
competition.name <- xmlValue(getNodeSet(competition.info.node[[1]],"//h1/a")[[1]])
competition.date <- xmlValue(getNodeSet(competition.info.node[[1]],
                                        "//div[@id='comp-header-stats']/div[@id='comp-header-stats-end']")[[1]])
ptr <- regexpr("\\d{1,2} \\w{3} \\d\\d\\d\\d",competition.date,ignore.case=TRUE)
competition.end.date <- substr(competition.date,ptr,ptr+attr(ptr,"match.length")-1)

df <- cbind(competition.name,competition.end.date,df,stringsAsFactors=FALSE)

quit_session()
