###
#  testing Web Driver
###

library(Rwebdriver)
library(XML)

start_session(root="http://127.0.0.1:4444/wd/hub/",browser="firefox")
implicit_wait(5000)

post.url(url="https://www.kaggle.com/competitions")



# find link to show all competitions
buttonID <- element_xpath_find(value='//*[@id="all-switch"]')
element_click(buttonID)
# Sys.sleep(1)

showActiveID <- element_xpath_find(value='//*[@id="competitions-filter"]/div[2]/ul/li[1]/label')
element_click(showActiveID)
# Sys.sleep(1)
showCompletedID <- element_xpath_find(value='//*[@id="competitions-filter"]/div[2]/ul/li[2]/label')
element_click(showCompletedID)
# Sys.sleep(3)   #wait until page refreshes - not ideal solution

allCompetitionID <- element_xpath_find(value='//*[@id="all-or-enterable"]/ul/li/label')
element_click(allCompetitionID)

#wait for page to refresh and show only completed competitons
competition.inventory.page <- htmlParse(page_source())
loop.counter <- 0
while (loop.counter < 10 && 
       xmlValue(xpathApply(competition.inventory.page,'//*[@id="sidebar-active-comps-found"]')[[1]]) != 0) {
    cat("before extraction active comp count:",loop.counter,":",
        xmlValue(xpathApply(competition.inventory.page,'//*[@id="sidebar-active-comps-found"]')[[1]]),
        "\n")
    flush.console()
    competition.inventory.page <- htmlParse(page_source())
    Sys.sleep(1)
    loop.counter <- loop.counter + 1
}

# isolate the html table containing data about completed competitions
competition.table <- xpathApply(competition.inventory.page,'//*[@id="competitions-table"]/tbody')

# extract out each row in the competition table
competitions <- xpathApply(competition.table[[1]],"tr")

# extract data about teams participating in the competition
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
#             #retrieve member profile page
#             post.url(url=paste0(PLAYER.URL.PREFIX,url.frag))
#             # Sys.sleep(2)
#             element_xpath_find('//*[@id="profile-bio"]/h2')
#             
#             #get member page data
#             member.page <- htmlParse(page_source())
#             
#             #find user location
#             user.location <- xmlValue(xpathApply(member.page,'//*[@id="profile2-bio-vitals"]/dd')[[1]])
#             
#             if (nchar(user.location) > 0 ) {
#                 geocoded.location <- geocode(user.location,output = "more")$country
#             } else {
#                 geocoded.location <- "unknown"
#             }
#             
#             page_back()
#             element_xpath_find('//*[@id="leaderboard-table"]/tbody/tr[1]/th[1]')
            geocoded.location <- NA
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

extractTeamDataWrapper <- function(lb.idx,lb.html) {
    
    one.row <- xpathApply(lb.html,
                          paste0('//*[@id="leaderboard-table"]/tbody/tr[',
                                 lb.idx,']'))[[1]] 
    team.place <- getNodeSet(one.row,"td[@class='leader-number']")
    
    df <- NULL
    if (length(team.place) == 1 && length(xmlValue(team.place[[1]])) > 0) {
        if (as.integer(xmlValue(team.place[[1]])) >= 1) {
            df <- extractTeamData(one.row)
        }
    }
    
    return(df)
}

# define function to extract out comeptition data
getCompetitonData <- function(comp.idx) {
    
    one.row <- xpathApply(competition.inventory.page,
                          paste0('//*[@id="competitions-table"]/tbody/tr[',
                                    comp.idx,']'))[[1]]
    
    competition.name <- xmlValue(xpathApply(one.row,"td[1]/div/a/h4")[[1]])
    competition.url <- xmlAttrs(xpathApply(one.row,"td[1]/div/a")[[1]])["href"]
    type <- xmlValue(xpathApply(one.row,"td[2]")[[1]])
    number.of.teams <- xmlValue(xpathApply(one.row,"td[3]")[[1]])
    deadline <- xmlValue(xpathApply(one.row,"td[4]")[[1]])
    
    # stqndardize type attribute and create prize.amount
    type <- gsub(",","",type)
    type <- gsub("\\$","",type)
    if (grepl("\\d+",type)) {
        prize.amount <- as.numeric(type)
        competition.type <- "standard"
    } else {
        competition.type <- type
        prize.amount <- NA
    }
    
    #create the data frame for the competiton
    df.comp <- data.frame(competition.name,
                     competition.url,
                     competition.type,
                     prize.amount,
                     number.of.teams,
                     deadline,
                     stringsAsFactors=FALSE)
    ####
    # extract out team data
    ####
    # get team data only for standard competitions and non-zero time count
    if (competition.type == "standard" && number.of.teams > 0) {
        # go to competition specific page
        compID <- element_xpath_find(paste0('//*[@id="competitions-table"]/tbody/tr[',
                                            comp.idx,
                                            ']/td[1]/div/a/h4'))
        element_click(compID)
        cat("specific competition on page:",get.url(),"\n")
        flush.console()
        
        # click link to display leaderboard
        lbID <- element_xpath_find("//*[@id='competition-dashboard-dropdown']/li[@class='cd-leaderboard']/ul/li[2]/a")
        element_click(lbID)
        
        lb.html <- htmlParse(page_source())
        
        # isolate the html table containing data about completed competitions
        lb.table <- xpathApply(lb.html,'//*[@id="leaderboard-table"]/tbody')
        
        # extract out each row in the competition table
        leaderboard <- xpathApply(lb.table[[1]],"tr")
        
        ll <- lapply(1:length(leaderboard),extractTeamDataWrapper,lb.html)
        
        df.team <- do.call(rbind,ll)
        
        # go back to competition inventory page
        page_back(2)
        cat("after back button on page:",get.url(),"\n")
        flush.console()
        buttonID <- element_xpath_find(value='//*[@id="all-switch"]')
        element_click(buttonID)
        # Sys.sleep(1)
        showAllID <- element_xpath_find('//*[@id="all-or-enterable"]/ul/li/label')
        element_click(showAllID)
        
        # wait for page to refresh with only completed competitons
        current.page <- htmlParse(page_source())
        loop.counter <- 0
        while (loop.counter < 10 && 
               xmlValue(xpathApply(current.page,'//*[@id="sidebar-active-comps-found"]')[[1]]) != 0) {
            cat("active comp count:",loop.counter,":",
                xmlValue(xpathApply(current.page,'//*[@id="sidebar-active-comps-found"]')[[1]]),
                "\n")
            flush.console()
            current.page <- htmlParse(page_source())
            Sys.sleep(1)
            loop.counter <- loop.counter + 1
        }
    } else {
        df.team <- NULL
    }
    # return competition and team data
    return(list(df.comp=df.comp,df.team=df.team))
}



# create data frame for all completed competitions
# ll <- lapply(competitions,getCompetitonData
ll <- lapply(1:3,getCompetitonData)


# competition.df <- do.call(rbind,ll)
# 
# save(competition.df,file="./competition.RDATA")
# 
# 
# # //*[@id="leaderboard-table"]/tbody
# 
# 
# quit_session()



