###
#  Program to extract completed competition data from kaggle.com
###

# standard R packages
library(XML)
library(ggmap)
library(tm)
library(plyr)

PLAYER.URL.PREFIX <- "https://www.kaggle.com"

# non-standard package - installed from github.com
# library(devtools)
# install_github(repo="Rwebdriver", username="crubba")
# Rwebdriver requires non-R software from http://www.seleniumhq.org/projects/webdriver/
library(Rwebdriver)  

# start Selenium Webdriver server prior to running this R program
# create session to Webdriver software
start_session(root="http://127.0.0.1:4444/wd/hub/",browser="firefox")
implicit_wait(5000)  # wait for 5 seconds to locate elements on web page

# get initial web page for kaggle competitions
post.url(url="https://www.kaggle.com/competitions")

# specify web page options to find all completed competitions
buttonID <- element_xpath_find(value='//*[@id="all-switch"]')
element_click(buttonID)

# uncheck Active competiton checkbox
showActiveID <- element_xpath_find(value='//*[@id="competitions-filter"]/div[2]/ul/li[1]/label')
element_click(showActiveID)

# check Completed competition checkbox
showCompletedID <- element_xpath_find(value='//*[@id="competitions-filter"]/div[2]/ul/li[2]/label')
element_click(showCompletedID)

# click on "All Competition" radio button to force page refresh
allCompetitionID <- element_xpath_find(value='//*[@id="all-or-enterable"]/ul/li/label')
element_click(allCompetitionID)

#wait for page to refresh and show only completed competitons
#look for "Active" count to be zero
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

# extract data about one team in the competition
extractTeamData <- function(team) {
    # team: HTML element representing one team from a competition
    
    # extract place of team
    team.place <- as.integer(xmlValue(getNodeSet(team,"td[@class='leader-number']")[[1]]))
    
    # determine type of team single player or multiple player team
    team.info <- getNodeSet(team,"td/div/a[contains(@class,'team-link')]")
    if (length(team.info) > 0) {
        # team member data present
        team.name <- trimws(stripWhitespace(xmlValue(team.info[[1]])))
        team.type <- xmlAttrs(team.info[[1]])
        team.type <- team.type["class"]
        
        #deterimine if multi-player or single player team
        if (team.type == "team-link team-player") {
            # multiple player team, extract element nodes for all team members
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
    } else {
        # no team member info, set to default values
        team.type <- "anonymous-type"
        team.name <- "Anonymous"
        member.name <- "Not Given"
        member.url <- NA
    }
    
    # create data frame to hold team member data
    data.frame(team.type=team.type,
               team.place=team.place,
               team.name=team.name,
               member.name=member.name,
               member.url=member.url,
               stringsAsFactors=FALSE)
    
}

# wrapper function to team data extraction
extractTeamDataWrapper <- function(lb.idx,lb.html) {
    # lb.idx: index into leaderboard table
    # lb.html: parsed webpage displaying the leaderboard
    
    # extract team data for specified leaderboard index value
    one.row <- xpathApply(lb.html,
                          paste0('//*[@id="leaderboard-table"]/tbody/tr[',
                                 lb.idx,']'))[[1]] 
    team.place <- getNodeSet(one.row,"td[@class='leader-number']")
    
    df <- NULL
    if (length(team.place) == 1 && length(xmlValue(team.place[[1]])) > 0) {
        if (grepl("\\d+",xmlValue(team.place[[1]]))) {
            df <- extractTeamData(one.row)
        }
    }
    
    return(df)
}

# function to extract competition related data
getCompetitonData <- function(comp.idx) {
    # comp.idx:  index into competition table
    
    cat("starting compeition:",comp.idx,"out of",length(competitions),"\n")
    flush.console()
    
    # extract out competition data for specified competition index
    one.row <- xpathApply(competition.inventory.page,
                          paste0('//*[@id="competitions-table"]/tbody/tr[',
                                    comp.idx,']'))[[1]]
    # extract required data attributes
    competition.name <- xmlValue(xpathApply(one.row,"td[1]/div/a/h4")[[1]])
    competition.url <- xmlAttrs(xpathApply(one.row,"td[1]/div/a")[[1]])["href"]
    type <- xmlValue(xpathApply(one.row,"td[2]")[[1]])
    number.of.teams <- xmlValue(xpathApply(one.row,"td[3]")[[1]])
    deadline <- xmlValue(xpathApply(one.row,"td[4]")[[1]])
    
    # set default values
    start.date <- NA
    end.date <- NA
    
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
    
    ####
    # extract out team data
    # for only "standard" competitions that have one or more teams
    ####
    df.team <- NULL  # assume no data
    # get team data only for standard competitions and non-zero time count
    if (competition.type == "standard" && number.of.teams > 0) {
        # go to competition specific page
        compID <- element_xpath_find(paste0('//*[@id="competitions-table"]/tbody/tr[',
                                            comp.idx,
                                            ']/td[1]/div/a/h4'))
        element_click(compID)
        cat("specific competition on page:",get.url(),"\n")
        flush.console()
        
        # find the location for start and end date data
        element_xpath_find('//*[@id="end-time-note"]/strong[1]')
        
        # get start and end date for competitons
        competition.html <- htmlParse(page_source())
        start.node <- xpathApply(competition.html,'//*[@id="end-time-note"]/text()[2]')
        if (length(start.node) == 1) {
            start.date <- xmlValue(start.node[[1]])
        }
        
        end.node <- xpathApply(competition.html,'//*[@id="end-time-note"]/text()[3]')
        if (length(start.node) == 1) {
            end.date <- xmlValue(end.node[[1]])
        } 
        
        # find link to display leaderboard
        lbID <- element_xpath_find("//*[@id='competition-dashboard-dropdown']/li[@class='cd-leaderboard']/ul/li[2]/a")
        
        # did we find a hyper-link to leaderboard?
        if (length(lbID) == 0) {
            # no...try alternative location for lb button
            lbID <- element_xpath_find('//*[@id="competition-dashboard-dropdown"]/li[4]/a')
        }
        
        # did we really get a hyper-link to leaderboard?
        # if no leaderboard hyper-link found, then bypass and return NULL df.team
        if (length(lbID) != 0) {
            # yes...found hyperlink, go to leaderboard
            element_click(lbID)
            lb.html <- htmlParse(page_source())
            
            # isolate the html table containing data about completed competitions
            lb.table <- xpathApply(lb.html,'//*[@id="leaderboard-table"]/tbody')
            
            # extract out each row in the competition table
            leaderboard <- xpathApply(lb.table[[1]],"tr")
            
            ll <- lapply(1:length(leaderboard),extractTeamDataWrapper,lb.html)
            
            # consolidate all team data into a singel data frame
            df.team <- do.call(rbind,ll)
            
            # go back to competition inventory page
            page_back(2)
            cat("after 2 back button on page:",get.url(),"\n")
            flush.console()

        } else {
            # go back only one page to competition inventory page
            page_back(2)
            cat("after 1 back button on page:",get.url(),"\n")
            flush.console()
        }
        
        # wait for competition inventory page to refresh
        buttonID <- element_xpath_find(value='//*[@id="all-switch"]')
        element_click(buttonID)
        
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
        
    }
    
    ###
    # return competition and team data
    ###
    
    #create the data frame for the competiton
    df.comp <- data.frame(competition.name,
                          competition.url,
                          competition.type,
                          prize.amount,
                          number.of.teams,
                          deadline,
                          start.date,
                          end.date,
                          stringsAsFactors=FALSE)
    
    # create data frame for team data
    if (!is.null(df.team)) {
        df.team <- cbind(competition.name,df.team,stringsAsFactors=FALSE)
    }
    
    cat("completed:",comp.idx,", ",competition.name,"\n")
    flush.console()
    
    return(list(df.comp=df.comp,df.team=df.team))
}


###
# create data frame for all completed competitions
###
# ll <- lapply(1:10,getCompetitonData)  #for debugging
system.time(ll <- lapply(1:length(competitions),getCompetitonData))

# consolidate all competiton data into a single data frame
competition.df <- do.call(rbind,lapply(ll,function(x){x$df.comp}))
comment(competition.df) <- paste("created on",Sys.time())

# consolidate all team member data into a single data frame
team.df <- do.call(rbind,lapply(ll,function(x){x$df.team}))
comment(team.df) <- paste("created on",Sys.time())

# competition and team data for later processing
save(competition.df,team.df,file="./competition_data.RData")

# quit Webdriver session
quit_session()



