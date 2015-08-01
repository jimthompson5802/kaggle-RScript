###
#  testing Web Driver
###

library(Rwebdriver)
library(XML)

start_session(root="http://127.0.0.1:4444/wd/hub/",browser="firefox")

post.url(url="https://www.kaggle.com/competitions")



# find link to show all competitions
buttonID <- element_xpath_find(value='//*[@id="all-switch"]')
element_click(buttonID)

showActiveID <- element_xpath_find(value='//*[@id="competitions-filter"]/div[2]/ul/li[1]/label')
element_click(showActiveID)

showCompletedID <- element_xpath_find(value='//*[@id="competitions-filter"]/div[2]/ul/li[2]/label')
element_click(showCompletedID)

competition.inventory.page <- htmlParse(page_source())

# isolate the html table containing data about completed competitions
competition.table <- xpathApply(competition.inventory.page,'//*[@id="competitions-table"]/tbody')

# extract out each row in the competition table
competitions <- xpathApply(competition.table[[1]],"tr")

# define function to extract out comeptition data
getCompetitonData <- function(one.row) {
    competition.name <- xmlValue(xpathApply(one.row,"td[1]/div/a/h4")[[1]])
    competition.url <- xmlAttrs(xpathApply(one.row,"td[1]/div/a")[[1]])["href"]
    prize <- xmlValue(xpathApply(one.row,"td[2]")[[1]])
    number.of.teams <- xmlValue(xpathApply(one.row,"td[3]")[[1]])
    
    df <- data.frame(competition.name,
                     competition.url,
                     prize,
                     number.of.teams,
                     stringsAsFactors=FALSE)
    return(df)
}

# create data frame for all completed competitions
ll <- lapply(competitions,getCompetitonData)
competition.df <- do.call(rbind,ll)

save(competition.df,file="./competition.RDATA")


quit_session()



