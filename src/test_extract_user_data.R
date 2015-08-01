###
#  test retrieving user location data from profile
###

library(ggmap)
library(RCurl)
library(XML)
library(Rwebdriver)


#start webdriver session 
start_session(root="http://127.0.0.1:4444/wd/hub/",browser="firefox")

#specify the member profile url
member.url.fragment <- "/titericz"

kaggle.base.url <- "https://www.kaggle.com"

#retrieve member profile page
post.url(url=paste0(kaggle.base.url,member.url.fragment))


#get member page data
member.page <- htmlParse(page_source())

#find user location
member.location <- xmlValue(xpathApply(member.page,'//dd[@data-bind="text: location"]')[[1]])



fileConn<-file("./data/member_page.txt")
writeLines(member.page, fileConn)
close(fileConn)


quit_session()
