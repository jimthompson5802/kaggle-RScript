###
# testing code fragment for extracting user location from profile
###

library(Rwebdriver)
library(XML)
library(ggmap)


start_session(root="http://127.0.0.1:4444/wd/hub/",browser="firefox")
implicit_wait(5000)

post.url(url="https://www.kaggle.com/pengliu84")
# post.url(url="https://www.kaggle.com/jimthompson")

profile.html <- htmlParse(page_source())


user.location <- xmlValue(xpathApply(profile.html,'//*[@id="profile2-bio-vitals"]/dd')[[1]])

if (nchar(user.location) >0 ) {
    geocoded.location <- geocode(user.location,output = "more")$country
} else {
    geocoded.location <- "unknown"
}



