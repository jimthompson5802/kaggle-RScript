# determine member location
member.location <- sapply(member.url,function(url.frag){
    #             if (!is.na(member.url)) {
    #                 #retrieve member profile page
    #                 post.url(url=paste0(PLAYER.URL.PREFIX,url.frag))
    #                 # Sys.sleep(2)
    #                 element_xpath_find('//*[@id="profile-bio"]/h2')
    #                 
    #                 #get member page data
    #                 member.page <- htmlParse(page_source())
    #                 
    #                 #find user location
    #                 user.location <- xmlValue(xpathApply(member.page,'//*[@id="profile2-bio-vitals"]/dd')[[1]])
    #                 
    #                 if (nchar(user.location) > 0 ) {
    #                     geocoded.location <- geocode(user.location,output = "more")$country
    #                 } else {
    #                     geocoded.location <- "unknown"
    #                 }
    #                 
    #                 page_back()
    #                 element_xpath_find('//*[@id="leaderboard-table"]/tbody/tr[1]/th[1]')
    #             } else {
    #                 geocoded.location <- NA
    #             }
    geocoded.location <- NA
    return(geocoded.location)
})

# pro-rate place medals
medal.weight <- 1/length(member.url)
} else {
    member.location <- NA
    medal.weight <- NA
}
