###
#  testing Web Driver
###

library(Rwebdriver)
library(XML)

# opts <- list(proxy="127.0.0.1:4444")
# options(RCurlOptions=opts)


start_session(root="http://127.0.0.1:4444/wd/hub/",browser="firefox")

post.url(url="http://www.r-datacollection.com/materials/selenium/intro.html")

# url of displayed page
get.url()

# title of page
page_title()

# button to click
buttonID <- element_xpath_find(value="html/body/div/div[2]/form/input")

#click on the button
element_click(ID=buttonID)

allHandles <- window_handles()

window_change(allHandles[1])

# find elements to fill-in
yearID <- element_xpath_find("//*[@id='yearSelect']")
monthID <- element_xpath_find("//*[@id='monthSelect']")
recipID <- element_xpath_find("//*[@id='recipientSelect']")

# enter desired data for retrieval
element_click(yearID)
keys("2013")

element_click(monthID)
keys("January")

element_click(recipID)
keys("Barack Obama")

# find submit button
submitID <- element_xpath_find("//*[@id='yearForm']/div/button")
element_click(submitID)

# now retrieve html of response page
pageSource <- page_source()
moneyTab <- readHTMLTable(pageSource,which=1)

# specify column names for data frame
colnames(moneyTab) <- c("year","name","party","contributor","state","amount")

# eliminate first row which has column name from web page
moneyTab <- moneyTab[-1,]
head(moneyTab)

quit_session()

# player.url <- paste0(PLAYER.URL.PREFIX,df[1,"member.url"])
# webpage <- getURL(player.url)
# webpage <- readLines(tc <- textConnection(webpage)); close(tc)
# player.html <- htmlParse(webpage)
# 
# player <- xpathApply(player.html,"//div[@class='profile2-col-section profile2-2cols']")
# player.data <- getNodeSet(player[[1]],"div/div/div/dl")
# xmlValue(player.data[[1]])

