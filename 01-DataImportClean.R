###############################################################################
## Import and clean data 
###############################################################################

## 01 preliminaries
###############################################################################
require(readxl)
require(htmltab)
require(tidyr)
require(dplyr)


## 02 download data on population structure
###############################################################################
## this is from the World Populaiton Prospects, interpolated data for 
## individual years of age, 1950 - 2015

data.url <- paste0("https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20",
                      "(Standard)/EXCEL_FILES/5_Interpolated/",
                      "WPP2015_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.XLS")
data.location <- paste("data", "WPP2015_annual_age_women.xls", sep="/")
download.file(data.url, data.location, mode = "wb")

full.data <- read_excel(data.location, sheet = 1, skip=16, na = "..." )
str(full.data)
# get list of country names
cntry.list <-as.vector(sort(unlist(unique(full.data[full.data[,5] < 900 ,3]))))

## 03 download data on first year of women's suffrage from Wikipedia
###############################################################################
wiki.votes <- htmltab(doc="https://en.wikipedia.org/wiki/Women%27s_suffrage", which = 1,
                      colNames = c("cntry", "year", "notes"))


## 03.1 clean  data on first year of women's suffrage from Wikipedia
###############################################################################
## god this is messy... manually match country names with WPP data from cntry.list:

wiki.votes$cntry[grep("Â",wiki.votes$cntry)] <- substring(
  wiki.votes$cntry[grep("Â",wiki.votes$cntry)], 3)

wiki.votes$cntry[1] <- cntry.list[1]
wiki.votes$cntry[2] <- cntry.list[2]
wiki.votes <- wiki.votes[-4,]  # no andorra in wpp
wiki.votes$cntry[4] <- cntry.list[4]
wiki.votes$cntry[9] <- cntry.list[11]
wiki.votes$cntry[14] <- cntry.list[5]
wiki.votes <- slice(wiki.votes, c(1:14, rep(15, each=3), 16:n()))
wiki.votes$cntry[15] <- cntry.list[63] # grenada
wiki.votes$cntry[16] <- cntry.list[151] # st lucia
wiki.votes$cntry[17] <- cntry.list[152] # st vincent and the grenadines
wiki.votes$cntry[18] <- cntry.list[16]
wiki.votes$cntry[20]  <- cntry.list[18]
wiki.votes$cntry[21] <-  cntry.list[19]
wiki.votes <- wiki.votes[-22,]  # no bermuda in wpp
wiki.votes$cntry[23] <- cntry.list[21]
wiki.votes$cntry[26] <- cntry.list[25]
wiki.votes$cntry[27]  <- cntry.list[26]
wiki.votes$cntry[28]  <- cntry.list[27]
wiki.votes$cntry[29] <- cntry.list[123] # myanmar
wiki.votes$cntry[31] <- cntry.list[30]
wiki.votes$cntry[32] <- cntry.list[31]
wiki.votes$cntry[34] <- cntry.list[29]
wiki.votes <- wiki.votes[-35,]  # no cayman islands in wpp
wiki.votes <- wiki.votes[-41,]  # no corsica in wpp
wiki.votes$cntry[41] <- cntry.list[51]
cntry.list[1:60]
