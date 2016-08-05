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

# data.url <- paste0("https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20",
#                       "(Standard)/EXCEL_FILES/5_Interpolated/",
#                       "WPP2015_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.XLS")
# data.location <- paste("data", "WPP2015_annual_age_women.xls", sep="/")
# download.file(data.url, data.location, mode = "wb")

estimates.data <- read_excel(data.location, sheet = 1, skip=16, na = "..." )
projections.data <- read_excel(data.location, sheet = 2, skip=16, na = "..." )
select(estimates.data, -Notes)
full.data <- bind_rows(select(estimates.data, -Notes), select(projections.data, -Notes))
colnames(full.data)[3] <- "cntry"

colnames(estimates.data)
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
rownames(wiki.votes) <- 1:193
wiki.votes$cntry[1] <- cntry.list[1]
wiki.votes$cntry[2] <- cntry.list[2]
wiki.votes <- wiki.votes[-4,]  # no andorra in wpp
wiki.votes$cntry[4] <- cntry.list[4]
wiki.votes$cntry[9] <- cntry.list[11]
wiki.votes$cntry[14] <- cntry.list[5]
wiki.votes <- slice(wiki.votes, c(1:14, rep(15, each=3), 16:n()))
wiki.votes$cntry[15] <- cntry.list[73] # grenada
wiki.votes$cntry[16] <- cntry.list[151] # st lucia
wiki.votes$cntry[17] <- cntry.list[152] # st vincent and the grenadines
wiki.votes$cntry[18] <- cntry.list[16]
wiki.votes$cntry[20]  <- cntry.list[18]
wiki.votes$cntry[21] <-  cntry.list[19]
wiki.votes <- wiki.votes[-22,]  # no bermuda in wpp
rownames(wiki.votes) <- 1:194
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
rownames(wiki.votes) <- 1:192
wiki.votes$cntry[41] <- cntry.list[51]
wiki.votes$cntry[42] <- cntry.list[42]
wiki.votes <- wiki.votes[-43,] # no cook islands
rownames(wiki.votes) <- 1:191
wiki.votes$cntry[44] <- cntry.list[44]
wiki.votes <- slice(wiki.votes, c(1:46, rep(47, each=2), 48:n())) # czechoslovakia
wiki.votes$cntry[47] <- cntry.list[49]
wiki.votes$cntry[48] <- cntry.list[161]
wiki.votes <- slice(wiki.votes, c(1:48, rep(49, each=2), 50:n())) # denmark
wiki.votes$cntry[49] <- cntry.list[52]
wiki.votes$cntry[50] <- cntry.list[83]
wiki.votes <- slice(wiki.votes, c(1:57, rep(58, each=2), 59:n())) # ethiopia+eritrea
wiki.votes$cntry[58] <- cntry.list[61]
wiki.votes$cntry[59] <- cntry.list[59]
wiki.votes$cntry[61] <- cntry.list[63]
wiki.votes$cntry[64] <- cntry.list[68]
wiki.votes$cntry[65] <- cntry.list[69]
wiki.votes$cntry[76] <- cntry.list[82]
wiki.votes <- wiki.votes[-82,]  # no isle of man in wpp 
wiki.votes <- wiki.votes[-86,]  # no jersey in wpp 
rownames(wiki.votes) <- 1:192
wiki.votes$cntry[87] <- cntry.list[94]
wiki.votes$cntry[90] <- cntry.list[50]
wiki.votes$cntry[91] <- cntry.list[145]
wiki.votes$cntry[93] <- cntry.list[98]
wiki.votes$cntry[94] <- cntry.list[99]
wiki.votes$cntry[105] <- cntry.list[109]
wiki.votes <- wiki.votes[-109,]  # no marshall isles in wpp 
rownames(wiki.votes) <- 1:191
wiki.votes$cntry[112] <- cntry.list[118]
wiki.votes$cntry[113] <- cntry.list[146]
wiki.votes$cntry[115] <- cntry.list[119]
wiki.votes$cntry[117] <- cntry.list[122]
wiki.votes <- wiki.votes[-119,]  # no nauru in wpp 
rownames(wiki.votes) <- 1:190
wiki.votes$cntry[119] <- cntry.list[125]
wiki.votes <- wiki.votes[-121,]  # no neth antilles in wpp 
wiki.votes <- wiki.votes[-128,]  # no palau in wpp 
wiki.votes <- wiki.votes[-133,]  # no pitcairn in wpp 
rownames(wiki.votes) <- 1:187
wiki.votes$cntry[138] <- cntry.list[149]
wiki.votes <- wiki.votes[-142,]  # no san marino in wpp 
rownames(wiki.votes) <- 1:186
wiki.votes$cntry[142] <- cntry.list[154]
wiki.votes$cntry[151] <- cntry.list[168]
wiki.votes$cntry[156] <- cntry.list[174]
wiki.votes$cntry[157] <- cntry.list[175]
wiki.votes <- wiki.votes[-158,]  # no taiwan in wpp 
rownames(wiki.votes) <- 1:185
wiki.votes$cntry[158] <-  cntry.list[176]
wiki.votes$cntry[167] <-  cntry.list[185]
wiki.votes <- wiki.votes[-168,]  # no tuvalu in wpp 
rownames(wiki.votes) <- 1:184
wiki.votes$cntry[169] <-  cntry.list[187]
wiki.votes$cntry[172]<-  cntry.list[191]
wiki.votes$cntry[175] <- cntry.list[194]
wiki.votes <- wiki.votes[-177,]  # no vatican in wpp 
rownames(wiki.votes) <- 1:183
wiki.votes$cntry[177] <- cntry.list[196]
wiki.votes$cntry[178]<- cntry.list[197]
wiki.votes$cntry[179] <- cntry.list[199]
wiki.votes <- wiki.votes[-180,]  # no absorb south yemen in wpp 
rownames(wiki.votes) <- 1:182
wiki.votes$cntry[180]
wiki.votes$cntry[181] <- cntry.list[201]
wiki.votes <- slice(wiki.votes, c(1:181, rep(182, each=6))) # yugoslavia
wiki.votes$cntry[182]<- cntry.list[162] # slov
wiki.votes$cntry[183] <- cntry.list[157] # serb
wiki.votes$cntry[184] <- cntry.list[177] # mac
wiki.votes$cntry[185] <- cntry.list[120] #mntg
wiki.votes$cntry[186] <- cntry.list[45] # cro
wiki.votes$cntry[187] <- cntry.list[22] # bih


## now clean up the years - taking latest year (regardelss of naitonal or local, so the 
## year when full voting rights are given)
wiki.votes$year[6] <- 1917
wiki.votes$year[12] <- 1971
wiki.votes$year[19] <- 1948
wiki.votes$year[23] <- 1952
wiki.votes$year[27] <- 1944
wiki.votes$year[34] <- 1975
wiki.votes$year[53] <- 1967
wiki.votes$year[55] <- 1950
wiki.votes$year[68] <- 1952
wiki.votes$year[69] <- 1965
wiki.votes$year[78] <- 1945
wiki.votes$year[81] <- 1922
wiki.votes$year[83] <- 1945
wiki.votes$year[92] <- 2005
wiki.votes$year[113] <- 1940
wiki.votes$year[118] <- 1989
wiki.votes$year[128] <- 1946
wiki.votes$year[134] <- 1976
wiki.votes$year[135] <- 1935
wiki.votes$year[137] <- 1946
wiki.votes$year[149] <- 1994
wiki.votes$year[156] <- 1991
wiki.votes$year[166] <- 1934
wiki.votes$year[171] <- 1928
wiki.votes$year[174] <- 1927
wiki.votes$year[180] <- 1962
wiki.votes$year <- as.numeric(wiki.votes$year)

write.csv(wiki.votes, file = "data/wiki.votes.clean.csv")

## filter out countries witout voting data
###############################################################################

full.data %>%
  filter(cntry %in% wiki.votes$cntry) -> Xtest

