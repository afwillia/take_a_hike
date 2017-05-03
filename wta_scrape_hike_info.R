# process hike names into urls
# 11/23/16
#
# Previous script: wta_scrape.R- get html file from each hike website
#
# This script parses each website's html file for information.
# Then cleans up data into a DF. One row for each hike.
#
# Output: wta_scrape_hike_info_hikes.Rda - clean df, one row per hike
#
#----------------------------

# libraries ----------
library(rvest)
library(stringr)

# load data. hike_df contains html of each hike
load("~/Documents/wta_data_scrape/wta_data_scrape_hike_html.Rda")

# information we want from hike page:
# name ------------------- .documentFirstHeading
# length (roundtrip?) ---- #distance span
# elevation gain (ft) ---- #hike-stats :nth-child(3) div:nth-child(2) span
# highest point (ft) ----- #hike-stats .grid_3 div:nth-child(3) span
# rating & votes --------- .star-rating & .rating-count
# coordinates ------------ lat= .latlong span:nth-child(1)
#                         long= .latlong span:nth-child(2)
# pass needed ----------- .alert a
  
hike_text <- lapply(hike_df, function(x) {
  # obtain relevant info from site
  node_list <- list(
       name=html_nodes(x, ".documentFirstHeading"),
       length=html_nodes(x, "#distance span"),
       elevation=html_nodes(x, "#hike-stats :nth-child(3) div:nth-child(2) span"),
       high_pt=html_nodes(x, "#hike-stats .grid_3 div:nth-child(3) span"),
       rating=html_nodes(x, ".star-rating"),
       votes=html_nodes(x, ".rating-count"),
       cord_lat=html_nodes(x, ".latlong span:nth-child(1)"),
       cord_long=html_nodes(x, ".latlong span:nth-child(2)"),
       pass=html_nodes(x, ".alert a")
       )
  
  # parse xml into text
  sapply(node_list, function(y) html_text(y))
})

## turn into data.frame and clean up columns ------------------------------
hikes <- do.call(rbind, hike_text)
hikes <- data.frame(apply(hikes,2,as.character), stringsAsFactors = FALSE)

# turn rating into numeric
ratings <- str_split(hikes$rating, "\n", simplify = TRUE)
ratings <- str_trim(ratings[,2])
ratings <- as.numeric(str_split(ratings, " out of ", simplify = TRUE)[,1])
hikes$rating <- ratings

# lat and long are numeric
hikes$cord_lat <- as.numeric(hikes$cord_lat)
hikes$cord_long <- as.numeric(hikes$cord_long)

# elevation and height are numeric
hikes$elevation <- as.numeric(hikes$elevation)
hikes$high_pt <- as.numeric(hikes$high_pt)

# clean up length and convert one-way lengths to round trip by multiplying by 2
# convert length to km
one_way_inds <- which(str_trim(str_split(hikes$length, ",", simplify = TRUE)[,2]) == "one-way")
hike_length <- str_trim(str_split(hikes$length, ",", simplify = TRUE)[,1])
hike_length <- gsub(" miles", "",hike_length)
hike_length <- gsub("character\\(0\\)", "",hike_length)
hike_length <- as.numeric(hike_length)
hike_length[one_way_inds] <- 2 * hike_length[one_way_inds]
hikes$length <- round(hike_length * 1.60934, digits=1)

# convert elevation and high_pt from feet to meters
hikes$elevation <- round(hikes$elevation * 0.3048, digits=0)
hikes$high_pt <- round(hikes$high_pt * 0.3048, digits=0)

# votes, just keep the number
votes <- str_split(hikes$votes, " vote", simplify = TRUE)[,1]
votes <- as.numeric(gsub("\\(", "",votes))
hikes$votes <- votes

## output data ----------------------------------------------------
save(hikes, file="~/Documents/wta_data_scrape/wta_scrape_hike_info_hikes.Rda")
write.csv(hikes, file="~/Documents/wta_data_scrape/wta_hikes.csv", row.names=FALSE)
