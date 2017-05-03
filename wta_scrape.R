# scrape WTA hikes
# 11/23/2016
#
# 
# Grab hike data from wta.org
# First, grab total number of hikes from search result page, use this for looping
# Second, go through each search result page and grab links from xml file, convert to text
# Third, scrape data from each html link. 
#
# Output: wta_scrape_hike_urls.Rda - clean list of hike URLs
#         wta_data_scrape_hike_html.Rda - list of html files from each website.
#
# Run next: wta_scrape_hike_info.R to parse html
#
#----------------------------------------------

## libraries -----------------------------------
library(rvest)
library(stringr)

## get number of hikes -------------------------------------
link <- "http://www.wta.org/go-hiking/hikes?b_start:int=1"
wta <- read_html(link)

# total number of hikes
hike_bar <- html_nodes(wta, ".search-result-header")
n_hike <- html_text(hike_bar) %>%
  strsplit("\n")
n_hike <- str_trim(n_hike[[1]])[4] # result is in list, hike is 4th item. not ideal coding
n_hike <- as.numeric(gsub("[^[:digit:]]", "", n_hike))
  
## links to hikes from search results --------------------------------------------
# loop through search in increments of 30, grabbing the hike link.
link2 <- "http://www.wta.org/go-hiking/hikes?b_start:int="
max_hits <- 30 # number of search results per page
inds <- 1:ceiling(n_hike/max_hits)
hike_list <- lapply(inds, function(num){
  link <- paste0(link2, (num-1)*max_hits)
  search <- read_html(link)
  urls <- html_attr(html_nodes(search, "a"), "href")
  unique(grep("go-hiking/hikes/", urls, value=TRUE))
})

# clean up urls, remove hike_search links, keeping only real hikes.
hike_urls <- unique(unlist(hike_list))
hike_urls <- hike_urls[-which(hike_urls=="http://www.wta.org/go-hiking/hikes/hike_search?show_adv=1")]
hike_urls <- hike_urls[-which(hike_urls=="http://www.wta.org/go-hiking/hikes/hike_search")]
save(hike_urls, file="~/Documents/wta_data_scrape/wta_scrape_hike_urls.Rda")

## connect to each link and scrape html data ------------------------------
hike_df <- lapply(hike_urls, function(x) read_html(x) )
save(hike_df, file="~/Documents/wta_data_scrape/wta_data_scrape_hike_html.Rda")



