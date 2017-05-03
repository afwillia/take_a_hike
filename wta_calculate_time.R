# work with hikes data to .....
# 11/24/2016
#
# Find driving distances to hikes using google maps API
# Merge with hike data and calculate total travel times
# 
#
# -------------------------------------------

## load libraries -----------------------
library(ggplot)
library(ggmap)
source("./google_api_key.R") # google api to use maps

## load data. hikes is a df with one row per hike and various information
load(file="~/Documents/wta_data_scrape/wta_scrape_hike_info_hikes.Rda")

## define functions ------------------------------------

# tobler's hiking function - a way to estimate walking speed based on slope
# returns time in hours
tobler <- Vectorize(
  function(elevation, length) {
    # short distances mess this formula up, so just return 1 hr
    if (is.na(elevation) | is.na(length)) return(NA)
    if (length <= 2) return(1)
    # exp(1) = e
    rate <- 6 * exp(1) ^ (-3.5 * abs(elevation/1000 / length + 0.05 )) * 3 / 5
   
    round( length / rate, digits=1)
  })

# naismith's rule for walking speed.
# length is in miles, elevation in feet.
# returns time in hours
naismith <- Vectorize(
  function(elevation, length) {
    time <- ( length / 5) + ( elevation / 600 )
    round(time, digits=1)
  })

# function to create google api call 
# as of now, json and units are hard coded in URL.
# look into using an encoded polyline because it's shorter
query_google <- function(origins="47.648695,-122.322099", destinations, key=google_api_key) {
  paste0(
    "https://maps.googleapis.com/maps/api/distancematrix/json?units=imperial",
    "&origins=", origins,
    "&destinations=", destinations,
    "&key=", key
  )
}

## add time to hike data ----------------------------------------
hikes$time_naismith <- naismith(hikes$elevation, hikes$length)
hikes$time_tobler <- tobler(hikes$elevation, hikes$length)

# compare two algorithms. tobler is often much higher than naismith
table(abs(hikes$time_naismith - hikes$time_tobler))

## Query google api for travel times ---------------------------------
# subset hikes with coordinates. we'll visit hikes w/out them later
cord_dat <- hikes[!is.na(hikes$cord_lat) & !is.na(hikes$cord_long),]

# create string of coordinates in the form of "lat1,lon1|lat2,lon2|...|latn,lonn"
origin_cords <- paste(cord_dat$cord_lat, cord_dat$cord_long, sep = ",", collapse='|')

# for testing, let's try 25 cords.
# google free api only allows for 25 destinations at a time, so loop through!
# test_cords <- paste(cord_dat$cord_lat, cord_dat$cord_long, sep = ",")[1:25]
# test_cords <- paste(test_cords, collapse='|')
# 
# # use gepaf package to create polyline encoding for long list of cords
# #test_cords <- gepaf::encodePolyline(cord_dat[,c('cord_lat','cord_long')], factor = 5)
# 
# test_url <- query_google(destinations=test_cords, key=google_api_key)
# dat <- getURL(test_url)
# google_dat <- jsonlite::fromJSON(dat)

# # loop through good cords in increments of 25 to get distances from google
# api_hits <- 25
# ind <- seq(1, nrow(cord_dat), by=api_hits)
# dist_all <- list()
# length(dist_all) <- length(ind)
# distance_df_all <- lapply(ind, function(index){
#   message(index)
#   end <- min(index+api_hits, nrow(cord_dat))
#   test_cords <- paste(cord_dat$cord_lat, cord_dat$cord_long, sep = ",")[index:end]
#   test_cords <- paste(test_cords, collapse='|')
#   
#   test_url <- query_google(destinations=test_cords, key=google_api_key)
#   dat <- getURL(test_url)
#   google_dat <- jsonlite::fromJSON(dat)
#   
#   if (length(google_dat) == 4) {
#   
#   dist_all[[which(ind==index)]] <<- data.frame(cbind(dest_address=google_dat$destination_addresses,
#                                     distance=data.frame(google_dat$rows[[1]])$distance$text,
#                                     duration=data.frame(google_dat$rows[[1]])$duration$text,
#                                     status=data.frame(google_dat$rows[[1]])$status),
#                               stringsAsFactors = FALSE)
#   
#   data.frame(cbind(dest_address=google_dat$destination_addresses,
#       distance=data.frame(google_dat$rows[[1]])$distance$text,
#       duration=data.frame(google_dat$rows[[1]])$duration$text,
#       status=data.frame(google_dat$rows[[1]])$status),
#       stringsAsFactors = FALSE)
#   } else message('Skipping records ', index, '-', index+api_hits, ' in hike dataframe.\n')
#   
#   if (!is.null(jsonlite::fromJSON(dat)$error_message)) message(jsonlite::fromJSON(dat)$error_message)
#     
# })
# 
# dist_all_df <- do.call(rbind, dist_all)
# save(dist_all_df, file="~/Documents/wta_data_scrape/data/dist_all_df.Rda")

# test google api loop

# make empty list to fill with google data
ind <- seq(1, nrow(cord_dat))
drive_dist_list <- list()
length(drive_dist_list) <- length(ind)

drive_dist_loop <- lapply(ind, function(index){
  
  test_cords <- paste(cord_dat$cord_lat, cord_dat$cord_long, sep = ",")[index]
  
  test_url <- query_google(destinations=test_cords, key=google_api_key)
  dat <- getURL(test_url)
  google_dat <- jsonlite::fromJSON(dat)
  
  if (length(google_dat) == 4) {
    
    drive_dist_list[[which(ind==index)]] <<- data.frame(cbind(dest_address=google_dat$destination_addresses,
                                                              distance=data.frame(google_dat$rows[[1]])$distance$text,
                                                              duration=data.frame(google_dat$rows[[1]])$duration$text,
                                                              status=data.frame(google_dat$rows[[1]])$status),
                                                        stringsAsFactors = FALSE)
    
    data.frame(cbind(dest_address=google_dat$destination_addresses,
                     distance=data.frame(google_dat$rows[[1]])$distance$text,
                     duration=data.frame(google_dat$rows[[1]])$duration$text,
                     status=data.frame(google_dat$rows[[1]])$status),
               stringsAsFactors = FALSE)
  } else {
    #message('Skipping record ', index, ' in hike dataframe.\n')
    
    drive_dist_list[[which(ind==index)]] <<- data.frame(cbind(dest_address=NA,
                                                              distance=NA,
                                                              duration=NA,
                                                              status="FAILED"),
                                                        stringsAsFactors = FALSE)
    
    data.frame(cbind(dest_address=NA,
                     distance=NA,
                     duration=NA,
                     status="FAILED"),
               stringsAsFactors = FALSE)
  }
  
  #if (!is.null(jsonlite::fromJSON(dat)$error_message)) message(jsonlite::fromJSON(dat)$error_message)
  
})

#some have status OK, but did not return results from google api, subset those out
drive_dist_df <- do.call(rbind, drive_dist_loop[sapply(drive_dist_list, length) == 4])
save(drive_dist_df, file='~/Documents/wta_data_scrape/data/drive_distances_df.Rda')


# output from google api is a list of length 4. "rows" element contains distance, duration, and status info
# distance and duration elements are lists containing "text" and "value". not sure what value does
distance_df <- drive_dist_df

# convert duration from "x hours y mins" to X in hours so it matches hikes$time
drive2 <- str_split(distance_df$duration, " hour", simplify=TRUE)
# observations with no "mins" label are in hours, so multiply by 60
drive2[-grep('mins', drive2)] <- as.numeric(drive2[-grep('mins', drive2)]) * 60
# strip mins label from mins observations 
drive2[grep('mins', drive2)] <- as.numeric( gsub('\\D', '', drive2[grep('mins', drive2)]) )
# convert matrix to df and character to numeric
drive2 <- data.frame(drive2, stringsAsFactors = FALSE)
drive2 <- sapply(drive2, as.numeric)
# set NAs to 0 for addition
drive2[is.na(drive2)] <- 0
drive_hours <- round((drive2[,1] + drive2[,2]) / 60, digits=1)

distance_df$drive_time <- drive_hours

# put hikes and distance df together
hikes_times <- cbind(cord_dat[sapply(drive_dist_list, length) == 4,], distance_df)
hikes_times <- dplyr::mutate(hikes_times, total_time=drive_time + time_naismith)
save(hikes_times, file="~/Documents/wta_data_scrape/data/hikes_times.Rda")

## Get coordinates from hikes w/out coordinates using ggmap -------------------------
# see some wrong coordinates here (iron creek falls, eg) how to QC?

# bad searches
bad_searches <- which(sapply(dist_all, length) != 4)
bad_rows <- unlist(lapply(ind[bad_searches], function(x) seq(from=x, to=x+api_hits)))
ind[bad_searches]
no_cords <- hikes[is.na(hikes$cord_lat) & is.na(hikes$cord_long),]
no_cords_cords <- geocode(no_cords$name)
no_cords$cord_lat <- no_cords_cords[,2]
no_cords$cord_long <- no_cords_cords[,1]

# add rows with cords to original df
cord_dat <- rbind(cord_dat, no_cords[!is.na(no_cords$cord_lat) & !is.na(no_cords$cord_long),])
still_no_cords <- no_cords[is.na(no_cords$cord_lat) & is.na(no_cords$cord_long),]



