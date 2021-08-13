library(censusxy)
library(tidygeocoder)
library(dplyr)


census_geocode_mini <- function(data, 
                                id = NULL,
                                street,
                                city = NULL,
                                state = NULL,
                                zip = NULL,
                                return = "geographies",
                                vintage = "Census2010_Current",
                                n_repeat = 4, 
                                n_jobs = 6,
                                save_as = "tmp.csv"){
  # To geocode the geograhies of a small number of address. The Census Geocoder  
  # API is very slow when submit a large batch. 
  #
  # Argument:
  # - data: data.frame containing columns for addresses, with about 70 rows
  # - n_repeat: repeated time without improvement before stopping query
  # - n_job: cores for parallel query
  # - save_as: save the geocided data into a csv file.
  # - other arguments: check cxy_geocode function
  
  t1 <- Sys.time()
  N0 <- nrow(data)
  
  # create a placeholder for geocoded data
  df0 <- data[FALSE, ]
  df1 <- structure(list(cxy_lon = numeric(0), 
                        cxy_lat = numeric(0), 
                        cxy_state_id = integer(0), 
                        cxy_county_id = integer(0), 
                        cxy_tract_id = integer(0), 
                        cxy_block_id = integer(0)), 
                   row.names = integer(0), 
                   class = "data.frame")
  
  matched <- cbind(df0, df1)
  
  # repeated geocoding until not impovement after n_repeat tries
  keep_downloading = TRUE
  i_round <- 0
  i_repeat <- 0
  while(keep_downloading){
    i_round <- i_round + 1
    print(i_round)
    downloaded <- cxy_geocode(data, id, street, city, state, zip,
                              output = "simple", 
                              return = return, 
                              vintage = vintage, 
                              parallel = n_jobs)
    
    matched <- rbind(matched, downloaded[!is.na(downloaded$cxy_lon), ])
    
    print(paste0("---- unmatched: ", N0 - nrow(matched), " ----"))
    
    if (nrow(matched) < N0){
      no_match <- sum(is.na(downloaded$cxy_lon))
      
      if (no_match == nrow(data)){
        i_repeat <- i_repeat + 1
        print(paste0("i_repeat = ", i_repeat))
        if (i_repeat >= n_repeat){
          keep_downloading <- FALSE
        }
      } else {
        i_repeat <- 0
      }
      # new data
      data <- downloaded[is.na(downloaded$cxy_lon), names(df0)]
    } else {
      keep_downloading = FALSE
    }
  }
  
  t2 <- Sys.time()
  print(t2 - t1)
  
  message(N0 - nrow(matched), " addresses have no match")
  
  if (nrow(matched) == N0){
    res <- matched
  } else {
    res <- rbind(matched, downloaded)
  }
  
  res <- res %>%
    rename(lon = cxy_lon,
           lat = cxy_lat) %>%
    mutate(block_geoid = if_else(
      is.na(lon), 
      NA_character_, 
      paste0("10000US", cxy_state_id, cxy_county_id, cxy_tract_id, cxy_block_id)
    ))
  
  data.table::fwrite(res, file = save_as)
  return(res)
}

# # # example
# aaa = census_geocode_mini(stl_homicides[1:70, ],
#                           street = "street_address",
#                           city = "city",
#                           state = "state")


census_geocode <- function(data,
                           id = NULL,
                           street,
                           city = NULL,
                           state = NULL,
                           zip = NULL,
                           return = "geographies",
                           vintage = "Census2010_Current",
                           n_repeat = 4, 
                           n_jobs =6,
                           mini_batch = 70,
                           saved_dir = "tmp/",
                           start_from = 1){
  # To geocode a large number of addresses. The dataframe is broken into  
  # n_minibatch small dataframe and queried and saved as csv files separated. 
  #
  # Argument:
  # - mini_batch: number of row of a mini_batch
  # - saved_dir: directory to save the geocoded data
  # - start_from: integer, the first batch to start from. Used when connection
  #   broken.
  # - other arguments: see census_geocode_mini function
  
  n_minibatch <- nrow(data) %/% mini_batch
  remainder <- nrow(data) %% mini_batch
  
  for (i in start_from:n_minibatch){
    print(paste0(i, "/", n_minibatch))
    file_name <- paste0(saved_dir, "batch_", i, ".csv")
    census_geocode_mini(data[(mini_batch * (i - 1) + 1):(mini_batch * i), ], 
                        id, street, city, state, zip,
                        return = return, 
                        vintage = vintage, 
                        n_repeat = n_repeat,
                        n_jobs = n_jobs,
                        save_as = file_name)
  }
  
  if (remainder > 0){
    census_geocode_mini(
      data[(mini_batch * n_minibatch + 1):(mini_batch * n_minibatch + remainder), ],
      id, street, city, state, zip,
      return = return, 
      vintage = vintage, 
      n_repeat = n_repeat,
      n_jobs = n_jobs,
      save_as = paste0(saved_dir, "batch_", n_minibatch + 1, ".csv")
    )
  }
}

# # example
# census_geocode(stl_homicides[1:100,],
#                street = "street_address",
#                city = "city",
#                state = "state",
#                saved_dir = "tmp/")

# 
# dat <- batch_3 %>%
#   mutate(address = paste0(
#   AddressFirstLine, ", ", City, ", ", StateCode, ", ", ZipCode, ", ", "US"
# ))


arcgis_geocode_mini <- function(data, address, 
                                save_as = "tmp.csv"){
  # geocode single line address to (lon, lat)
  
  t1 <- Sys.time()
  
  res <- data %>%
    as.data.frame() %>%
    tidygeocoder::geocode(
      address = address,
      method = "arcgis",
      lat = "lat",
      long = "lon")
  
  t2 <- Sys.time()
  print(t2 - t1)
  
  data.table::fwrite(res, file = save_as)
  
  return(res)
}

# # example
# lon_lat <- arcgis_geocode(dat[1:100, ],
#                address = address)  


arcgis_geocode <- function(data, 
                           address,
                           mini_batch = 70,
                           saved_dir = "tmp/",
                           start_from = 1){
  # geocode addresses into (lon, lat)
  
  n_minibatch <- nrow(data) %/% mini_batch
  remainder <- nrow(data) %% mini_batch
  
  for (i in start_from:n_minibatch){
    print(paste0(i, "/", n_minibatch))
    file_name <- paste0(saved_dir, "batch_", i, ".csv")
    arcgis_geocode_mini(data[(mini_batch * (i - 1) + 1):(mini_batch * i), ], 
                        address,
                        save_as = file_name)
  }
  
  if (remainder > 0){
    arcgis_geocode_mini(
      data[(mini_batch * n_minibatch + 1):(mini_batch * n_minibatch + remainder), ],
      address,
      save_as = paste0(saved_dir, "batch_", n_minibatch + 1, ".csv")
    )
  }
}

# # example 
# arcgis_geocode(dat[1:100,], address, saved_dir = "arcgis_geocoding_1/")




get_block_geoid_single <- function(lon, lat, vintage = "Census2010_Current"){
  # To query the block geoid of a single coordinate
  if (is.na(lon) || is.na(lat)){
    return(NA)
  }
  geoid <- cxy_geography(lon, lat, vintage = vintage) %>%
    .[1, "Census.Blocks.GEOID"] 
  
  if (length(geoid) == 0){
    return(NA)
  }
  
  paste0("10000US", geoid)
}

# # example
# block_2020 = get_block_geoid_single(lon = -110.23324, lat = 38.63593, "Census2020_Current")
# block_2010 = get_block_geoid_single(lon = -110.23324, lat = 38.63593)

get_block_geoid <- function(data, lon = "lon", lat = "lat", 
                            vintage = "Census2010_Current"){
  # the data has three column, id, lon, and lat
  res = c()
  for (i in 1:nrow(data)){
    print(i)
    print(Sys.time())
    geoid <- get_block_geoid_single(data[[lon]][i], data[[lat]][i], vintage)
    res <- c(res, geoid)
  }
  data$block_geoid <- res
  return(data)
}


bbb <- get_block_geoid(lon_lat)

