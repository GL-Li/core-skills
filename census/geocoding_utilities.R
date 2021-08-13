library(data.table)
library(magrittr)

# Step 1: from addresses to (lon, lat) coordinates ===========================
# 
# These functions use `tidygeocoder` package to call Arcgis API. It takes about 
# 1 mins to geocode 100 addresses.

arcgis_geocode_mini <- function(data, address, 
                                save_as = "tmp.csv"){
    # To geocode a small number of address to (lon, lat) coordinates. The 
    # coordinates are appended to the original data as two new columns and saved 
    # as a csv file.
    #
    # Arguments:
    # - data: data frame containing an address column. The addresses are in the 
    #   format like "123 Hope St,Boston,MA,05678,US". Missing values are allowed
    #   in places other than street addresses. Works best when the data frame has
    #   about 70 addresses. Much slower with larger number of addresses.
    # - address: string, column name of the address column.
    # - save_as: string, file path to save the results.
    #
    # Return: data frame with geocoded coordinates
    
    # slow process. Monitor the time.
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
    
    return(invisible(res))
}


arcgis_geocode <- function(data, 
                           address,
                           mini_batch = 70,
                           save_dir = "tmp/",
                           start_from = 1){
    # To geocode a large number of address to (lon, lat) coordinates. The 
    # coordinates are appended to the original data as two new columns and saved 
    # as multiple csv files in a directory.
    #
    # Arguments:
    # - data: data frame containing an address column. The addresses are in the 
    #   format like "123 Hope St,Boston,MA,05678,US". Missing values are allowed
    #   in places other than street addresses. 
    # - address: string, column name of the address column.
    # - mini_batch: integer, number of rows of the data frame fed to function
    #   arcgis_geocode_mini().
    # - save_dir: string, directory to save the geocoded coordinates.
    # - start_from: which batch to start from. It allows continuing geocoding 
    #   from last batch in case the process is interrupted.
    #
    # Return: NULL
    
    n_minibatch <- nrow(data) %/% mini_batch
    remainder <- nrow(data) %% mini_batch
    
    for (i in start_from:n_minibatch){
        print(paste0(i, "/", n_minibatch))
        file_name <- paste0(save_dir, "batch_", i, ".csv")
        arcgis_geocode_mini(data[(mini_batch * (i - 1) + 1):(mini_batch * i), ], 
                            address,
                            save_as = file_name)
    }
    
    if (remainder > 0){
        arcgis_geocode_mini(
            data[(mini_batch * n_minibatch + 1):(mini_batch * n_minibatch + remainder), ],
            address,
            save_as = paste0(save_dir, "batch_", n_minibatch + 1, ".csv")
        )
    }
}



# Step 2: from (lon, lat) to block geoid ======================================

# At this moment we geocode census 2010 geoids. Pay attention to the new 2020 
# geoids when they are available.
# 
# This part uses `censusxy` package.

get_block_geoid_single <- function(lon, lat, vintage = "Census2010_Current"){
    # To query the block geoid of a single (lon, lat) coordinate
    # 
    # Arguments:
    # - lon, lat: numeric
    # - vintage: same as vintage in censusxy::cxy_geography
    #
    # Return: string, census block geoid like "10000US440116543211002"
    
    if (is.na(lon) || is.na(lat)){
        return(NA)
    }
    
    geoid <- censusxy::cxy_geography(lon, lat, vintage = vintage) %>%
        .[1, "Census.Blocks.GEOID"] 
    
    if (length(geoid) == 0){
        return(NA)
    }
    
    paste0("10000US", geoid)
}

# # example
# block_2020 = get_block_geoid_single(lon = -110.23324, lat = 38.63593, "Census2020_Current")
# block_2010 = get_block_geoid_single(lon = -110.23324, lat = 38.63593)

get_block_geoid_mini <- function(data, lon = "lon", lat = "lat", 
                                 vintage = "Census2010_Current",
                                 save_as = "tmp.csv"){
    # To geocode a small number of (lon, lat) coordinates into block geoids. The 
    # geoids are appended to the original data as a new column and saved 
    # as a csv file.
    #
    # Arguments:
    # - data: data frame containing two columns for longitude and latitude. 
    #   Works best when the data frame has about 70 addresses. Much slower with
    #   with larger number of coordinates.
    # - lon, lat: string, column name of longitude ant latitude.
    # - vintage: same as vintage in censusxy::cxy_geography
    # - save_as: string, file path to save the results.
    #
    # Return: data frame with encoded block geoids
    
    t1 <- Sys.time()
    res = c()
    for (i in 1:nrow(data)){
        geoid <- get_block_geoid_single(data[[lon]][i], data[[lat]][i], vintage)
        res <- c(res, geoid)
    }
    data$block_geoid <- res
    
    t2 <- Sys.time()
    print(t2 - t1)
    
    fwrite(data, file = save_as)
    return(invisible(data))
}


# bbb <- get_block_geoid_mini(lon_lat[1:10,])


census_geocode_lon_lat <- function(data, 
                                   lon = "lon", 
                                   lat = "lat",
                                   vintage = "Census2010_Current",
                                   mini_batch = 70,
                                   saved_dir = "tmp/",
                                   start_from = 1,
                                   file_name = NULL){
    # To geocode a large number of (lon, lat) coordinates to block geoids. The 
    # block geoids are appended to the original data as a new column and saved 
    # as multiple csv files in a directory.
    #
    # Arguments:
    # - data: data frame containing longitude and latitude columns. 
    # - lon, lat: string, column name of lon and lat.
    # - vintage: same as vintage in censusxy::cxy_geography
    # - mini_batch: integer, number of rows of the data frame fed to function
    #   arcgis_geocode_mini().
    # - save_dir: string, directory to save the geoids.
    # - start_from: which batch to start from. It allows continuing geocoding 
    #   from last batch in case the process is interrupted.
    #
    # Return: NULL
    
    n_minibatch <- nrow(data) %/% mini_batch
    remainder <- nrow(data) %% mini_batch
    
    for (i in start_from:n_minibatch){
        print(paste0(i, "/", n_minibatch))
        
        if (is.null(file_name)){
            file_name <- paste0(saved_dir, "batch_", i, ".csv")
        }
        
        get_block_geoid_mini(data[(mini_batch * (i - 1) + 1):(mini_batch * i), ], 
                             lon, lat, vintage,
                             save_as = file_name)
    }
    
    if (remainder > 0){
        get_block_geoid_mini(
            data[(mini_batch * n_minibatch + 1):(mini_batch * n_minibatch + remainder), ],
            lon, lat, vintage,
            save_as = paste0(saved_dir, "batch_", n_minibatch + 1, ".csv")
        )
    }
}

lon_lat_to_geoid_concurrent <- function(batch_dir_from, batch_dir_to, 
                                        n_batch, start_from = 1){
    # geocode (lon, lat) to block geoid while (lon, lat) batch files are being
    # created by arcgis_geocode(). 
    #
    # Arguments:
    # - batch_dir_from: directory where geocoded (lon, lat) files are saved
    # - batch_dir_to: directory where geocided block geoid files are saved
    # - n_batch: number of file expected in batch_dir_from
    # - start_from: which batch to start from 
    
    for (i in start_from:n_batch){
        print(paste0(i, " / ", n_batch))
        fpath <- paste0(batch_dir_from, "batch_", i, ".csv")
        while(!file.exists(fpath)){
            message("Waiting for new (lon, lat) file: ", fpath)
            Sys.sleep(30)
        }
        
        lon_lat <- fread(fpath)
        
        fname <- paste0(batch_dir_to, "batch_", i, ".csv")
        census_geocode_lon_lat(lon_lat, 
                               saved_dir = batch_dir_to,
                               file_name = fname)
    }
}


# Helper functions =============================================================

read_dir <- function(data_dir){
    # read all files in a directory into data.table
    
    fnames <- paste0(data_dir, list.files(data_dir))
    
    lst <- vector(mode = "list", length = length(fnames))
    
    for (i in 1:length(fnames)){
        if (i %% 1000 == 0){
            print(i)
        }
        lst[[i]] <- fread(fnames[i])
    }
    
    return(rbindlist(lst))
}