{
  library(SafeGraphR)
  library(data.table)
  library(stringr)
  library(purrr)
  library(lubridate)
}


get_week <- function(x, sbux, stad) {
  
  #BLOOP!
  key = 'SECRETKEY'
  secret = 'SECRETSECRET'
  
  # Base directory
  dir <- 'C:/Users/nickc/Documents/SafeGraph/'
  
  # Check most recent patterns file
  list_pats <- list.files(paste0(dir,'patterns'), pattern = '.csv.gz', recursive = TRUE)
  last_date <- list_pats %>% str_sub(1,10) %>% ymd() %>% max()
  
  # We doing old or new
  new <- str_sub(x,5,5) == '/'
  
  if (new) {
    # New!
    floc <- 'patterns/'
    safegraph_aws(dataset = 'weekly-new', prefix = paste0(floc,x), 
                  key = key, secret = secret)
    
    flist <- list.files(pattern = '\\.csv\\.gz')
    
    start_date <- rep(ymd(str_sub(x,1,10)) - days(9),length(flist))
  } else{
    # Old!
    floc <- 'main-file/'
    safegraph_aws(dataset='weekly', prefix =  paste0(floc,str_sub(x,1,10)), 
                  key = key, secret = secret)
    flist <- '-weekly-patterns.csv.gz'
    
    start_date <- ymd(str_sub(x,1,10))
  }
  
  # Read the file(s)
  datast <- list(list(),list())
  
  for (f in 1:length(flist)) {
    x <- fread(flist[f], 
               select = c('safegraph_place_id',
                          'visits_by_day', 
                          'city',
                          'region',
                          'poi_cbg'))
    
    # Get starbucks
    datast[[1]][[f]] <- x[safegraph_place_id %in% sbux$safegraph_place_id]
    datast[[1]][[f]] <- expand_integer_json(datast[[1]][[f]],
                                            expand = 'visits_by_day',
                                            index = 'day',
                                            by = c('safegraph_place_id','poi_cbg'))
    datast[[1]][[f]][,date := start_date + days(day-1)]
    
    # get stadiums
    datast[[2]][[f]] <- merge(x, stad, by = 'safegraph_place_id')
    
    datast[[2]][[f]] <- expand_integer_json(datast[[2]][[f]],
                                            expand = 'visits_by_day',
                                            index = 'day',
                                            by = c('location_name','poi_cbg'))
    datast[[2]][[f]][,date := start_date + days(day-1)]
    
    file.remove(flist[f])
  }
  
  datast[[1]] <- rbindlist(datast[[1]])
  datast[[2]] <- rbindlist(datast[[2]])
  
  return(datast)
}

####### BRING IN PREPREPARED FILES

stadiums <- readRDS('stadium_locs.Rdata')
sbux <- readRDS('starbucks_locs.rdata')


###### Read in norm files

# Now that we have the ability to read each zip file, let's get a list of them
ndir <- 'C:/Users/nickc/Documents/SafeGraph/normalization-stats/'

norm <-  list.files(ndir, pattern = '.csv', recursive = TRUE) %>%
  map(function(x) {
    dt <- read_many_csvs(ndir, filelist = x, makedate = TRUE)
    dt[,filename := x]
    return(dt) 
  }) %>%
  rbindlist(fill = TRUE)

ndir <- 'C:/Users/nickc/Documents/SafeGraph/normalization_stats/'

norm2 <- list.files(ndir, pattern = '.csv', recursive = TRUE) %>%
  map(function(x) {
    dt <- read_many_csvs(ndir, filelist = x, makedate = TRUE)
    dt[,filename := x]
    return(dt) 
  }) %>%
  rbindlist(fill = TRUE)
norm2 <- norm2[is.na(region) | region == 'ALL_STATES']
norm2[, region := NULL]

norm <- rbind(norm, norm2) %>%
  unique()
norm[, filename := str_replace(filename,'normalization_stats.csv','')]
norm[, filename := str_replace(filename,'normalization-stats.csv','weekly-patterns.csv.gz')]

# Just the football season
norm <- norm[date >= as.Date('2019-09-01') & date <= as.Date('2019-12-31')]

saveRDS(norm, 'normalization_data_for_absolute.Rdata')


####### READ IN FILES

weeklist <- unique(norm$filename)

weeks_run <- c()
sbux_data <- list()
stadium_data <- list()

for (w in weeklist) {
  weeks_run <- c(weeks_run, w)
  
  weekdata <- get_week(w, sbux, stadiums)
  
  sbux_data[[w]] <- weekdata[[1]]
  stadium_data[[w]] <- weekdata[[2]]
  
  rm(weekdata)
  
  # Every five weeks, save
  if (length(weeks_run) >= 5 | w == weeklist[length(weeklist)]) {
    daterange <- paste0(
      norm[filename %in% weeks_run]$date %>% min() %>% as.character(),
      '_to_',
      norm[filename %in% weeks_run]$date %>% max() %>% as.character()
    )
    saveRDS(rbindlist(sbux_data), paste0('sbux_',daterange,'.Rdata'))
    
    stadium_data <- rbindlist(stadium_data)
    if (nrow(stadium_data) > 0) {
      saveRDS(stadium_data, paste0('stadium_',daterange,'.Rdata'))
    }
    
    weeks_run <- c()
    sbux_data <- list()
    stadium_data <- list()
  }
}

# and get the panel summary files
summ <- read_many_csvs('C:/Users/nickc/Documents/SafeGraph/home-summary-file/',
                       filelist =  paste0(str_sub(weeklist,1,10),'-home-panel-summary.csv'))
saveRDS(summ, 'summary_file_absolute_values.Rdata')
