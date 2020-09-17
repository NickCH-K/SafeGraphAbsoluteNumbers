{
  library(data.table)
  library(SafeGraphR)
  library(lubridate)
  library(stringr)
  library(purrr)
}

####### LOCATION MATCHING

# Find list of locations close to a college
dir <- 'C:/Users/nickc/Documents/SafeGraph/2020/08/'
filename = 'Core-USA-August2020-Release-CORE_POI-2020_07-2020-08-07.zip'

f <- paste0(dir, filename)
files_in_zip <- utils::unzip(f, list = TRUE)$Name
files_in_zip <- files_in_zip[grep("\\.csv\\.gz", files_in_zip)]
utils::unzip(f, files = files_in_zip)
locs <- files_in_zip %>% purrr::map(function(x) {
  message(paste("Starting to read", x, "at", 
                Sys.time()))
  patterns <- data.table::fread(x, select = c("safegraph_place_id",
                                              'location_name',
                                              'city',
                                              'region',
                                              'brands',
                                              'poi_cbg'))
  file.remove(x)
  return(patterns)
}) %>% data.table::rbindlist() %>% unique() 

sbux <- locs[brands == 'Starbucks']
sbux <- sbux[,c('safegraph_place_id','poi_cbg'), with = FALSE]
saveRDS(sbux,'starbucks_locs.rdata')

stadiums <- locs[location_name %in% c('State Farm Stadium',
                                      'Mercedes Benz Stadium',
                                      'Bank of America Stadium',
                                      'Soldier Field',
                                      'Soldier Field South Lot',
                                      'Paul Brown Stadium',
                                      'AT&T Stadium',
                                      'Empower Field at Mile High',
                                      'Lambeau Field',
                                      'NRG Stadium',
                                      'Lucas Oil Stadium',
                                      'TIAA Bank Field',
                                      'U S Bank Stadium',
                                      'Arrowhead Stadium',
                                      'SoFi Stadium',
                                      'Hard Rock Stadium',
                                      'Gillette Stadium',
                                      'Mercedes Benz Superdome',
                                      'MetLife Stadium',
                                      'Lincoln Financial Field',
                                      'Heinz Field',
                                      'Levi\'s Stadium',
                                      'CenturyLink Field',
                                      'Raymond James Stadium',
                                      'Nissan Stadium',
                                      'FedEx Field') | 
                   (location_name == 'FirstEnergy Stadium' & region == 'OH') | 
                   (location_name == 'Ford Field' & city == 'Detroit')]
stadiums[location_name == 'Soldier Field South Lot', location_name := 'Soldier Field']
stadiums <- stadiums[!(location_name == 'Soldier Field' & city == 'Monterey')]

saveRDS(stadiums[,c('safegraph_place_id','location_name','poi_cbg'), with = FALSE],'stadium_locs.Rdata')
