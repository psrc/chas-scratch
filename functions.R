library(tidyverse)
library(data.table)
library(openxlsx)
library(odbc)
library(DBI)

# connect to Elmer
db.connect <- function(adatabase) {
  elmer_connection <- dbConnect(odbc(),
                                driver = "SQL Server",
                                server = "AWS-PROD-SQL\\Sockeye",
                                database = adatabase,
                                trusted_connection = "yes"
  )
}

# read table
read.dt <- function(adatabase, type = c('table', 'query'), string) {
  elmer_connection <- db.connect(adatabase)
  if(type == 'table') dtelm <- dbReadTable(elmer_connection, SQL(atable))
  if(type == 'query') dtelm <- dbGetQuery(elmer_connection, SQL(string))
  dbDisconnect(elmer_connection)
  setDT(dtelm)
}

# read-in CHAS data dictionary
sheets <- c('1', '8', '9', '15C', '14B')

compile_chas <- function(geography, sheets) {
  filename <- 'CHAS data dictionary 15-19.xlsx'
  
  sumlevel <- switch(geography,
                     'place' = '160',
                     'county' = '050')
  lookups <- list()
  
  for(i in 1:length(sheets)) {
    lu <- read.xlsx(file.path(sumlevel, filename), sheet = paste('Table', sheets[i]))
    setDT(lu)
    
    lu_moe <- copy(lu) 
    lu_moe[, Column.Name := str_replace_all(Column.Name, 'est', 'moe')]
    l <- rbindlist(list(lu, lu_moe))
    lookups[[i]] <- l
  }
  
  dir <- switch(geography,
                'place' = 'C:\\Users\\CLam\\github\\chas\\160',
                'county' = 'C:\\Users\\CLam\\github\\chas\\050')
 
  tables <- paste0('Table', sheets)
  
  dfs <- map(tables, ~fread(file.path(dir, paste0(.x, '.csv'))))
  names(dfs) <- tables
  
  if(geography == 'place') {

    dfs <- map(dfs, ~.x[st == 53, ][, geoid_short := str_extract(geoid, '(?<=S).*')])
    
    # join with Elmer, filter for PSRC places
    sql_string <- "SELECT * FROM census.geography_dim WHERE summary_level = 160;"
    place_lu <- read.dt('Elmer', type = 'query', sql_string)
    place_lu <- place_lu[geography_type == 'Place', .(geography_name, geography_type, geography_type_abbreviation, place_geoid)][, place_geoid := str_trim(place_geoid)]
    
    dfs <- map(dfs, ~merge(.x, place_lu, by.x = 'geoid_short', by.y = 'place_geoid'))
    
  } else if(geography == 'county') {

    dfs <- map(dfs, ~.x[st == 53 & cnty %in% c(33, 35, 53, 61), ][, geoid_short := str_extract(geoid, '(?<=S).*')])
  }
  
  # pivot long
  tcols <- map(dfs, ~str_subset(colnames(.x), "^T.*"))
  icols <- map2(dfs, tcols, ~setdiff(colnames(.x), .y))
  
  dfs <- pmap(list(dfs, icols, tcols), ~melt.data.table(..1, id.vars = ..2, measure.vars = ..3, variable.name = 'headers', value.name = 'value'))
  
  # join header
  dfs <- map2(dfs, lookups, ~merge(.x, .y, by.x = 'headers', by.y = 'Column.Name'))
  dfs <- map(dfs, ~.x[, sort := as.numeric(str_extract(headers, "\\d*$"))][order(sort)])
  
}

test <- compile_chas('place', sheets)
test2 <- compile_chas('county', sheets)
