library(RSQLite)
library(odbc)
library(DBI)
library(tidyverse)
library(data.table)

sqldb.connect <- function() {
  # connect to SQLite db
  
  dbConnect(SQLite(), "chas_2023-07-26.db")
}


read.dt <- function(type = c('table', 'query'), string) {
  # read table
  
  con <- sqldb.connect()
  if(type == 'table') dt <- dbReadTable(con, string)
  if(type == 'query') dt <- dbGetQuery(con, string)
  dbDisconnect(con)
  setDT(dt)
}

gather_tables <- function(juris = c('place', 'tract', 'county'), chas_table_codes) {
  # gather CHAS tables of interest and store in a named list
  
  exp <- paste0('chas.', juris, '_', chas_table_codes)
  
  dfs <- map(exp, ~read.dt('table', .x))
  dfs <- map(dfs, ~.x[, sort := as.numeric(str_extract(variable_name, "\\d*$"))][order(sort)])
  names(dfs) <- chas_table_codes
  return(dfs)
}
