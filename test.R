library(tidyverse)
library(data.table)
library(openxlsx)
library(odbc)
library(DBI)

# Function to assemble Rental Affordability
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

gather_tables <- function(chas_table_codes) {
  # gather CHAS tables of interest and store in a named list
  
  exp <- map(chas_table_codes, ~paste0('execute chas.get_data_by_place','"', .x ,'"' ,', 2019'))
  
  dfs <- map(exp, ~read.dt('Elmer', 'query', .x))
  dfs <- map(dfs, ~.x[, sort := as.numeric(str_extract(variable_name, "\\d*$"))][order(sort)])
  names(dfs) <- chas_table_codes
  return(dfs)
}

create_cost_burden_table <- function() {
  # gather tables T9 to create formatted Cost Burden table
  
  chas_tables <- 'T9'
  dfs <- gather_tables(chas_tables)
  df <- dfs$T9[!(sort %in% c(1, 2, 38))]
  
  # Assemble Table ----
  
  desc <- c("less than or equal to 30%",
            "greater than 30% but less than or equal to 50%",
            "greater than 50%",
            "not computed (no/negative income)",
            "All")
  
  names(desc) <- c("No Cost Burden", "Cost-Burdened (30-50%)", "Severely Cost-Burdened (>50%)", "Not Calculated", "All" )

  # pivot wider
  df[, description := fcase(cost_burden == "less than or equal to 30%", "No Cost Burden",
                            cost_burden == "greater than 30% but less than or equal to 50%", "Cost-Burdened (30-50%)",
                            cost_burden == "greater than 50%", "Severely Cost-Burdened (>50%)",
                            cost_burden ==  "not computed (no/negative income)", "Not Calculated",
                            cost_burden == "All", "All")]
 
  
  # total cost/not-cost burdened (new rows)
  tot_cb <- df[description %in% c("Cost-Burdened (30-50%)", "Severely Cost-Burdened (>50%)"), 
               .(estimate = sum(estimate), cost_burden = 'Total Cost-Burdened', description = 'Total Cost-Burdened'),
               by = c('geography_name', 'geography_type_abbreviation', 'chas_year', 'tenure', 'race_ethnicity')]
  
  tot_ncb <- df[description %in% c('Not Calculated', 'No Cost Burden'), 
                .(estimate = sum(estimate), cost_burden = 'Total Not Cost-Burdened', description = 'Total Not Cost-Burdened'), 
                by = c('geography_name', 'geography_type_abbreviation', 'chas_year', 'tenure', 'race_ethnicity')]
  
  df <- rbindlist(list(df, tot_cb, tot_ncb), use.names = TRUE, fill = TRUE)
  
  # total (for horizontal sum)
  tot <- df[, .(estimate = sum(estimate), race_ethnicity = 'Total'), by = c('geography_name', 'geography_type_abbreviation', 'chas_year', 'tenure', 'cost_burden', 'description')]
 
  # poc (for column)
  poc <- df[race_ethnicity != str_subset(unique(df$race_ethnicity), "^W.*"), .(estimate = sum(estimate), race_ethnicity = 'POC'), 
            by = c('geography_name', 'geography_type_abbreviation', 'chas_year', 'tenure', 'cost_burden', 'description')]
  
  df <- rbindlist(list(df, poc, tot), use.names = TRUE, fill = TRUE)
  
  # factor description
  df[, description := factor(description, levels = c(names(desc), 'Total Cost-Burdened', 'Total Not Cost-Burdened'))]
  race_levels <- c(str_subset(unique(df$race_ethnicity), "^American.*"),
                   str_subset(unique(df$race_ethnicity), "^Asian.*"),
                   str_subset(unique(df$race_ethnicity), "^Black.*"),
                   str_subset(unique(df$race_ethnicity), "^Hispanic.*"),
                   str_subset(unique(df$race_ethnicity), "^Pacific.*"),
                   str_subset(unique(df$race_ethnicity), "^other.*"),
                   'POC',
                   str_subset(unique(df$race_ethnicity), "^White.*"),
                   
                   'Total'
                   )
  df[, race_ethnicity := factor(race_ethnicity, levels = race_levels)]
  
  # add denominator column
  df_denom <- df[cost_burden == 'All', 
                   .(geography_name, geography_type_abbreviation, chas_year, tenure, race_ethnicity, estimate_denom = estimate)]
  df <- merge(df, df_denom, by = c('geography_name', 'geography_type_abbreviation', 'chas_year', 'tenure', 'race_ethnicity')) 
  
  # calculate shares
  df[, share := estimate/estimate_denom]
  
  # calculate estimates
  df_est <- dcast.data.table(df, chas_year + geography_name + tenure + description ~ race_ethnicity, value.var = 'estimate')
  df_share <- dcast.data.table(df, chas_year + geography_name + tenure + description ~ race_ethnicity, value.var = 'share')
  df_share[is.na(df_share)] <- 0
  
  return(list(e = df_est, s = df_share))
}

cb <- create_cost_burden_table()

create_rental_affordability_table <- function() {
  # gather tables T8, T15C, and T14B to create formatted Rental Affordability table
  
  chas_tables <- c('T8', 'T15C', 'T14B')
  dfs <- gather_tables(chas_tables)
  
  # Assemble Table ----
  
  desc <- c('Extremely Low Income (<30% AMI)', 
            'Very Low Income (30-50% AMI)', 
            'Low Income (50-80% AMI)', 
            'Moderate Income (80-100% AMI)', 
            'Greater than 100% of AMI', 
            'All')
  
  # Table 8 
  t8_head <- c(69, 82, 95, 108, 121, 68)
  names(t8_head) <- desc
  
  t8 <- dfs$T8[sort %in% t8_head, ]
  t8 <- t8[, `:=`(sort = factor(sort, levels = t8_head), col_desc = 'renter_hh_income')][order(sort)]
  t8$description <- names(t8_head)[t8$sort]
  
  # Table 15C
  t15c_head <- c(4, 25, 46, 67, 3)
  t15c_desc <- c(desc[1:4], desc[6])
  names(t15c_head) <- t15c_desc
  t15c <- dfs$T15C[sort %in% t15c_head, ]
  t15c <- t15c[, `:=` (sort = factor(sort, levels = t15c_head), col_desc = 'rental_unit_affordability')][order(sort)]
  t15c$description <- names(t15c_head)[t15c$sort]
  
  # Table 14B
  t14b_head <- c(4, 8, 12, 16, 3)
  t14b_desc <- c(desc[1:4], desc[6])
  names(t14b_head) <- t14b_desc
  t14b <- dfs$T14B[sort %in% t14b_head, ]
  t14b <- t14b[, `:=` (sort = factor(sort, levels = t14b_head), col_desc = 'vacant_rental_units')][order(sort)]
  t14b$description <- names(t14b_head)[t14b$sort]
  
  # select common columns
  cols <- c('variable_name', 'sort','chas_year', 'geography_name', 'estimate', 'moe',  'col_desc', 'description')
  ra_dfs <- map(list(t8, t15c, t14b), ~.x[, ..cols])
  df <- rbindlist(ra_dfs)
  
  ## Format Table ----
  
  # pivot wider
  df <- dcast.data.table(df, chas_year + geography_name + description ~ col_desc, value.var = 'estimate')
  
  # reorder rows
  df <- df[, description := factor(description, levels = desc)][order(description)]
  
  # order columns
  setcolorder(df, c('chas_year', 'geography_name', 'description', 'renter_hh_income', 'rental_unit_affordability', 'vacant_rental_units'))
  
  # Calculate Table ----
  df_ra <- df[, rental_units := rental_unit_affordability + vacant_rental_units]
  
  # create column with denominator
  df_tot <- dcast.data.table(df_ra, chas_year + geography_name ~ description, value.var = c('renter_hh_income', 'rental_units'), subset = .(description == 'All'))
  df_ra <- merge(df_ra, df_tot, by = c('chas_year', 'geography_name'))
  
  # calculate shares
  df_ra[, `:=` (renter_hh_income_share = renter_hh_income/renter_hh_income_All, rental_units_share = rental_units/rental_units_All)]
}

ra <- create_rental_affordability_table()

