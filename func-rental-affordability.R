# Function to assemble Rental Affordability table

source('config.R')

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