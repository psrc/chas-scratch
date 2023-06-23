# Function to assemble Income table

source('config.R')

# will need to create separate function for data from 2010-2014 (table changes)

# 2015-2019 data
create_income_table <- function() {
  # gather T1 table
  
  chas_tables <- c('T1')
  dfs <- gather_tables(chas_tables)
  
  # Assemble Table ----
  
  desc <- c('Extremely Low Income (<30% AMI)', 
            'Very Low Income (30-50% AMI)', 
            'Low Income (50-80% AMI)', 
            'Moderate Income (80-100% AMI)', 
            'Greater than 100% of AMI', 
            'All')
  
  # Table 1 
  t1_head_hispanic <- c(10, 17, 24, 31, 38, "")
  names(t1_head_hispanic) <- desc
  t1_hispanic <- dfs$t1_hispanic[sort %in% t1_head_hispanic, ]
  t1_hispanic <- t1_hispanic[, `:=`(sort = factor(sort, levels = t1_head_hispanic), col_desc = 'Hispanic or Latino')][order(sort)]
  t1_hispanic$description <- names(t1_head_hispanic)[t1_hispanic$sort]
  
 
  # select common columns
  cols <- c('variable_name', 'sort','chas_year', 'geography_name', 'estimate', 'moe',  'col_desc', 'description')
  ra_dfs <- map(list(t1_hispanic,t1_black), ~.x[, ..cols])
  df <- rbindlist(ra_dfs)
  
  ## Format Table ----
  
  # pivot wider
  df <- dcast.data.table(df, chas_year + geography_name + description ~ col_desc, value.var = 'estimate')
  
  # reorder rows
  df <- df[, description := factor(description, levels = desc)][order(description)]
  
  # order columns
  setcolorder(df, c('chas_year', 'geography_name', 'description', 'Hispanic or Latino', 'Black or African American', 'Asian', 'Pacific Islander', 'American Indian or Alaska Native', 'White'))
  
#   # Calculate Table ----
#   df_ra <- df[, rental_units := rental_unit_affordability + vacant_rental_units]
#   
#   # create column with denominator
#   df_tot <- dcast.data.table(df_ra, chas_year + geography_name ~ description, value.var = c('renter_hh_income', 'rental_units'), subset = .(description == 'All'))
#   df_ra <- merge(df_ra, df_tot, by = c('chas_year', 'geography_name'))
#   
#   # calculate shares
#   df_ra[, `:=` (renter_hh_income_share = renter_hh_income/renter_hh_income_All, rental_units_share = rental_units/rental_units_All)]
 }