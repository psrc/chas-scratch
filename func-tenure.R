# Function to assemble Tenure by R/E table

source('config.R')

create_tenure_table <- function() {
  # grab Table9
  
  chas_table <- c('T9')
  dfs <- gather_tables(chas_table)
  
  re_order <- c('American Indian or Alaskan Native', 
             'Asian', 
             'Black or African American', 
             'Hispanic or Latino (of any race)', 
             'Other Race', 
             'Pacific Islander',
             'White',
             'All')
  
  # Assemble Table ----
  
  # Table 9 Owner-Occupied 
  t9_oo_head <- c(18,13,8,28,33,23,3,2)
  
  t9_oo <- dfs$T9[sort %in% t9_oo_head, ]
  t9_oo <- t9_oo[, `:=`(sort = factor(sort, levels = t9_oo_head), col_desc = 'owner_occupied')][order(sort)]

  # Table 9 Renter-Occupied
  t9_ro_head <- c(54,49,44,64,69,59,39,38)
  
  t9_ro <- dfs$T9[sort %in% t9_ro_head, ]
  t9_ro <- t9_ro[, `:=`(sort = factor(sort, levels = t9_ro_head), col_desc = 'renter_occupied')][order(sort)]

  # select common columns
  cols <- c('variable_name', 'sort','chas_year', 'geography_name', 'estimate', 'moe', 'race_ethnicity', 'col_desc')
  ra_dfs <- map(list(t9_oo,t9_ro), ~.x[, ..cols])
  df <- rbindlist(ra_dfs)
  
  # Format Race/Ethnicity data
  df$description <- df$race_ethnicity
  df <- df %>% 
    mutate(description=factor(case_when(grepl("^American Indian ", description) ~"American Indian or Alaskan Native",
                                        grepl("^Asian ", description) ~"Asian",
                                        grepl("^Black ", description) ~"Black or African American",
                                        grepl("^Hispanic, any race", description) ~"Hispanic or Latino (of any race)",
                                        grepl("^other ", description) ~"Other Race",
                                        grepl("^Pacific ", description) ~"Pacific Islander",
                                        grepl("^White ", description) ~"White",
                                        grepl("^All", description) ~"All",
                                        !is.na(description) ~ "")))
  
  ## Format Table ----
  
  # pivot wider
  df <- dcast.data.table(df, chas_year + geography_name + description ~ col_desc, value.var = 'estimate')
  
  # reorder rows
  df <- df[, description := factor(description, levels = re_order)][order(description)]
  
  # order columns
  setcolorder(df, c('chas_year', 'geography_name', 'description', 'owner_occupied', 'renter_occupied'))
  
  # Calculate Table ----
  df_ra <- df[, all_units := owner_occupied + renter_occupied]
  
  # calculate shares
  df_ra$renter_share <- df_ra$renter_occupied/df_ra$all_units
  df_ra$owner_share <- df_ra$owner_occupied/df_ra$all_units
}