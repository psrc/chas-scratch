# Function to assemble Income table

source('function-query-sqlite-chas.R')

create_income_table <- function(juris = c('place', 'region')) {

  chas_tables <- 'T1'
  ifelse(juris == 'place', j <- 'place', j <- 'county')
  dfs <- gather_tables(j, chas_tables)

  # Assemble Table ----
  desc <- c('Extremely Low-Income (≤30% AMI)',
            'Very Low-Income (30-50%)',
            'Low-Income (50-80%)',
            'Moderate Income (80-100%)',
            'Above Median Income (>100%)',
            'All')
  
  cols <- c('variable_name', 'sort','chas_year', 'geography_name', 'estimate', 'moe',  'tenure',
            'household_income', 'race_ethnicity', 'income_grp', 'race_ethnicity_grp')
  
  dfs$T1[, income_grp := fcase(household_income == 'All', 'All',
                                household_income == 'less than or equal to 30% of HAMFI', 'Extremely Low-Income (≤30% AMI)',
                                household_income == 'greater than 30% but less than or equal to 50% of HAMFI', 'Very Low-Income (30-50%)',
                                household_income == 'greater than 50% but less than or equal to 80% of HAMFI', 'Low-Income (50-80%)',
                                household_income == 'greater than 80% but less than or equal to 100% of HAMFI', 'Moderate Income (80-100%)',
                                household_income == 'greater than 100% of HAMFI', 'Above Median Income (>100%)')]
  
  dfs$T1[, race_ethnicity_grp := fcase(grepl("^American Indian ", race_ethnicity), "American Indian or Alaskan Native",
                                        grepl("^Asian ", race_ethnicity), "Asian",
                                        grepl("^Black ", race_ethnicity), "Black or African American",
                                        grepl("^Hispanic, any race", race_ethnicity), "Hispanic or Latino (of any race)",
                                        grepl("^Pacific ", race_ethnicity), "Pacific Islander",
                                        grepl("^White ", race_ethnicity), "White",
                                        grepl("^All", race_ethnicity), "All Races")]
  
  # exclude high level totals
  df <- dfs$T1[!(sort %in% c(1, 2, 75)),]

  df <- df[, ..cols][, income_grp := factor(income_grp, levels = desc)]
  
  # df <- df[geography_name == 'Bellevue', ..cols
  #          ][, income_grp := factor(income_grp, levels = desc)
  #            ]
  
  race_levels <- c(str_subset(unique(df$race_ethnicity_grp), "^American.*"),
                   str_subset(unique(df$race_ethnicity_grp), "^Asian.*"),
                   str_subset(unique(df$race_ethnicity_grp), "^Black.*"),
                   str_subset(unique(df$race_ethnicity_grp), "^Pacific.*"),
                   str_subset(unique(df$race_ethnicity_grp), "^Other.*"),
                   'People of Color',
                   str_subset(unique(df$race_ethnicity_grp), "^Hispanic.*"),
                   str_subset(unique(df$race_ethnicity_grp), "^White.*"),
                   'All Races')
  
  df[, race_ethnicity_grp := factor(race_ethnicity_grp, levels = race_levels)]
  
  if(juris == 'region') {
    # aggregate counties to region
    
    df <- df[, .(estimate = sum(estimate)), by = c('variable_name', 'sort', 'chas_year', 'income_grp', 'race_ethnicity_grp', 'tenure')
    ][, geography_name := 'Region'] 
  }
  
  df_sum <- df[, .(estimate = sum(estimate)), by = c('chas_year', 'geography_name', 'tenure', 'income_grp', 'race_ethnicity_grp')]

  # df_cast <- dcast.data.table(df_sum, chas_year + geography_name + tenure + income_grp ~ race_ethnicity_grp, value.var = 'estimate')
  # openxlsx::write.xlsx(df_cast, 'test-tract-agg-bellevue.xlsx' )
  
  # sum each race/ethnicity/POC
  tot_resp_race <- df_sum[income_grp != 'All' & race_ethnicity_grp != 'All Races', .(estimate = sum(estimate), income_grp = 'All'), 
                  by = c('chas_year', 'geography_name', 'tenure', 'race_ethnicity_grp')]
  
  # sum POC (w/income group)
  poc <- df_sum[!race_ethnicity_grp %in% c('All Races', 'White', str_subset(race_ethnicity_grp, "^H.*")), .(estimate = sum(estimate), race_ethnicity_grp = 'People of Color'), 
         by = c('chas_year', 'geography_name', 'tenure', 'income_grp')]
  
  tot_poc <- poc[, .(estimate = sum(estimate), income_grp = 'All'), by = c('chas_year', 'geography_name', 'tenure', 'race_ethnicity_grp') ]
  
  # combine
  df_all <- rbindlist(list(df_sum, tot_resp_race, poc, tot_poc), use.names=TRUE, fill = TRUE)
  
  # incorporate race/ethnicity, poc, and all income totals for denominator column
  denom <- rbindlist(list(tot_resp_race, tot_poc), use.names=TRUE)
  setnames(denom, 'estimate', 'denom')
  denom[, income_grp:= NULL]
  
  all <- df_all[income_grp == 'All' & race_ethnicity_grp == 'All Races', ][, income_grp := NULL]
  setnames(all, 'estimate', 'denom')
  denom <- rbindlist(list(denom, all))
  
  df_join <- merge(df_all, denom, by =  c('chas_year', 'geography_name', 'tenure', 'race_ethnicity_grp'), all.x = TRUE)
  
  # create shares
  df_join[, share := estimate/denom]
  df_join[is.na(share), share := 0]
  
  # pivot wider
  df_est <- dcast.data.table(df_join, chas_year + geography_name + tenure + income_grp ~ race_ethnicity_grp, value.var = 'estimate')
  # openxlsx::write.xlsx(df_est, 'test-tract-agg-bellevue-sums.xlsx' )
  df_shr <- dcast.data.table(df_join, chas_year + geography_name + tenure + income_grp ~ race_ethnicity_grp, value.var = 'share')
  
  return(list(e = df_est, s = df_shr))
}

test <- create_income_table(juris = 'region')