source('function-query-sqlite-chas.R')

create_income_table_tract <- function() {
  
  # gather T1 table
  chas_table <- c('T1')
  dfs <- gather_tables(juris = 'tract', chas_table)
  
  # Assemble Table ----
  desc <- c('Extremely Low-Income (≤30% AMI)',
            'Very Low-Income (30-50%)',
            'Low-Income (50-80%)',
            'Moderate Income (80-100%)',
            'Above Median Income (>100%)')
  
  cols <- c('variable_name', 'sort','chas_year', 'tract_geoid', 'estimate', 'moe',  'tenure',
            'household_income', 'race_ethnicity', 'income_desc', 'race_ethnicity_desc')
  
  dfs$T1[, income_desc := fcase(household_income == 'All', 'All',
                             household_income == 'less than or equal to 30% of HAMFI', 'Less than 80% AMI',
                             household_income == 'greater than 30% but less than or equal to 50% of HAMFI', 'Less than 80% AMI',
                             household_income == 'greater than 50% but less than or equal to 80% of HAMFI', 'Less than 80% AMI',
                             household_income == 'greater than 80% but less than or equal to 100% of HAMFI', 'Greater than 80% AMI',
                             household_income == 'greater than 100% of HAMFI', 'Greater than 80% AMI')]
  
  dfs$T1[, race_ethnicity_desc := fcase(grepl("^American Indian ", race_ethnicity), "People of Color",
                                        grepl("^Asian ", race_ethnicity), "People of Color",
                                        grepl("^Black ", race_ethnicity), "People of Color",
                                        grepl("^Hispanic, any race", race_ethnicity), "Hispanic or Latino (of any race)",
                                        grepl("^Pacific ", race_ethnicity), "People of Color",
                                        grepl("^White ", race_ethnicity), "White",
                                        grepl("^All", race_ethnicity), "All Races")]
  
  # exclude high level totals
  df <- dfs$T1[!(sort %in% c(1, 2, 75)),]
  
  df <- df[, ..cols]
  
  df_sum <- df[, . (estimate = sum(estimate)), by = c('chas_year', 'tract_geoid', 'tenure', 'income_desc', 'race_ethnicity_desc')]
  
 

  # create shares
  denom <- df_sum[income_desc != 'All', .(denom = sum(estimate)), by = c('chas_year', 'tract_geoid', 'tenure', 'race_ethnicity_desc')
                      ]
  
  df_join <- merge(df_sum, denom, by = c('chas_year', 'tract_geoid', 'tenure', 'race_ethnicity_desc'))
  
  # test wide ----
  # denom <- df_sum[, .(denom = sum(estimate)), by = c('chas_year', 'tract_geoid', 'tenure', 'race_ethnicity_desc')]
  # df_all <- rbindlist(list(df_sum, tot_by_re), use.names=TRUE)

  # df_cast <- dcast.data.table(df_all, chas_year + tract_geoid + tenure + income_desc ~ race_ethnicity_desc, value.var = 'estimate')
  # openxlsx::write.xlsx(df_cast[tract_geoid == '53033000100'], 'test-tract-agg.xlsx' )
}

x <- create_income_table_tract()