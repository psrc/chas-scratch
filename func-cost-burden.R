# Function to assemble Cost Burden table

source('config.R')

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
  
  names(desc) <- c("No Cost Burden", "Cost-Burdened (30-50%)", "Severely Cost-Burdened (>50%)", "Not Calculated", "All")
  
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