# tract-based script to consolidate information to display on leaflet map for Rental Affordability metric

# library(tidyverse)
# library(data.table)
source('function-query-sqlite-chas.R')

create_rental_affordability_tract_table <- function() {
  # Generate tract level table for rental affordability metric. To be used in tract map.
  
  chas_tables <- c('T8', 'T15C', 'T14B')
  dfs <- gather_tables(juris = 'tract', chas_tables)
  
  # share of rental units < 80% AMI ----
  
  desc <- c('Extremely Low Income (<30% AMI)',
            'Very Low Income (30-50% AMI)',
            'Low Income (50-80% AMI)',
            'Moderate Income (80-100% AMI)',
            'Greater than 100% of AMI',
            'All')
  
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
  
  cols <- c('variable_name', 'sort','chas_year', 'tract_geoid', 'estimate', 'moe',  'col_desc', 'description')
  ra_dfs <- map(list(t15c, t14b), ~.x[, ..cols])
  df <- rbindlist(ra_dfs)
  
  df_sum <- df[, . (estimate = sum(estimate)), by = c('chas_year', 'tract_geoid', 'description')
  ][,  grouping := fcase(description == 'All', 'All',
                         description == 'Extremely Low Income (<30% AMI)','Less than 80% AMI',
                         description == 'Very Low Income (30-50% AMI)', 'Less than 80% AMI',
                         description == 'Low Income (50-80% AMI)', 'Less than 80% AMI',
                         description == 'Moderate Income (80-100% AMI)', 'Greater than 80% AMI')]
  
  df_sum <- df_sum[, .(estimate = sum(estimate)), by = c('chas_year', 'tract_geoid', 'grouping')]
  
  df_denom <- df_sum[grouping == 'All', .(tract_geoid, denom = estimate)]
  
  df_calc <- merge(df_sum, df_denom, by = 'tract_geoid')
  
  df_calc[, share := estimate/denom]
  
  d <- df_calc[grouping %in% str_subset(grouping, "Less.*"), .(chas_year, tract_geoid, grouping, share)]
}

test <- create_rental_affordability_tract_table()