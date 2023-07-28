# Function to assemble Income table

# source('config.R')
source('function-query-sqlite-chas.R')
# library(dplyr)
# library(tidyverse)
# library(data.table)

create_income_table <- function() {
  
  # gather T1 table
  chas_table <- c('T1')
  dfs <- gather_tables(juris = 'tract',chas_table)
  
  # Assemble Table ----
  desc <- c('Extremely Low-Income (≤30% AMI)',
            'Very Low-Income (30-50%)',
            'Low-Income (50-80%)',
            'Moderate Income (80-100%)',
            'Above Median Income (>100%)')
  
  cols <- c('variable_name', 'sort','chas_year', 'tract_geoid', 'estimate', 'moe',  'col_desc', 'race_ethnicity')
  # cols <- c('variable_name', 'sort','chas_year', 'geography_name', 'estimate', 'moe',  'col_desc', 'race_ethnicity')

  # Table 1 - select fields based on data vintage
  ifelse(dfs$T1$chas_year <= '2014',
         
         { t1_head_30 <- c(134,130,131,133,132,129,143,175,171,172,174,173,170,184,216,212,213,215,214,211,225,10,6,7,9,8,5,11,51,47,48,50,49,46,52,92,88,89,91,90,87,93)
           t1_head_30_50 <- c(142,138,139,141,140,137,151,183,179,180,182,181,178,192,224,220,221,223,222,219,233,18,14,15,17,16,13,19,59,55,56,58,57,54,60,100,96,97,99,98,95,101)
           t1_head_50_80 <- c(150,146,147,149,148,145,159,191,187,188,190,189,186,200,232,228,229,231,230,227,241,26,22,23,25,24,21,27,67,63,64,66,65,62,68,108,104,105,107,106,103,109)
           t1_head_80_100 <- c(158,154,155,157,156,153,159,199,195,196,198,197,194,200,240,236,237,239,238,235,241,34,30,31,33,32,29,35,75,71,72,74,73,70,76,116,112,113,115,114,111,117)
           t1_head_100 <- c(166,162,163,165,164,161,167,207,203,204,206,205,202,208,248,244,245,247,246,243,249,42,38,39,41,40,37,43,83,79,80,82,81,78,84,124,120,121,123,122,119,125)
         
         },
         
         { t1_head_30 <- c(10,6,7,9,8,5,46,42,43,45,44,41,83,79,80,82,81,78,119,115,116,118,117,114)
           t1_head_30_50 <- c(17,13,14,16,15,12,53,49,50,52,51,48,90,86,87,89,88,85,126,122,123,125,124,121)
           t1_head_50_80 <- c(24,20,21,23,22,19,60,56,57,59,58,55,97,93,94,96,95,92,133,129,130,132,131,128)
           t1_head_80_100 <- c(31,27,28,30,29,26,67,63,64,66,65,62,104,100,101,103,102,99,140,136,137,139,138,135)
           t1_head_100 <- c(38,34,35,37,36,33,74,70,71,73,72,69,111,107,108,110,109,106,147,143,144,146,145,142)
         
          })
  
  t1_30 <- dfs$T1[sort %in% t1_head_30, ]
  t1_30 <- t1_30[, `:=`(sort = factor(sort, levels = t1_head_30), col_desc = 'Extremely Low-Income (≤30% AMI)')][order(sort)]
  
  t1_30_50 <- dfs$T1[sort %in% t1_head_30_50, ]
  t1_30_50 <- t1_30_50[, `:=`(sort = factor(sort, levels = t1_head_30_50), col_desc = 'Very Low-Income (30-50%)')][order(sort)]
  
  t1_50_80 <- dfs$T1[sort %in% t1_head_50_80, ]
  t1_50_80 <- t1_50_80[, `:=`(sort = factor(sort, levels = t1_head_50_80), col_desc = 'Low-Income (50-80%)')][order(sort)]
  
  t1_80_100 <- dfs$T1[sort %in% t1_head_80_100, ]
  t1_80_100 <- t1_80_100[, `:=`(sort = factor(sort, levels = t1_head_80_100), col_desc = 'Moderate Income (80-100%)')][order(sort)]
  
  t1_100 <- dfs$T1[sort %in% t1_head_100, ]
  t1_100 <- t1_100[, `:=`(sort = factor(sort, levels = t1_head_100), col_desc = 'Above Median Income (>100%)')][order(sort)]
  
  ## Format Table ----
  
  # join all AMI tables into one
  all_dfs <-map(list(t1_30, t1_30_50, t1_50_80, t1_80_100, t1_100), ~.x[, ..cols]) #### geography_name not found
  df <- rbindlist(all_dfs)
  
  #df <- df %>% filter(df$geography_name == "Bellevue")
  
  df_sum <- df[, . (estimate = sum(estimate)), by = c('chas_year', 'tract_geoid', 'description')
  ][,  grouping := fcase(description == 'All', 'All',
                         description == 'Extremely Low Income (<30% AMI)','Less than 80% AMI',
                         description == 'Very Low Income (30-50% AMI)', 'Less than 80% AMI',
                         description == 'Low Income (50-80% AMI)', 'Less than 80% AMI',
                         description == 'Moderate Income (80-100% AMI)', 'Greater than 80% AMI')]
  
  df <- df %>%
    group_by(grouping, race_ethnicity) %>%
    summarize(geography_name = first(geography_name),
              estimate = sum(estimate),
              chas_year = first(chas_year),
              race_ethnicity = first(race_ethnicity))
  
  # format Race/Ethnicity data
  df$description <- df$col_desc
  df <- df %>% 
    mutate(race_ethnicity=factor(case_when(grepl("^American Indian ", race_ethnicity) ~"American Indian or Alaskan Native",
                                        grepl("^Asian ", race_ethnicity) ~"Asian",
                                        grepl("^Black ", race_ethnicity) ~"Black or African American",
                                        grepl("^Hispanic, any race", race_ethnicity) ~"Hispanic or Latino (of any race)",
                                        grepl("^Pacific ", race_ethnicity) ~"Pacific Islander",
                                        grepl("^White ", race_ethnicity) ~"White",
                                        grepl("^All", race_ethnicity) ~"All",
                                        !is.na(race_ethnicity) ~ "")))
  
  # pivot wider
  setDT(df)
  df <- dcast.data.table(df, chas_year + geography_name + description ~ race_ethnicity, value.var = 'estimate')
  
  # reorder rows
  df <- df[, description := factor(description, levels = desc)][order(description)]
  
  # calculate column total  ----
  df_ra <- df[, Total := `American Indian or Alaskan Native` + `Asian` + `Black or African American` + `Hispanic or Latino (of any race)` +  `Pacific Islander` + `White`]

  # calculate row totals/shares
  df_ra <- df_ra %>%
    bind_rows(summarise(., across(c(`American Indian or Alaskan Native`,`Asian`,`Black or African American`,`Hispanic or Latino (of any race)`,`Pacific Islander`,`White`,`Total`), sum),
                        across(description, ~'Total')))
  df_ra[, `:=` (`American Indian or Alaskan Native_share` = `American Indian or Alaskan Native`/Total, 
                Asian_share = Asian/Total,
                `Black or African American_share` = `Black or African American`/Total,
                `Hispanic or Latino (of any race)_share` = `Hispanic or Latino (of any race)`/Total,
                `Pacific Islander_share` = `Pacific Islander`/Total,
                White_share = White/Total)]
}

x <- create_income_table()