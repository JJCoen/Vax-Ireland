get_mort_EStat <- function(EStat){ 
    #' Extracts mortality counts for selected age categories.
    #'
    #' @description This function inputs mortality data for 2010 to 2021 
    #' from the EuroStat website.  For compatibility with CSO dataset,
    #' it extracts the following age categories:
    #' "0 to 4", "5 to 14", "15 to 24", "25 to 34", 
    #' "35 to 44", "45 to 54", "55 to 64", "65 to 74", 
    #' "75 and Over"
    #'   
    #' 
    #' @param EStat data table. This data is stored in a "wide" format 
    #' with years (2020-2021) spread across the first row and 
    #' ages from 0 to 99, over 99, and total count on the second row.
    #' For each year, there are 102 entries.
    #' 
    #' @usage get_mort_EStat(EStat)
    #' @return EStat_cat data table: 
    #'     year: ranging from 2010 to 2021
    #'     count: number of deaths in each age category, sorted by year
    #'     age_cat: Ten CSO age categories, incl Total
    
    # remove "Unknown" entries
    EStat_x <- EStat[, -c(103, 2*103, 3*103, 4*103, 5*103, 6*103, 7*103,
                          8*103, 9*103, 10*103, 11*103, 12*103)]
    
    # Transpose EStat_x from wide to long format
    EStat_t <- transpose(EStat_x) |> 
        as.data.table()
    EStat_t[, ":=" (age = V1, count = V2)][, c("V1", "V2") := NULL]
    # remove(EStat_x, EStat)
    
    # Convert count column from character to numeric
    EStat_t <- EStat_t[, count := as.numeric(count)]
    
    # Need to include column for year
    year_range <- c(rep(2010, 102), rep(2011, 102),rep(2012, 102),
      rep(2013, 102),rep(2014, 102),rep(2015, 102),rep(2016, 102),
      rep(2017, 102),rep(2018, 102),rep(2019, 102),rep(2020, 102),
      rep(2021, 102))
    EStat_t[, year := year_range]
    EStat_t[, age := unlist(age)]

    # Record CSO age categories starting with younger groups
    age_tx <- c("Total", "0-4", "5-14", "15-24", "25-34")
    # Older Age categories 
    age_tx <- c(age_tx, "35-44", "45-54", "55-64", "65-74", 
                "75+")
    # Create vector with 10 categories for each of the 12 years
    # from 2010 to 2021
    age_gr <- rep(age_tx, 12)
    
    # Create vector for years 2010 to 2021 with repetition
    # for the 10 age categories
    year_mx <- replicate(c(10), rep(2010:2021))
    year_vec <- as.vector(t(year_mx))
    
    # Collate counts for single year age into age categories
    counts_gr <- collate_by_year(EStat_t[year == 2010, count])
    counts_gr <- c(counts_gr, 
                   collate_by_year(EStat_t[year == 2011, count]) )
    counts_gr <- c(counts_gr, 
                   collate_by_year(EStat_t[year == 2012, count]) )
    counts_gr <- c(counts_gr, 
                   collate_by_year(EStat_t[year == 2013, count]) )
    counts_gr <- c(counts_gr, 
                   collate_by_year(EStat_t[year == 2014, count]) )
    counts_gr <- c(counts_gr, 
                   collate_by_year(EStat_t[year == 2015, count]) )
    counts_gr <- c(counts_gr, 
                   collate_by_year(EStat_t[year == 2016, count]) )
    counts_gr <- c(counts_gr, 
                   collate_by_year(EStat_t[year == 2017, count]) )
    counts_gr <- c(counts_gr, 
                   collate_by_year(EStat_t[year == 2018, count]) )
    counts_gr <- c(counts_gr, 
                   collate_by_year(EStat_t[year == 2019, count]) )
    counts_gr <- c(counts_gr, 
                   collate_by_year(EStat_t[year == 2020, count]) )
    counts_gr <- c(counts_gr, 
                   collate_by_year(EStat_t[year == 2021, count]) )
    
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
    # Construct new data table for CSO categories
    EStat_cat <- data.table(year = year_vec,
        age_cat = age_gr,
        count = counts_gr)
    
    return(EStat_cat)
}

collate_by_year <- function(counts_yr){
    #' Collate mortality counts for each year of age into
    #' age categories in use by the CSO
    #' 
    #' @param counts_yr numerical vector containing counts for 
    #' each year of age and a given calendar year.
    #' 
    #' @return mortality counts aggregated to age category

    # Count of persons in CSO age categories 
    # Total
    counts_gr <- counts_yr[1]
    # 0-4 
    counts_gr <- c(counts_gr, sum( counts_yr[2:6] ) )
    # "5-14"
    counts_gr <- c(counts_gr, sum( counts_yr[7:16] ))
    # 15-24
    counts_gr <- c(counts_gr, sum( counts_yr[17:26] ))
    # 25-34
    counts_gr <- c(counts_gr, sum( counts_yr[27:36] ))
    # 35-44
    counts_gr <- c(counts_gr, sum( counts_yr[37:46] ))
    # 45-54 
    counts_gr <- c(counts_gr, sum( counts_yr[47:56] ))
    # 55-64
    counts_gr <- c(counts_gr, sum( counts_yr[57:66] ))
    # 65-74
    counts_gr <- c(counts_gr, sum( counts_yr[67:76] ))
    # 75+
    counts_gr <- c(counts_gr, sum( counts_yr[77:102] ))
    return(counts_gr)
}
