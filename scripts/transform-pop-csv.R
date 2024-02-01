transform_pop_csv <- function(persons_dt){
    #' Transform from 5-year age groups 
    #' to same age categories used in CSO mortality counts
    #'
    #' @description This function converts 27 age categories in
    #' source data
    #' to the 10 groups in mortality table VSAQ2:
    #' "0 to 4", "5 to 14", "15 to 24", "25 to 34", 
    #' "35 to 44", "45 to 54", "55 to 64", "65 to 74", 
    #' "75 and Over", "All ages"
    #' and sums the number of persons according to these age categories
    #' Counts may be population numbers or number of deaths
    #' 
    #' @param persons_dt data table. First column: "age_group" 
    #' identifies the age categories in source data   
    #' There are 27 age groups, concluding with "80 plus" and  
    #' "All ages"
    #' Second column: "count" 
    #' gives the number of persons in each age group.
    #' @usage transform_pop_10_14(persons_dt)
    #' @return A data table containing the 10 CSO age 
    #' categories and the number of persons in each group.
    
    # Record CSO age categories starting with younger groups
    age_tx <- c("0-4", "5-14", "15-24", "25-34")
    # Older Age categories 
    age_tx <- c(age_tx, "35-44", "45-54", "55-64", "65-74", 
                "75+", "Total")
    
    # Count of persons in CSO age categories 
    # 0-4 
    counts_tx <- c(persons_dt[2, count])
    # "5-14"
    counts_tx <- c(counts_tx, persons_dt[5:6, sum(count)])
    # 15-24
    counts_tx <- c(counts_tx, persons_dt[8, count])
    # 25-34
    counts_tx <- c(counts_tx, persons_dt[c(11,13), sum(count)])
    # 35-44
    counts_tx <- c(counts_tx, persons_dt[14:15, sum(count)])
    # 45-54 
    counts_tx <- c(counts_tx, persons_dt[c(16,18), sum(count)])
    # 55-64
    counts_tx <- c(counts_tx, persons_dt[19:20, sum(count)])
    # 65-74
    counts_tx <- c(counts_tx, persons_dt[c(21,23), sum(count)])
    # 75+
    counts_tx <- c(counts_tx, persons_dt[24:26, sum(count)])
    # Total
    counts_tx <- c(counts_tx, persons_dt[27, count])
        
    # Construct new data table for CSO categories
    persons_tx <- data.table(age_cat = age_tx,
                             count = counts_tx)
    return(persons_tx)
}