transform_pop <- function(persons_dt){
    #' Transform from 5-year age groups 
    #' to same age categories used in CSO mortality counts
    #'
    #' 
    #' @description This function converts 5 yr age categories 
    #' to 10 of the groups in mortality table VSAQ2:
    #' "0 to 4", "5 to 14", "15 to 24", "25 to 34", 
    #' "35 to 44", "45 to 54", "55 to 64", "65 to 74", 
    #' "75 and Over"
    #' and sums the number of persons according to these age categories
    #' Counts may be population numbers or number of deaths
    #' 
    #' @param persons_dt data table. First column: "age_cat" 
    #' identifies the age categories in source data   
    #' In most cases, there are 17 five-year age categories, 
    #' concluding with the "80+" group 
    #' Second column: "count" 
    #' gives the number of persons in each age group.
    #' @usage transform_pop(persons_dt)
    #' @return A data table containing the 14 CSO age 
    #' categories and the number of persons in each group.
    
    # Record CSO age categories starting with younger groups
    age_tx <- c("0-4", "5-14", "15-24", "25-34")
    # Older Age categories 
    age_tx <- c(age_tx, "35-44", "45-54", "55-64", "65-74", 
                "75+", "Total")
    
    # Count of persons in CSO age categories 
    # 0-4 
    counts_tx <- c(persons_dt[1, count])
    # "5-14"
    counts_tx <- c(counts_tx, persons_dt[2:3, sum(count)])
    # 15-24
    counts_tx <- c(counts_tx, persons_dt[4:5, sum(count)])
    # 25-34
    counts_tx <- c(counts_tx, persons_dt[6:7, sum(count)])
    # 35-44
    counts_tx <- c(counts_tx, persons_dt[8:9, sum(count)])
    # 45-54 
    counts_tx <- c(counts_tx, persons_dt[10:11, sum(count)])
    # 55-64
    counts_tx <- c(counts_tx, persons_dt[12:13, sum(count)])
    # 65-74
    counts_tx <- c(counts_tx, persons_dt[14:15, sum(count)])
    # 75+
    counts_tx <- c(counts_tx, persons_dt[16:17, sum(count)])
    # Total
    counts_tx <- c(counts_tx, persons_dt[1:17, sum(count)])
        
    # Construct new data table for CSO categories
    persons_tx <- data.table(age_cat = age_tx,
                             count = counts_tx)
    return(persons_tx)
}