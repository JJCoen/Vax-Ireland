# Scripts to process numbers encoded as characters
library(stringr)
library(purrr)

get_num <- function(s){
    # Replace missing values with "0.00%"
    if(s == "-") s <- "0.00%" 
    # one decimal
    if (str_length(s) == 5) as.numeric( str_sub(s, 1, 4) ) else
        # two decimals
        if (str_length(s) == 6) as.numeric( str_sub(s, 1, 5) ) else
            # 100.0%
            if (str_length(s) == 7) as.numeric(100)
}
get_num_list <- function(num_list) map_dbl(num_list, get_num)

# remove_missing <- function(str) {
#     if(str == "-") "0.00%" else str
# }
# remove_missing_list <- function(str_list){
#     map_chr(as.character(str_list), remove_missing)
# }
