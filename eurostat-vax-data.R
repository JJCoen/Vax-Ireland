# Load the data.table package (if not already loaded)
library(data.table)
library(readr)
library(janitor)

# Get the number of rows (excluding header)
num_rows <- length(count.fields("./raw-data/vax-data.csv", skip = 1))

num_cols <- count.fields("./raw-data/vax-data.csv", skip = 1)

vax <- read_csv("./raw-data/vax-data.csv", n_max = 1000 ) |> 
    setDT()
head(vax)
unique(vax$TargetGroup)

vax[, unique(ReportingCountry)]

vax_irl <- read_csv_chunked('./raw-data/vax-data.csv', 
                    callback = DataFrameCallback$new(function(x, pos) 
                        subset(x, ReportingCountry == "IE"))) |> 
    clean_names() |> 
    setDT()
tail(vax_irl[, .(YearWeekISO, TargetGroup, FirstDose)], 20)

vax_cso <- read_csv("./raw-data/CDC47202403.csv" ) |> 
    clean_names() |> 
    setDT()
tail(vax_cso)

mid_year <- c("2021 June" ) #, "2022 June", "2023 June")

vax_rate_21 <- vax_cso[month == "2021 June" & age_group == "12 years and over", 
        mean(value)]             
vax_rate_22 <- vax_cso[month == "2022 June" & age_group == "12 years and over", 
                       mean(value)]             
vax_rate_23 <- vax_cso[month == "2023 June" & age_group == "12 years and over", 
                       mean(value)]             
